{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
             OverlappingInstances, ScopedTypeVariables, UndecidableInstances #-}
-- |Synchronous OpenCL kernel execution that avoids copying input
-- 'Vector's when running the OpenCL kernel on the CPU.
module CLUtil.KernelArgsCL (KernelArgsCL, runKernel) where
import Control.Applicative
import Control.Monad (void, when)
import Data.Either (partitionEithers)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
import Foreign.Ptr (nullPtr, Ptr)
import Foreign.Storable (Storable(..))
import CLUtil.CL
import CLUtil.KernelArgTypes 
import CLUtil.State
import CLUtil.VectorBuffers
import Control.Parallel.OpenCL

-- NOTE: This is adapted from KernelArgsCPS. The only change is to
-- push the use of the 'OpenCLState' value to final kernel execution
-- so that this kernel-running interface fits in better with use of
-- the 'CL' monad.

-- In this variation, reading an output is an action that maps the
-- OpenCL buffer, and provides a ForeignPtr and the number of elements
-- in the output vector.
data PostExec = ReadOutput (IO (IO (ForeignPtr ()), Int))
              | FreeInput (IO ())

postToEither :: PostExec -> Either (IO (IO (ForeignPtr ()), Int)) (IO ())
postToEither (ReadOutput r) = Left r
postToEither (FreeInput m) = Right m

partitionPost :: [PostExec] -> ([IO (IO (ForeignPtr ()), Int)], [IO ()])
partitionPost = partitionEithers . map postToEither

-- We want to write something like this when dealing with a Vector
-- argument...
-- \cont ->
-- V.unsafeWith v $ \ptr -> do
--   b <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_USE_HOST_PTR]
--                       (sz, castPtr ptr)
--   clSetKernelArg k arg b
--   cont (FreeInput (void (clReleaseMemObject b)):)
-- where sz = V.length v * sizeOf (undefined::a)


-- The continuation of a buffer preperation step is a function takes
-- an 'OpenCLState' and a list of element sizes, and returns an action
-- to perform after kernel execution and the remaining element
-- sizes. This arrangement is because we only know the types of the
-- output vectors in the top-level runner instances (e.g. IO (Vector
-- a)), not when we see an 'OutputSize' kernel argument.
type PrepCont = (OpenCLState -> [Int] -> IO (Maybe PostExec, [Int]))
              -> IO [PostExec]

-- To prepare an argument, we take a continuation, perform some IO (to
-- allocate memory, or just grab a pointer), then run the
-- continuation.
type PrepExec = PrepCont -> IO [PostExec]

-- Wrap an output buffer in a 'Vector'.
mkRead :: Storable a => (IO (ForeignPtr ()), Int) -> IO (Vector a)
mkRead (getPtr,num) = flip V.unsafeFromForeignPtr0 num . castForeignPtr
                      <$> getPtr

-- | Implementation of a variable arity technique similar to
-- "Text.Printf".
class KernelArgsCL a where
  -- Setting an argument requires a state, a kernel, the position of
  -- the argument, the number of work items specified so far, and a
  -- list of actions that prepare argument buffers and return the
  -- cleanup action to take when the kernel is finished.
  setArgCL :: CLKernel -> CLuint -> Maybe NumWorkItems -> 
              Maybe WorkGroup -> [PrepExec] -> a

-- Nest a stack of buffer preparation actions. This lets us safely use
-- the pointers underlying 'Vector' arguments without an extra copy.
nestM :: OpenCLState -> [Int] -> IO () -> [PrepExec] -> IO [PostExec]
nestM s outputSizes finish = go [] outputSizes
  where go acc _ [] = finish >> return acc
        go acc sizes (m:ms) = m (\f -> do (cleanup, sizes') <- f s sizes
                                          go (maybe acc (:acc) cleanup)
                                             sizes'
                                             ms)

runCPS :: [Int] -> OpenCLState -> CLKernel -> NumWorkItems -> Maybe WorkGroup ->
          [PrepExec] -> 
          IO ([IO (IO (ForeignPtr ()), Int)], [IO ()])
runCPS outputSizes s k n wg prep =
  partitionPost <$> nestM s outputSizes runK prep
  where runK = do ev <- clEnqueueNDRangeKernel (clQueue s) 
                                               k 
                                               (workItemsList n)
                                               (maybe [] workGroupSizes wg)
                                               []
                  _ <- clWaitForEvents [ev]
                  void $ clReleaseEvent ev

-- Synchronous execution of a kernel with no automatic outputs. This
-- is useful for kernels that modify user-managed buffers.
instance CL' m => KernelArgsCL (m ()) where
  setArgCL k _ (Just n) wg prep = ask >>= \s ->
    liftIO $ do
      (o,cleanup) <- runCPS [] s k n wg prep
      when (not (null o)) (error "Outputs aren't bound!")
      sequence_ cleanup
  setArgCL _ _ _ _ _ = error "The number of work items is missing!"

-- Execute a kernel where the calling context is expecting a single
-- 'Vector' return value.
instance forall a m. (Storable a, CL' m) => KernelArgsCL (m (Vector a)) where
  setArgCL k _ (Just n) wg prep = ask >>= \s ->
    liftIO $ do
      (o,cleanup) <- runCPS [sizeOf (undefined::a)] s k n wg prep
      r1 <- case o of
             [] -> error "One output bound, none specified"
             [f] -> mkRead <$> f
             _ -> error "More outputs specified than bound"
      sequence_ cleanup
      r1
  setArgCL _ _ _ _ _ = error "The number of work items is missing!"

-- Execute a kernel where the calling context is expecting two
-- 'Vector' return values.
instance forall a b m. (Storable a, Storable b, CL' m) => 
  KernelArgsCL (m (Vector a, Vector b)) where
  setArgCL k _ (Just n) wg prep = ask >>= \s ->
    liftIO $ do
      (o, cleanup) <- runCPS [sizeOf (undefined::a), sizeOf (undefined::b)]
                             s k n wg prep
      (r1,r2) <- case o of
                   [] -> error "Two output bound, none specified"
                   [_] -> error "Two outputs bound, one specified"
                   [f,g] -> (,) <$> (mkRead <$> f) <*> (mkRead <$> g)
                   _ -> error "More outputs specified than bound"
      sequence_ cleanup
      (,) <$> r1 <*> r2
  setArgCL _ _ _ _ _ = error "The number of work items is missing!"

-- Execute a kernel where the calling context is expecting three
-- 'Vector' return values.
instance forall a b c m. (Storable a, Storable b, Storable c, CL' m) => 
  KernelArgsCL (m (Vector a, Vector b, Vector c)) where
  setArgCL k _ (Just n) wg prep = ask >>= \s -> 
    liftIO $ do
      (o, cleanup) <- runCPS [ sizeOf (undefined::a)
                             , sizeOf (undefined::b)
                             , sizeOf (undefined::c) ]
                             s k n wg prep
      (r1,r2,r3) <- case o of
                      [f,g,h] -> (,,) <$> (mkRead <$> f) 
                                      <*> (mkRead <$> g) 
                                      <*> (mkRead <$> h)
                      _ -> error "Different number of outputs specified than bound"
      sequence_ cleanup
      (,,) <$> r1 <*> r2 <*> r3
  setArgCL _ _ _ _ _ = error "The number of work items is missing!"

-- Pass an arbitrary 'Storable' as a kernel argument.
instance (Storable a, KernelArgsCL r) => KernelArgsCL (a -> r) where
  setArgCL k arg n wg prep = \a -> 
    let load cont = clSetKernelArgSto k arg a >>
                    cont (\_ sz -> return (Nothing,sz))
    in setArgCL k (arg+1) n wg (load : prep)

-- Handle 'Vector' input arguments.
instance (Storable a, KernelArgsCL r) => KernelArgsCL (Vector a -> r) where
  setArgCL k arg n wg prep = 
    \v -> let load cont = cont $ \s sz -> 
                withVectorBuffer s v $ \b ->
                  let clean = FreeInput . void $ clReleaseMemObject b
                  in do clSetKernelArgSto k arg b
                        return (Just clean, sz)
          in setArgCL k (arg+1) n wg (load:prep)

instance (Storable a, KernelArgsCL r) => KernelArgsCL (LocalMem a -> r) where
  setArgCL k arg n wg prep =
    \(Local m) -> let sz = m * sizeOf (undefined::a)
                      local cont = do clSetKernelArg k arg sz nullPtr
                                      cont (\_ szs -> return (Nothing, szs))
                  in setArgCL k (arg+1) n wg (local:prep)

-- Keep track of an argument that specifies the number of work items
-- to execute.
instance KernelArgsCL r => KernelArgsCL (NumWorkItems -> r) where
  setArgCL k arg _ wg prep = \n -> setArgCL k arg (Just n) wg prep

-- Keep track of an argument that specifies the local work group size.
instance KernelArgsCL r => KernelArgsCL (WorkGroup -> r) where
  setArgCL k arg n _ prep = \wg -> setArgCL k arg n (Just wg) prep

-- A finalizer we attach to the 'ForeignPtr' wrapping a mapped OpenCL
-- memory buffer. This lets us wrap the mapped buffer in a 'Vector',
-- then release it when the 'Vector' is GC'ed.
bufferFinalizer :: CLCommandQueue -> CLMem -> Ptr () -> IO ()
bufferFinalizer q b p = do clEnqueueUnmapMemObject q b p [] >>=
                             -- clWaitForEvents . (:[])
                             void . clReleaseEvent
                           void $ clReleaseMemObject b

-- Handle 'Vector' outputs by automatically managing the underlying
-- OpenCL buffers.
instance KernelArgsCL r => KernelArgsCL (OutputSize -> r) where
  setArgCL k arg n wg prep = 
    \(Out m) ->
      let load cont = cont allocateOutput
          allocateOutput _ [] = error "Couldn't determine output element size"
          allocateOutput s (sz:szs) =
            do b <- clCreateBuffer (clContext s)
                                   [ CL_MEM_WRITE_ONLY
                                   , CL_MEM_ALLOC_HOST_PTR ]
                                   (m*sz, nullPtr)
               clSetKernelArgSto k arg b
               -- The goal is to map the output buffer, and wrap that
               -- pointer in a Vector. This would mean that the Vector
               -- is wrapping memory allocated by OpenCL.
               let getPtr = do (ev,p) <- clEnqueueMapBuffer (clQueue s) b 
                                                            True [CL_MAP_READ]
                                                            0 (m*sz) [] 
                               _ <- clWaitForEvents [ev]
                               void $ clReleaseEvent ev
                               newForeignPtr p $ bufferFinalizer (clQueue s) b p
                   finishOutput = return (getPtr,m)
               return (Just $ ReadOutput finishOutput, szs) 
      in setArgCL k (arg+1) n wg (load:prep)

-- |Simple interface for calling an OpenCL kernel. Supports input
-- 'Vector' and 'Storable' arguments, and produces 'Vector' or unit
-- outputs. Uses the actual pointers underlying any vector arguments,
-- improving performance of kernels run on the CPU.
-- 
-- > (v1,v2) <- runKernel kernel vIn (Work1D 4) (Out 4) (Out 4)
--
-- Note that a 'Vector' returned by using an output parameter
-- (configured with an 'OutputSize' value) wraps memory allocated by
-- OpenCL. If you want to release your OpenCL context, but hold on to
-- such a 'Vector', copy its contents with 'vectorDup'.
runKernel :: KernelArgsCL a => CLKernel -> a
runKernel k = setArgCL k 0 Nothing Nothing []
