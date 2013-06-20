{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, OverlappingInstances,
             ForeignFunctionInterface #-}
-- |Synchronous OpenCL kernel execution that avoids copying input
-- 'Vector's when running the OpenCL kernel on the CPU.
module Control.Parallel.CLUtil.KernelArgsCPS (KernelArgsCPS, runKernelCPS) where
import Control.Applicative
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (partitionEithers)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
import Foreign.Ptr (nullPtr, Ptr)
import Foreign.Storable (Storable(..))
import Control.Parallel.CLUtil.KernelArgTypes 
import Control.Parallel.CLUtil.State
import Control.Parallel.CLUtil.VectorBuffers
import Control.Parallel.OpenCL

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


-- The continuation of a buffer preperation step is a function takes a
-- list of element sizes, and returns an action to perform after
-- kernel execution and the remaining element sizes. This arrangement
-- is because we only know the types of the output vectors in the
-- top-level runner instances (e.g. IO (Vector a)), not when we see an
-- 'OutputSize' kernel argument.
type PrepCont = ([Int] -> IO (Maybe PostExec, [Int])) -> IO [PostExec]

-- To prepare an argument, we take a continuation, perform some IO (to
-- allocate memory, or just grab a pointer), then run the
-- continuation.
type PrepExec = PrepCont -> IO [PostExec]

-- Wrap an output buffer in a 'Vector'.
mkRead :: Storable a => (IO (ForeignPtr ()), Int) -> IO (Vector a)
mkRead (getPtr,num) =  flip V.unsafeFromForeignPtr0 num . castForeignPtr 
                   <$> getPtr

class KernelArgsCPS a where
  -- Setting an argument requires a state, a kernel, the position of
  -- the argument, the number of work items specified so far, and a
  -- list of actions that prepare argument buffers and return the
  -- cleanup action to take when the kernel is finished.
  setArgCPS :: OpenCLState -> CLKernel -> CLuint -> Maybe NumWorkItems -> 
               Maybe WorkGroup -> [PrepExec] -> a

-- Nest a stack of buffer preparation actions. This lets us safely use
-- the pointers underlying 'Vector' arguments without an extra copy.
nestM :: [Int] -> IO () -> [PrepExec] -> IO [PostExec]
nestM outputSizes finish = go [] outputSizes
  where go acc _ [] = finish >> return acc
        go acc sizes (m:ms) = m (\f -> do (cleanup, sizes') <- f sizes
                                          go (maybe acc (:acc) cleanup)
                                             sizes'
                                             ms)

runCPS :: [Int] -> OpenCLState -> CLKernel -> NumWorkItems -> Maybe WorkGroup ->
          [PrepExec] -> 
          IO ([IO (IO (ForeignPtr ()), Int)], [IO ()])
runCPS outputSizes s k n wg prep =
  partitionPost <$> nestM outputSizes runK prep
  where runK = do ev <- clEnqueueNDRangeKernel (clQueue s) 
                                               k 
                                               (workItemsList n)
                                               (maybe [] workGroupSizes wg)
                                               []
                  _ <- clWaitForEvents [ev]
                  void $ clReleaseEvent ev

-- Synchronous execution of a kernel with no automatic outputs. This
-- is useful for kernels that modify user-managed buffers.
instance MonadIO m => KernelArgsCPS (m ()) where
  setArgCPS s k _ (Just n) wg prep = liftIO $ do
    (o,cleanup) <- runCPS [] s k n wg prep
    when (not (null o)) (error "Outputs aren't bound!")
    sequence_ cleanup
  setArgCPS _ _ _ _ _ _ = error "The number of work items is missing!"

-- Execute a kernel where the calling context is expecting a single
-- 'Vector' return value.
instance forall a m. (Storable a, MonadIO m)
  => KernelArgsCPS (m (Vector a)) where
  setArgCPS s k _ (Just n) wg prep = liftIO $ do
    (o,cleanup) <- runCPS [sizeOf (undefined::a)] s k n wg prep
    r1 <- case o of
               [] -> error "One output bound, none specified"
               [f] -> mkRead <$> f
               _ -> error "More outputs specified than bound"
    sequence_ cleanup
    r1
  setArgCPS _ _ _ _ _ _ = error "The number of work items is missing!"

-- Execute a kernel where the calling context is expecting two
-- 'Vector' return values.
instance forall a b m. (Storable a, Storable b, MonadIO m) => 
  KernelArgsCPS (m (Vector a, Vector b)) where
  setArgCPS s k _ (Just n) wg prep = liftIO $ do
    (o, cleanup) <- runCPS [sizeOf (undefined::a), sizeOf (undefined::b)]
                           s k n wg prep
    (r1,r2) <- case o of
                 [] -> error "Two output bound, none specified"
                 [_] -> error "Two outputs bound, one specified"
                 [f,g] -> (,) <$> (mkRead <$> f) <*> (mkRead <$> g)
                 _ -> error "More outputs specified than bound"
    sequence_ cleanup
    (,) <$> r1 <*> r2
  setArgCPS _ _ _ _ _ _ = error "The number of work items is missing!"

-- Execute a kernel where the calling context is expecting three
-- 'Vector' return values.
instance forall a b c m. (Storable a, Storable b, Storable c, MonadIO m) => 
  KernelArgsCPS (m (Vector a, Vector b, Vector c)) where
  setArgCPS s k _ (Just n) wg prep = liftIO $ do
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
  setArgCPS _ _ _ _ _ _ = error "The number of work items is missing!"

-- Pass an arbitrary 'Storable' as a kernel argument.
instance (Storable a, KernelArgsCPS r) => KernelArgsCPS (a -> r) where
  setArgCPS s k arg n wg prep = \a -> 
    let load cont = clSetKernelArgSto k arg a >>
                    cont (\sz -> return (Nothing,sz))
    in setArgCPS s k (arg+1) n wg (load : prep)

-- Handle 'Vector' input arguments.
instance (Storable a, KernelArgsCPS r) => KernelArgsCPS (Vector a -> r) where
  setArgCPS s k arg n wg prep = 
    \v -> let load cont = withVectorBuffer s v $
                          \b -> let clean = FreeInput . void $
                                            clReleaseMemObject b
                                in do clSetKernelArgSto k arg b
                                      cont (\sz -> return (Just clean, sz))
          in setArgCPS s k (arg+1) n wg (load:prep)

instance (Storable a, KernelArgsCPS r) => KernelArgsCPS (LocalMem a -> r) where
  setArgCPS s k arg n wg prep =
    \(Local m) -> let sz = m * sizeOf (undefined::a)
                      local cont = do clSetKernelArg k arg sz nullPtr
                                      cont (\sz -> return (Nothing, sz))
                  in setArgCPS s k (arg+1) n wg (local:prep)

-- Keep track of an argument that specifies the number of work items
-- to execute.
instance KernelArgsCPS r => KernelArgsCPS (NumWorkItems -> r) where
  setArgCPS s k arg _ wg prep = \n -> setArgCPS s k arg (Just n) wg prep

-- Keep track of an argument that specifies the local work group size.
instance KernelArgsCPS r => KernelArgsCPS (WorkGroup -> r) where
  setArgCPS s k arg n _ prep = \wg -> setArgCPS s k arg n (Just wg) prep

-- A finalizer we attach to the 'ForeignPtr' wrapping a mapped OpenCL
-- memory buffer. This lets us wrap the mapped buffer in a 'Vector',
-- then release it when the 'Vector' is GC'ed.
bufferFinalizer :: CLCommandQueue -> CLMem -> Ptr () -> IO ()
bufferFinalizer q b p = do clEnqueueUnmapMemObject q b p [] >>=
                             -- clWaitForEvents . (:[])
                             clReleaseEvent
                           void $ clReleaseMemObject b

-- Handle 'Vector' outputs by automatically managing the underlying
-- OpenCL buffers.
instance KernelArgsCPS r => KernelArgsCPS (OutputSize -> r) where
  setArgCPS s k arg n wg prep = 
    \(Out m) -> 
      let load cont = cont allocateOutput
          allocateOutput [] = error "Couldn't determine output element size"
          allocateOutput (sz:szs) = 
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
                               clReleaseEvent ev
                               -- clWaitForEvents [ev]
                               newForeignPtr p $ bufferFinalizer (clQueue s) b p 
                   finishOutput = return (getPtr,m)
               return (Just $ ReadOutput finishOutput, szs) 
      in setArgCPS s k (arg+1) n wg (load:prep)

-- |Simple interface for calling an OpenCL kernel. Supports input
-- 'Vector' and 'Storable' arguments, and produces 'Vector'
-- outputs. Uses the actual pointers underlying any vector arguments,
-- improving performance of kernels run on the CPU. NOTE: some GPU
-- OpenCL drivers do not support this usage.
-- 
-- > (v1,v2) <- runKernelCPS cluState kernel vIn (Work1D 4) (Out 4) (Out 4)
runKernelCPS :: KernelArgsCPS a => OpenCLState -> CLKernel -> a
runKernelCPS s k = setArgCPS s k 0 Nothing Nothing []
