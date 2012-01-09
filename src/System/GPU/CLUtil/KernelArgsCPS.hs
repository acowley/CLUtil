{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
-- |Synchronous OpenCL kernel execution that avoids copying input
-- 'Vector's when running the OpenCL kernel on the CPU.
module System.GPU.CLUtil.KernelArgsCPS (KernelArgsCPS, runKernelCPS) where
import Control.Applicative
import Control.Monad (void, when)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, withForeignPtr, 
                           mallocForeignPtrBytes)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (Storable, sizeOf)
import System.GPU.CLUtil.KernelArgTypes 
import System.GPU.CLUtil.State
import System.GPU.CLUtil.VectorBuffers
import System.GPU.OpenCL

-- In this variation, reading an output is an action that frees the
-- OpenCL buffer and provides an allocated ForeignPtr and the number
-- of elements in the output vector.
data PostExec = ReadOutput (IO (ForeignPtr (), Int))
              | FreeInput (IO ())

postToEither :: PostExec -> Either (IO (ForeignPtr (), Int)) (IO ())
postToEither (ReadOutput r) = Left r
postToEither (FreeInput m) = Right m

partitionPost :: [PostExec] -> ([IO (ForeignPtr (), Int)], [IO ()])
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
mkRead :: Storable a => (ForeignPtr (), Int) -> IO (Vector a)
mkRead (ptr,num) = V.unsafeFreeze $
                   VM.unsafeFromForeignPtr (castForeignPtr ptr) 0 num

class KernelArgsCPS a where
  -- Setting an argument requires a state, a kernel, the position of
  -- the argument, the number of work items specified so far, and a
  -- list of actions that prepare argument buffers and return the
  -- cleanup action to take when the kernel is finished.
  setArgCPS :: OpenCLState -> CLKernel -> CLuint -> Maybe NumWorkItems -> 
               [PrepExec] -> a

-- Nest a stack of buffer preparation actions. This lets us safely use
-- the pointers underlying 'Vector' arguments without an extra copy.
nestM :: [Int] -> IO () -> [PrepExec] -> IO [PostExec]
nestM outputSizes finish = go [] outputSizes
  where go acc _ [] = finish >> return acc
        go acc sizes (m:ms) = m (\f -> do (cleanup, sizes') <- f sizes
                                          go (maybe acc (:acc) cleanup)
                                             sizes'
                                             ms)

runCPS :: [Int] -> OpenCLState -> CLKernel -> NumWorkItems -> 
          [PrepExec] -> 
          IO ([IO (ForeignPtr (), Int)], [IO ()])
runCPS outputSizes s k n prep =
  partitionPost <$> nestM outputSizes runK prep
  where runK = do ev <- clEnqueueNDRangeKernel (clQueue s) 
                                               k 
                                               (workItemsList n)
                                               [] []
                  clWaitForEvents [ev]
                  void $ clReleaseEvent ev

-- Synchronous execution of a kernel with no automatic outputs. This
-- is useful for kernels that modify user-managed buffers.
instance KernelArgsCPS (IO ()) where
  setArgCPS s k _ (Just n) prep = do
    (o,cleanup) <- runCPS [] s k n prep
    when (not (null o)) (error "Outputs aren't bound!")
    sequence_ cleanup

-- Execute a kernel where the calling context is expecting a single
-- 'Vector' return value.
instance forall a. Storable a => KernelArgsCPS (IO (Vector a)) where
  setArgCPS s k _ (Just n) prep = do
    (o,cleanup) <- runCPS [sizeOf (undefined::a)] s k n prep
    r1 <- case o of
               [] -> error "One output bound, none specified"
               [f] -> mkRead <$> f
               _ -> error "More outputs specified than bound"
    sequence_ cleanup
    r1

-- Execute a kernel where the calling context is expecting two
-- 'Vector' return values.
instance forall a b. (Storable a, Storable b) => 
  KernelArgsCPS (IO (Vector a, Vector b)) where
  setArgCPS s k _ (Just n) prep = do
    (o, cleanup) <- runCPS [sizeOf (undefined::a), sizeOf (undefined::b)]
                           s k n prep
    (r1,r2) <- case o of
                 [] -> error "Two output bound, none specified"
                 [_] -> error "Two outputs bound, one specified"
                 [f,g] -> (,) <$> (mkRead <$> f) <*> (mkRead <$> g)
                 _ -> error "More outputs specified than bound"
    sequence_ cleanup
    (,) <$> r1 <*> r2

-- Execute a kernel where the calling context is expecting three
-- 'Vector' return values.
instance forall a b c. (Storable a, Storable b, Storable c) => 
  KernelArgsCPS (IO (Vector a, Vector b, Vector c)) where
  setArgCPS s k _ (Just n) prep = do
    (o, cleanup) <- runCPS [ sizeOf (undefined::a)
                           , sizeOf (undefined::b)
                           , sizeOf (undefined::c) ]
                           s k n prep
    (r1,r2,r3) <- case o of
                    [f,g,h] -> (,,) <$> (mkRead <$> f) 
                                    <*> (mkRead <$> g) 
                                    <*> (mkRead <$> h)
                    _ -> error "Different number of outputs specified than bound"
    sequence_ cleanup
    (,,) <$> r1 <*> r2 <*> r3

-- Pass an arbitrary 'Storable' as a kernel argument.
instance (Storable a, KernelArgsCPS r) => KernelArgsCPS (a -> r) where
  setArgCPS s k arg n prep = \a -> 
                             let load cont = clSetKernelArg k arg a >>
                                             cont (\sz -> return (Nothing,sz))
                             in setArgCPS s k (arg+1) n (load : prep)

-- Handle 'Vector' input arguments.
instance (Storable a, KernelArgsCPS r) => KernelArgsCPS (Vector a -> r) where
  setArgCPS s k arg n prep = 
    \v -> let load cont = withVectorBuffer (clContext s) v $
                          \b -> let clean = FreeInput . void $
                                            clReleaseMemObject b
                                in do clSetKernelArg k arg b
                                      cont (\sz -> return (Just clean, sz))
          in setArgCPS s k (arg+1) n (load:prep)

-- Keep track of an argument that specifies the number of work items
-- to execute.
instance KernelArgsCPS r => KernelArgsCPS (NumWorkItems -> r) where
  setArgCPS s k arg _ prep = \n -> setArgCPS s k arg (Just n) prep

-- Handle 'Vector' outputs by automatically managing the underlying
-- OpenCL buffers.
instance KernelArgsCPS r => KernelArgsCPS (OutputSize -> r) where
  setArgCPS s k arg n prep = 
    \(Out m) -> 
      let load cont = cont allocateOutput
          allocateOutput [] = error "Couldn't determine output element size"
          allocateOutput (sz:szs) = 
            do ptr <- mallocForeignPtrBytes (m*sz)
               b <- withForeignPtr ptr $ \p ->
                    clCreateBuffer (clContext s)
                                   [ CL_MEM_WRITE_ONLY
                                   , CL_MEM_USE_HOST_PTR ]
                                   (m*sz, castPtr p)
               clSetKernelArg k arg b
               let finishOutput = clReleaseMemObject b >> return (ptr,m)
               return (Just $ ReadOutput finishOutput, szs) 
      in setArgCPS s k (arg+1) n (load:prep)

-- |Simple interface for calling an OpenCL kernel. Supports input
-- 'Vector' and 'Storable' arguments, and produces 'Vector'
-- outputs. Uses the actual pointers underlying any vector arguments,
-- improving performance of kernels run on the CPU.
-- 
-- > (v1,v2) <- runKernelCPS cluState kernel vIn (Work1D 4) (Out 4) (Out 4)
runKernelCPS :: KernelArgsCPS a => OpenCLState -> CLKernel -> a
runKernelCPS s k = setArgCPS s k 0 Nothing []
