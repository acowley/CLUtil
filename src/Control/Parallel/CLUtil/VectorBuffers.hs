{-# LANGUAGE ScopedTypeVariables #-}
-- |Utility functions for working with 'Vector's and OpenCL memory
-- buffers.
module Control.Parallel.CLUtil.VectorBuffers where
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, finalizerFree)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, sizeOf)
import Control.Parallel.OpenCL
import Control.Parallel.CLUtil.State

-- |Fill an OpenCL memory buffer with a 'Vector'.
vectorToBuffer :: forall a. Storable a => CLContext -> Vector a -> IO CLMem
vectorToBuffer context v = 
  V.unsafeWith v $ \ptr ->
    -- NOTE: If we use the host ptr, then operations on the CPU are
    -- much faster. But if the GC moves things around, the world will
    -- end.
    clCreateBuffer context 
                   [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] 
                   (sz, castPtr ptr)
  where sz = V.length v * sizeOf (undefined::a)

-- |Pass a function a buffer whose contents are the data underlying a
-- 'Vector'. In OpenCL parlance, this creates an OpenCL buffer with
-- the @CL_MEM_USE_HOST_PTR@ flag if the current device is a CPU.
withVectorBuffer :: forall a b. Storable a => 
                    OpenCLState -> Vector a -> (CLMem -> IO b) -> IO b
withVectorBuffer state v k = 
  do isCPU <- any isCPUDevice `fmap` clGetDeviceType (clDevice state)
     V.unsafeWith v $ \ptr ->
       clCreateBuffer (clContext state)
                      (if isCPU
                        then [CL_MEM_READ_ONLY, CL_MEM_USE_HOST_PTR] 
                        else [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR])
                      (sz, castPtr ptr) >>= k
  where sz = V.length v * sizeOf (undefined::a)
        isCPUDevice CL_DEVICE_TYPE_CPU = True
        isCPUDevice _ = False

-- |Read an OpenCL memory buffer into a 'Vector'.
bufferToVector :: forall a. Storable a => 
                  CLCommandQueue -> CLMem -> Int -> [CLEvent] -> IO (Vector a)
bufferToVector q mem count waitForIt = 
  do v <- VM.new count
     _ <- VM.unsafeWith v $ \ptr ->
            do ev <- clEnqueueReadBuffer q mem True 0 sz 
                                         (castPtr ptr) waitForIt
               clWaitForEvents [ev]
               clReleaseEvent ev
     V.unsafeFreeze v
  where sz = count * sizeOf (undefined::a)

imageToVector :: forall a. Storable a => 
                  CLCommandQueue -> CLMem -> (Int,Int,Int) -> [CLEvent] -> IO (Vector a)
imageToVector q mem dims waitForIt = 
  do v <- VM.new count
     _ <- VM.unsafeWith v $ \ptr ->
            do ev <- clEnqueueReadImage q mem True (0,0,0) dims 0 0
                                         (castPtr ptr) waitForIt
               clWaitForEvents [ev]
               clReleaseEvent ev
     V.unsafeFreeze v
  where count = let (w,h,d) = dims in w*h*d
        sz = count * sizeOf (undefined::a)

-- |Asynchronously read an OpenCL memory buffer into a new
-- 'Vector'. The returned action blocks until the read is finished,
-- then produces the 'Vector'.
bufferToVectorAsync :: forall a b. (Storable a, Integral b) => 
                  CLCommandQueue -> CLMem -> b -> [CLEvent] -> 
                  IO (IO (Vector a))
bufferToVectorAsync q mem count waitForIt = 
  do ptr <- mallocBytes (count' * sizeOf (undefined::a))
     readEvent <- clEnqueueReadBuffer q mem False 0 sz (castPtr ptr) waitForIt
     return $ do clWaitForEvents [readEvent]
                 clReleaseEvent readEvent
                 fp <- newForeignPtr finalizerFree ptr
                 V.unsafeFreeze $ VM.unsafeFromForeignPtr fp 0 count'
  where sz = count' * sizeOf (undefined::a)
        count' = fromIntegral count

-- |Allocate a buffer and prepare an action to efficiently produce a
-- vector from the buffer. This action should be run just once /after/
-- you are done with the output buffer. For example, the following
-- example, 
-- 
-- > (getVector, buffer) <- initOutputVector cluState [] 100
-- 
-- allocates a 100 element OpenCL buffer, and produces an action,
-- @getVector@, that can be used to produce a 'Vector' after all
-- needed buffer manipulation is complete.
initOutputVector :: forall a b. (Integral a, Storable b) => 
                    OpenCLState -> [CLMemFlag] -> a -> IO (IO (Vector b), CLMem)
initOutputVector s flags n = 
  do p <- mallocBytes (fromIntegral n * sizeOf(undefined::b))
     b <- clCreateBuffer (clContext s) (CL_MEM_USE_HOST_PTR:flags) 
                         (n, castPtr p)
     let mkVec = do fp <- newForeignPtr finalizerFree p
                    V.unsafeFreeze $ 
                     VM.unsafeFromForeignPtr fp 0 (fromIntegral n)
     return (mkVec, b)
