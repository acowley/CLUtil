{-# LANGUAGE ScopedTypeVariables #-}
-- |Utility functions for working with 'Vector's and OpenCL memory
-- buffers.
module System.GPU.CLUtil.VectorBuffers where
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, finalizerFree)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, sizeOf)
import System.GPU.OpenCL
import System.GPU.CLUtil.State

-- |Fill an OpenCL memory buffer with a 'Vector'.
vectorToBuffer :: forall a. Storable a => CLContext -> Vector a -> IO CLMem
vectorToBuffer context v = 
  V.unsafeWith v $ \ptr ->
    -- NOTE: If we use the host ptr, then operations on the CPU are
    -- much faster. But if the GC moves things around, the world will
    -- end.
    clCreateBuffer context 
                   --[CL_MEM_READ_ONLY, CL_MEM_USE_HOST_PTR]
                   [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] 
                   (sz, castPtr ptr)
  where sz = V.length v * sizeOf (undefined::a)

-- |Pass a function a buffer whose contents are the data underlying a
-- 'Vector'.
withVectorBuffer :: forall a b. Storable a => 
                    CLContext -> Vector a -> (CLMem -> IO b) -> IO b
withVectorBuffer context v k = 
  V.unsafeWith v $ \ptr ->
    clCreateBuffer context
                   [CL_MEM_READ_ONLY, CL_MEM_USE_HOST_PTR] 
                   (sz, castPtr ptr) >>= k
  where sz = V.length v * sizeOf (undefined::a)

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
