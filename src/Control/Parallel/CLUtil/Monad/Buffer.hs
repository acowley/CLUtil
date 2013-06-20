{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
-- | Typed monadic interface for working with OpenCL buffers.
module Control.Parallel.CLUtil.Monad.Buffer where
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Parallel.CLUtil
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (Storable(sizeOf))

import Control.Parallel.CLUtil.Monad.CL

-- |A @CLBuffer a@ is a buffer object whose elements are of type
-- @a@. It is the caller's responsibility that the given type must
-- naturally map to an OpenCL type (e.g. 'Word8', 'Int32', 'Float').
data CLBuffer a = CLBuffer { bufferLength :: Int
                           , bufferObject :: CLMem }

-- | Allocate a new buffer object of the given number of elements.
allocBuffer :: forall a. Storable a => [CLMemFlag] -> Int -> CL (CLBuffer a)
allocBuffer flags n = 
  do s <- ask
     fmap (CLBuffer n) . liftIO $ initOutputBuffer s flags numBytes
  where numBytes = n * sizeOf (undefined::a)

-- | Allocate a new buffer object and write a 'Vector''s contents to
-- it.
initBuffer :: forall a. Storable a => [CLMemFlag] -> Vector a -> CL CLMem
initBuffer flags v = 
  do c <- clContext <$> ask
     liftIO . V.unsafeWith v $ clCreateBuffer c flags . (sz,) . castPtr
  where sz = V.length v * sizeOf (undefined::a)

-- | @readBuffer' mem n events@ reads back a 'Vector' of @n@ elements
-- from the buffer object @mem@ after waiting for @events@ to finish.
readBuffer' :: forall a. Storable a => CLBuffer a -> Int -> [CLEvent] -> CL (Vector a)
readBuffer' (CLBuffer n' mem) n waitForIt =
  do when (n > n') (throwError "Tried to read more elements than a buffer has")
     q <- clQueue <$> ask
     v <- liftIO $ VM.new n
     ev <- liftIO . VM.unsafeWith v $ \ptr ->
             clEnqueueReadBuffer q mem True 0 sz (castPtr ptr) waitForIt
     when (ev /= nullPtr)
          (do okay "wait for event" $ clWaitForEvents [ev]
              okay "release event" $ clReleaseEvent ev)
     liftIO $ V.unsafeFreeze v
  where sz = n * sizeOf (undefined::a)

-- | @readBuffer mem@ reads back a 'Vector' containing all the data
-- stored in a 'CLBuffer'.
readBuffer :: Storable a => CLBuffer a -> CL (Vector a)
readBuffer b@(CLBuffer n _) = readBuffer' b n []

-- | Write a 'Vector''s contents to a buffer object. This operation
-- blocks until the memory to be written is copied.
writeBuffer :: forall a. Storable a => CLBuffer a -> Vector a -> CL ()
writeBuffer (CLBuffer n mem) v = 
  do when (V.length v > n)
          (throwError "writeBuffer: Vector is bigger than the CLBuffer")
     q <- clQueue <$> ask
     ev <- liftIO . V.unsafeWith v $ \ptr ->
             clEnqueueWriteBuffer q mem True 0 sz (castPtr ptr) []
     when (ev /= nullPtr)
          (do okay "wait for event" $ clWaitForEvents [ev]
              okay "release event" $ clReleaseEvent ev)
  where sz = V.length v * sizeOf (undefined::a)
