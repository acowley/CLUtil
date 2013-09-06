{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
-- | Typed monadic interface for working with OpenCL buffers.
module Control.Parallel.CLUtil.Monad.Buffer where
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Parallel.CLUtil
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

import Control.Parallel.CLUtil.Monad.CL
import Control.Parallel.CLUtil.Monad.Async

-- |A @CLBuffer a@ is a buffer object whose elements are of type
-- @a@. It is the caller's responsibility that the given type must
-- naturally map to an OpenCL type (e.g. 'Word8', 'Int32', 'Float').
data CLBuffer a = CLBuffer { bufferLength :: Int
                           , bufferObject :: CLMem }

-- | NOTE: This is an /EVIL/ 'Storable' instance that lets us treat a
-- 'CLBuffer' as its underlying 'CLMem' value for the sake of
-- interoperating with OpenCL. The 'Storable' instance does /not/ let
-- you roundtrip a value using 'peek' and 'poke'.
instance Storable (CLBuffer a) where
  sizeOf _ = sizeOf (undefined::CLMem)
  alignment _ = alignment (undefined::CLMem)
  peek = fmap (CLBuffer (error "Tried to peek a CLBuffer")) . peek . castPtr
  poke ptr (CLBuffer _ m) = poke (castPtr ptr) m

instance CLReleasable (CLBuffer a) where
  releaseObject (CLBuffer _ m) = clReleaseMemObject m

-- | Allocate a new buffer object of the given number of elements. The
-- buffer is /not/ registered for cleanup.
allocBuffer_ :: forall a. Storable a => [CLMemFlag] -> Int -> CL (CLBuffer a)
allocBuffer_ flags n = 
  do s <- ask
     fmap (CLBuffer n) . liftIO $ initOutputBuffer s flags numBytes
  where numBytes = n * sizeOf (undefined::a)

-- | Allocate a new buffer object of the given number of elements. The
-- buffer is registered for cleanup.
allocBuffer :: Storable a => [CLMemFlag] -> Int -> CL (CLBuffer a)
allocBuffer flags n = do b <- allocBuffer_ flags n
                         registerCleanup $ releaseObject b
                         return b

-- | Allocate a new buffer object and write a 'Vector''s contents to
-- it. The buffer is /not/ registered for cleanup.
initBuffer_ :: forall a. Storable a => [CLMemFlag] -> Vector a -> CL (CLBuffer a)
initBuffer_ flags v = 
  do c <- clContext <$> ask
     fmap (CLBuffer (V.length v)) . liftIO . V.unsafeWith v $
       clCreateBuffer c flags . (sz,) . castPtr
  where sz = V.length v * sizeOf (undefined::a)

-- | Allocate a new buffer object and write a 'Vector''s contents to
-- it. The buffer is registered for cleanup.
initBuffer :: Storable a => [CLMemFlag] -> Vector a -> CL (CLBuffer a)
initBuffer flags v = do b <- initBuffer_ flags v
                        registerCleanup $ releaseObject b
                        return b

-- | @readBuffer' mem n events@ reads back a 'Vector' of @n@ elements
-- from the buffer object @mem@ after waiting for @events@ to finish.
readBufferAsync' :: forall a. Storable a => CLBuffer a -> Int -> [CLEvent]
                 -> CL (CLAsync (Vector a))
readBufferAsync' (CLBuffer n' mem) n waitForIt =
  do when (n > n') (throwError "Tried to read more elements than a buffer has")
     q <- clQueue <$> ask
     v <- liftIO $ VM.new n
     ev <- liftIO . VM.unsafeWith v $ \ptr ->
             clEnqueueReadBuffer q mem True 0 sz (castPtr ptr) waitForIt
     return $ (ev, liftIO $ V.unsafeFreeze v)
  where sz = n * sizeOf (undefined::a)

-- | @readBuffer' buf n events@ performs a blocking read of the first
-- @n@ elements of a buffer after waiting for @events@.
readBuffer' :: forall a. Storable a => CLBuffer a -> Int -> [CLEvent] -> CL (Vector a)
readBuffer' buf n waitForIt = readBufferAsync' buf n waitForIt >>= waitOne

-- | @readBuffer mem@ reads back a 'Vector' containing all the data
-- stored in a 'CLBuffer'.
readBuffer :: Storable a => CLBuffer a -> CL (Vector a)
readBuffer b@(CLBuffer n _) = readBuffer' b n []

-- | Perform a non-blocking read of an buffer's entire contents.
readBufferAsync :: Storable a => CLBuffer a -> CL (CLAsync (Vector a))
readBufferAsync b@(CLBuffer n _) = readBufferAsync' b n []

-- | Write a 'Vector''s contents to a buffer object. This operation
-- is non-blocking.
writeBufferAsync :: forall a. Storable a => CLBuffer a -> Vector a -> CL (CLAsync ())
writeBufferAsync (CLBuffer n mem) v = 
  do when (V.length v > n)
          (throwError "writeBuffer: Vector is bigger than the CLBuffer")
     q <- clQueue <$> ask
     ev <- liftIO . V.unsafeWith v $ \ptr ->
             clEnqueueWriteBuffer q mem True 0 sz (castPtr ptr) []
     return (ev, return ())
  where sz = V.length v * sizeOf (undefined::a)

-- | Perform a blocking write of a 'Vector's contents to a buffer object.
writeBuffer :: Storable a => CLBuffer a -> Vector a -> CL ()
writeBuffer b v = writeBufferAsync b v >>= waitOne