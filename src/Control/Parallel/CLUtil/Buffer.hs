{-# LANGUAGE ScopedTypeVariables, TupleSections, RankNTypes #-}
-- | Typed monadic interface for working with OpenCL buffers.
module Control.Parallel.CLUtil.Buffer where
import Control.Applicative ((<$>), (<$))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (evaluate)
import Control.Monad (when)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.IORef (newIORef, writeIORef, readIORef)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.Marshal.Utils (copyBytes)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (Storable(..))

import Control.Parallel.CLUtil.Async
import Control.Parallel.CLUtil.CL
import Control.Parallel.CLUtil.State (OpenCLState(clContext, clQueue))
import Control.Parallel.OpenCL

-- |Allocate a raw buffer whose contents are undefined.
initOutputBuffer :: Integral a => OpenCLState -> [CLMemFlag] -> a -> IO CLMem
initOutputBuffer s flags n = clCreateBuffer (clContext s) flags (n, nullPtr)

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
-- buffer is registered for cleanup, and the key used to perform an
-- early cleanup of the buffer is returned.
allocBufferKey :: Storable a
               => [CLMemFlag] -> Int -> CL (CLBuffer a, ReleaseKey)
allocBufferKey flags n = do b <- allocBuffer_ flags n
                            k <- registerCleanup $ () <$ releaseObject b
                            return (b, k)

-- | Allocate a new buffer object of the given number of elements. The
-- buffer is registered for cleanup.
allocBuffer :: Storable a => [CLMemFlag] -> Int -> CL (CLBuffer a)
allocBuffer flags n = fst <$> allocBufferKey flags n

-- | Allocate a new buffer object and write a 'Vector''s contents to
-- it. The buffer is /not/ registered for cleanup.
initBuffer_ :: forall a. Storable a
            => [CLMemFlag] -> V.Vector a -> CL (CLBuffer a)
initBuffer_ flags v = 
  do c <- clContext <$> ask
     fmap (CLBuffer (V.length v)) . liftIO . V.unsafeWith v $
       clCreateBuffer c flags . (sz,) . castPtr
  where sz = V.length v * sizeOf (undefined::a)

-- | Allocate a new buffer object and write a 'Vector''s contents to
-- it. The buffer is registered for cleanup, and the key used to
-- perform an early cleanup of the buffer is returned.
initBufferKey :: Storable a
              => [CLMemFlag] -> V.Vector a -> CL (CLBuffer a, ReleaseKey)
initBufferKey flags v = do b <- initBuffer_ flags v
                           k <- registerCleanup $ () <$ releaseObject b
                           return (b,k)

-- | Allocate a new buffer object and write a 'Vector''s contents to
-- it. The buffer is registered for cleanup.
initBuffer :: Storable a => [CLMemFlag] -> V.Vector a -> CL (CLBuffer a)
initBuffer flags v = fst <$> initBufferKey flags v

-- | @readBuffer' mem n events@ reads back a 'Vector' of @n@ elements
-- from the buffer object @mem@ after waiting for @events@ to finish.
readBufferAsync' :: forall a. Storable a => CLBuffer a -> Int -> [CLEvent]
                 -> CL (CLAsync (V.Vector a))
readBufferAsync' (CLBuffer n' mem) n waitForIt =
  do when (n > n') (throwError "Tried to read more elements than a buffer has")
     q <- clQueue <$> ask
     v <- liftIO $ VM.new n
     ev <- liftIO . VM.unsafeWith v $ \ptr ->
             do (_, src) <- clEnqueueMapBuffer q mem True [CL_MAP_READ] 0 sz
                                               waitForIt
                copyBytes (castPtr ptr) src sz
                clEnqueueUnmapMemObject q mem src []
                -- clEnqueueReadBuffer q mem True 0 sz (castPtr ptr) waitForIt
     return . clAsync ev $ liftIO $ V.unsafeFreeze v
  where sz = n * sizeOf (undefined::a)

-- | @readBuffer' buf n events@ performs a blocking read of the first
-- @n@ elements of a buffer after waiting for @events@.
readBuffer' :: Storable a => CLBuffer a -> Int -> [CLEvent] -> CL (V.Vector a)
readBuffer' buf n waitForIt = readBufferAsync' buf n waitForIt >>= waitOne

-- | @readBuffer mem@ reads back a 'Vector' containing all the data
-- stored in a 'CLBuffer'.
readBuffer :: Storable a => CLBuffer a -> CL (V.Vector a)
readBuffer b@(CLBuffer n _) = readBuffer' b n []

-- | Perform a non-blocking read of an buffer's entire contents.
readBufferAsync :: Storable a => CLBuffer a -> CL (CLAsync (V.Vector a))
readBufferAsync b@(CLBuffer n _) = readBufferAsync' b n []

-- | Write a 'Vector''s contents to a buffer object. This operation
-- is non-blocking.
writeBufferAsync :: forall a. Storable a
                 => CLBuffer a -> V.Vector a -> CL (CLAsync ())
writeBufferAsync (CLBuffer n mem) v = 
  do when (V.length v > n)
          (throwError "writeBuffer: Vector is bigger than the CLBuffer")
     q <- clQueue <$> ask
     ev <- liftIO . V.unsafeWith v $ \ptr ->
             clEnqueueWriteBuffer q mem True 0 sz (castPtr ptr) []
     return . clAsync ev $ return ()
  where sz = V.length v * sizeOf (undefined::a)

-- | Perform a blocking write of a 'Vector's contents to a buffer object.
writeBuffer :: Storable a => CLBuffer a -> V.Vector a -> CL ()
writeBuffer b v = writeBufferAsync b v >>= waitOne

-- | Create a read-only 'CLBuffer' that shares an underlying pointer
-- with a 'V.Vector', then apply a function to that buffer. This is
-- typically used to have an OpenCL kernel directly read from a
-- vector. If the OpenCL context can not directly use the pointer,
-- this will raise a runtime error!
withSharedVector :: forall a r. Storable a
                 => V.Vector a -> (CLBuffer a -> CL r) -> CL r
withSharedVector v go = 
  do ctx <- clContext <$> ask
     mem <- liftIO . V.unsafeWith v $ \ptr ->
       clCreateBuffer ctx [CL_MEM_READ_ONLY, CL_MEM_USE_HOST_PTR]
                      (sz, castPtr ptr)
     r <- go (CLBuffer (V.length v) mem)
     _ <- liftIO $ clReleaseMemObject mem
     return r
  where sz = V.length v * sizeOf (undefined::a)

-- | Create a read-write 'CLBuffer' that shares an underlying pointer
-- with an 'VM.IOVector', then apply the given function to that
-- buffer. This is typically used to have an OpenCL kernel write
-- directly to a Haskell vector. If the OpenCL context can not
-- directly use the pointer, this will raise a runtime error!
withSharedMVector :: forall a r. Storable a
                  => VM.IOVector a -> (CLBuffer a -> CL r) -> CL r
withSharedMVector v go =
  do ctx <- clContext <$> ask
     mem <- liftIO . VM.unsafeWith v $ \ptr ->
       clCreateBuffer ctx [CL_MEM_READ_WRITE, CL_MEM_USE_HOST_PTR]
                      (sz, castPtr ptr)
     r <- go (CLBuffer (VM.length v) mem)
     _ <- liftIO $ clReleaseMemObject mem
     return r
  where sz = VM.length v * sizeOf (undefined::a)

-- | Provides access to a memory-mapped 'VM.MVector' of a
-- 'CLBuffer'. The result of applying the given function to the vector
-- is evaluated to WHNF, but the caller should ensure that this is
-- sufficient to not require hanging onto a reference to the vector
-- data, as this reference will not be valid. Returning the vector
-- itself is right out. The 'CLMapFlag's supplied determine if we have
-- read-only, write-only, or read/write access to the 'VM.MVector'.
withBufferAsync_ :: forall a r. Storable a
                 => [CLMapFlag] -> CLBuffer a
                 -> (forall s. VM.MVector s a -> ST s r) -> CL (CLAsync r)
withBufferAsync_ flags (CLBuffer n mem) f =
  do q <- clQueue <$> ask
     liftIO $ 
       do done <- newEmptyMVar
          res <- newIORef undefined
          _ <- forkIO $ do
                 (ev,ptr) <- clEnqueueMapBuffer q mem True flags 0 sz []
                 fp <- newForeignPtr_ $ castPtr ptr
                 x <- evaluate =<< (unsafeSTToIO . f
                                    $ VM.unsafeFromForeignPtr0 fp n)
                 clEnqueueUnmapMemObject q mem ptr [ev] >>= waitReleaseEvent
                 writeIORef res x
                 putMVar done ()
          return $ ioAsync (takeMVar done) (liftIO $ readIORef res)
     -- liftIO $ do (ev, ptr) <- clEnqueueMapBuffer q mem False flags 0 sz []
     --             let go = do fp <- newForeignPtr_ $ castPtr ptr
     --                         x <- evaluate =<<
     --                              (unsafeSTToIO . f
     --                               $ VM.unsafeFromForeignPtr0 fp n)
     --                         ev' <- clEnqueueUnmapMemObject q mem ptr []
     --                         _ <- clWaitForEvents [ev'] >> clReleaseEvent ev'
     --                         return x
     --             return . clAsync ev $ liftIO go
  where sz = n * sizeOf (undefined::a)

-- | Provides read/write access to a memory-mapped 'VM.MVector' of a
-- 'CLImage'. The caller should ensure that this is sufficient to not
-- require hanging onto a reference to the vector data, as this
-- reference will not be valid. Returning the vector itself is right
-- out.
withBufferRWAsync :: Storable a
                  => CLBuffer a -> (forall s. VM.MVector s a -> ST s r)
                  -> CL (CLAsync r)
withBufferRWAsync = withBufferAsync_ [CL_MAP_READ, CL_MAP_WRITE]

-- | Provides read/write access to a memory-mapped 'VM.MVector' of a
-- 'CLBuffer'. The caller should ensure that this is sufficient to not
-- require hanging onto a reference to the vector data, as this
-- reference will not be valid. Returning the vector itself is right
-- out.
withBufferRW :: Storable a
             => CLBuffer a -> (forall s. VM.MVector s a -> ST s r) -> CL r
withBufferRW img f = withBufferRWAsync img f >>= waitOne

-- | Provides read-only access to a memory-mapped 'V.Vector' of a
-- 'CLBuffer'. The result of applying the given function to the vector
-- is evaluated to WHNF, but the caller should ensure that this is
-- sufficient to not require hanging onto a reference to the vector
-- data, as this reference will not be valid. Returning the vector
-- itself is right out.
withBufferAsync :: Storable a
                => CLBuffer a -> (V.Vector a -> r) -> CL (CLAsync r)
withBufferAsync img f = 
  withBufferAsync_ [CL_MAP_READ] img (fmap f . V.unsafeFreeze)

-- | Provides read/write access to a memory-mapped 'V.Vector' of a
-- 'CLBuffer'. The result of applying the given function to the vector
-- is evaluated to WHNF, but the caller should ensure that this is
-- sufficient to not require hanging onto a reference to the vector
-- data, as this reference will not be valid. Returning the vector
-- itself is right out.
withBuffer :: Storable a => CLBuffer a -> (V.Vector a -> r) -> CL r
withBuffer img f = withBufferAsync img f >>= waitOne
