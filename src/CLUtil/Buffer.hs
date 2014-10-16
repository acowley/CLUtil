{-# LANGUAGE ConstraintKinds, FlexibleContexts, ScopedTypeVariables,
             TupleSections, RankNTypes #-}
-- | Typed monadic interface for working with OpenCL buffers.
module CLUtil.Buffer where
import Control.Applicative ((<$>), (<$))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (evaluate)
import Control.Monad (when)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.Marshal.Utils (copyBytes)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (Storable(..))

import CLUtil.Async
import CLUtil.CL
import CLUtil.State (OpenCLState(clContext, clQueue))
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

instance HasCLMem (CLBuffer a) where
  getCLMem (CLBuffer _ m) = m

-- | Allocate a new buffer object of the given number of elements.
allocBuffer :: forall a m. (Storable a, CL' m)
            => [CLMemFlag] -> Int -> m (CLBuffer a)
allocBuffer flags n = 
  do s <- ask
     fmap (CLBuffer n) . liftIO $ initOutputBuffer s flags numBytes
  where numBytes = n * sizeOf (undefined::a)

-- | Allocate a new buffer object and write a 'Vector''s contents to
-- it.
initBuffer :: forall a m. (Storable a, CL' m)
           => [CLMemFlag] -> V.Vector a -> m (CLBuffer a)
initBuffer flags v = 
  do c <- clContext <$> ask
     fmap (CLBuffer (V.length v)) . liftIO . V.unsafeWith v $
       clCreateBuffer c flags . (sz,) . castPtr
  where sz = V.length v * sizeOf (undefined::a)

-- | @readBuffer' mem n events@ reads back a 'Vector' of @n@ elements
-- from the buffer object @mem@ after waiting for @events@ to finish.
readBufferAsync' :: forall a m. (Storable a, CL' m)
                 => CLBuffer a -> Int -> [CLEvent]
                 -> m (CLAsync (V.Vector a))
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
readBuffer' :: (Storable a, CL' m)
            => CLBuffer a -> Int -> [CLEvent] -> m (V.Vector a)
readBuffer' buf n waitForIt = readBufferAsync' buf n waitForIt >>= waitOne

-- | @readBuffer mem@ reads back a 'Vector' containing all the data
-- stored in a 'CLBuffer'.
readBuffer :: (Storable a, CL' m) => CLBuffer a -> m (V.Vector a)
readBuffer b@(CLBuffer n _) = readBuffer' b n []

-- | Perform a non-blocking read of an buffer's entire contents.
readBufferAsync :: (Storable a, CL' m) => CLBuffer a -> m (CLAsync (V.Vector a))
readBufferAsync b@(CLBuffer n _) = readBufferAsync' b n []

-- | Write a 'Vector''s contents to a buffer object. This operation
-- is non-blocking.
writeBufferAsync :: forall a m. (Storable a, CL' m)
                 => CLBuffer a -> V.Vector a -> m (CLAsync ())
writeBufferAsync (CLBuffer n mem) v = 
  do when (V.length v > n)
          (throwError "writeBuffer: Vector is bigger than the CLBuffer")
     q <- clQueue <$> ask
     ev <- liftIO . V.unsafeWith v $ \ptr ->
             clEnqueueWriteBuffer q mem True 0 sz (castPtr ptr) []
     return . clAsync ev $ return ()
  where sz = V.length v * sizeOf (undefined::a)

-- | Perform a blocking write of a 'Vector's contents to a buffer object.
writeBuffer :: (Storable a, CL' m) => CLBuffer a -> V.Vector a -> m ()
writeBuffer b v = writeBufferAsync b v >>= waitOne

-- | Create a read-only 'CLBuffer' that shares an underlying pointer
-- with a 'V.Vector', then apply a function to that buffer. This is
-- typically used to have an OpenCL kernel directly read from a
-- vector. If the OpenCL context can not directly use the pointer,
-- this will raise a runtime error!
withSharedVector :: forall a r m. (Storable a, CL' m)
                 => V.Vector a -> (CLBuffer a -> m r) -> m r
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
withSharedMVector :: forall a r m. (Storable a, CL' m)
                  => VM.IOVector a -> (CLBuffer a -> m r) -> m r
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
withBufferAsync_ :: forall a r m. (Storable a, CL' m)
                 => [CLMapFlag] -> CLBuffer a
                 -> (forall s. VM.MVector s a -> ST s r) -> m (m r)
withBufferAsync_ flags (CLBuffer n mem) f =
  do q <- clQueue <$> ask
     liftIO $ 
       do done <- newEmptyMVar
          _ <- forkIO $ do
                 (ev,ptr) <- clEnqueueMapBuffer q mem True flags 0 sz []
                 fp <- newForeignPtr_ $ castPtr ptr
                 x <- evaluate =<< (unsafeSTToIO . f
                                    $ VM.unsafeFromForeignPtr0 fp n)
                 clEnqueueUnmapMemObject q mem ptr [ev] >>= waitReleaseEvent
                 putMVar done x
          return $ (liftIO $ takeMVar done)
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
withBufferRWAsync :: (Storable a, CL' m)
                  => CLBuffer a -> (forall s. VM.MVector s a -> ST s r)
                  -> m (m r)
withBufferRWAsync = withBufferAsync_ [CL_MAP_READ, CL_MAP_WRITE]

-- | Provides read/write access to a memory-mapped 'VM.MVector' of a
-- 'CLBuffer'. The caller should ensure that this is sufficient to not
-- require hanging onto a reference to the vector data, as this
-- reference will not be valid. Returning the vector itself is right
-- out.
withBufferRW :: (Storable a, CL' m)
             => CLBuffer a -> (forall s. VM.MVector s a -> ST s r) -> m r
withBufferRW img f = withBufferRWAsync img f >>= id

-- | Provides read-only access to a memory-mapped 'V.Vector' of a
-- 'CLBuffer'. The result of applying the given function to the vector
-- is evaluated to WHNF, but the caller should ensure that this is
-- sufficient to not require hanging onto a reference to the vector
-- data, as this reference will not be valid. Returning the vector
-- itself is right out.
withBufferAsync :: (Storable a, CL' m)
                => CLBuffer a -> (V.Vector a -> r) -> m (m r)
withBufferAsync img f = 
  withBufferAsync_ [CL_MAP_READ] img (fmap f . V.unsafeFreeze)

-- | Provides read/write access to a memory-mapped 'V.Vector' of a
-- 'CLBuffer'. The result of applying the given function to the vector
-- is evaluated to WHNF, but the caller should ensure that this is
-- sufficient to not require hanging onto a reference to the vector
-- data, as this reference will not be valid. Returning the vector
-- itself is right out.
withBuffer :: (Storable a, CL' m) => CLBuffer a -> (V.Vector a -> r) -> m r
withBuffer img f = withBufferAsync img f >>= id
