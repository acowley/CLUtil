{-# LANGUAGE ConstraintKinds, FlexibleContexts, ScopedTypeVariables,
             TemplateHaskell #-}
-- | A resource-tracking monad inspired by the @resourcet@
-- package. The distinction here is that we commonly want to separate
-- execution of an action prepared in the resource-tracking monad from
-- the cleanup of the resources it refers to. Typical usage will
-- involve allocating OpenCL images and buffers, then returning 'IO'
-- functions a caller can execute multiple times along with another
-- 'IO' action that frees resources.
module CLUtil.CL.Resource (
  -- * Initialization
  ezInit, clDeviceGPU, clDeviceCPU, clDeviceSelect,
  ezRelease, OpenCLState(..),

  -- * Running OpenCL computations
  CL, CL', runCL, runCL', runCLIO, runCLClean, nestCL,

  -- * Mangaging images and buffers
  Cleanup, registerCleanup, unregisterCleanup, ReleaseKey,
  runCleanup, releaseItem, CLReleasable(releaseObject), newCleanup,

  -- * Kernels
  KernelArgsCL, runKernel, runKernelAsync,

  -- * Operations in the @CL@ monad
  ask, throwError, liftIO, -- okay,

  -- * Buffer Objects
  CLBuffer(..), allocBuffer, allocBufferKey, 
  initBuffer, initBufferKey,
  readBuffer, readBuffer', writeBuffer, withSharedVector, withSharedMVector,

  -- * Image Objects
  CLImage(..), allocImage, allocImageKey, allocImageFmt,
  initImage, initImageKey, initImageFmt,
  readImage, readImage', writeImage, copyImage,
  NumChan(..), HalfFloat, 
  NormInt8(..), NormWord8(..), NormInt16(..), NormWord16(..),
  CLImage1, CLImage2, CLImage3, CLImage4,

  -- * Buffer-Image Interoperation
  copyBufferToImage, copyBufferToImageAsync,
  copyImageToBuffer, copyImageToBufferAsync,

  -- * Asynchonous Computations
  CLAsync, waitAll, waitAll_, waitAll', waitAllUnit, waitOne,
  readImageAsync', readImageAsync, copyImageAsync,
  writeImageAsync, readBufferAsync, readBufferAsync', writeBufferAsync,

  -- * OpenCL kernel arguments
  OutputSize(..), NumWorkItems(..), WorkGroup(..),
  LocalMem(..), localFloat, localDouble, localInt, localWord32,

  -- * Re-exports for convenience
  module Control.Parallel.OpenCL, Vector, CInt, CFloat, Word8, Storable
{-
  -- * Images
  allocImageFmt, allocImageKey, allocImage, allocImage_,
  initImageFmt, initImageKey, initImage, initImage_,
  module CLUtil.Image,
  -- * Buffers
  allocBufferKey, allocBuffer, initBufferKey, initBuffer,
  module CLUtil.Buffer,
  -- * Resource utilities
  registerCleanup, unregisterCleanup, runCleanup, releaseItem, ReleaseKey,
  newCleanup
-}
  ) where
import CLUtil hiding (allocBuffer, initBuffer, allocImage, initImage, CL, runCL)
import CLUtil.Buffer hiding (allocBuffer, initBuffer)
import qualified CLUtil.Buffer as B
import qualified CLUtil.CL as R
import CLUtil.Image hiding (allocImageFmt, allocImage, initImageFmt, initImage)
import qualified CLUtil.Image as I
import CLUtil.State
import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Parallel.OpenCL
import Data.Foldable (Foldable, sequenceA_)
import qualified Data.Foldable as F
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import Data.Proxy
import qualified Data.Vector.Storable as V
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable)

-- | Capture an 'OpenCLState' and track resource allocations.
type CL = StateT Cleanup R.CL

-- | A constraint corresponding to features supported by 'CL'.
type CL' m = (MonadState Cleanup m, R.CL' m)

-- | Release resources used by OpenCL.
data Cleanup = Cleanup { _nextReleaseKey :: !Int
                       , _releaseMap     :: !(IntMap (IO ()))  }
makeLenses ''Cleanup

-- | This is a somewhat dangerous 'Monoid' instance. If you have two
-- independently created 'Cleanup' values, and you have held on to
-- 'ReleaseKey's for specific resources, then combining 'Cleanup'
-- values will link the cleanup actions for keys found in both. This
-- means that running the cleanup action for one resource might cause
-- the cleanup action for another resource to run. This is a downside
-- to stateful programming where we hold on to references
-- (i.e. 'ReleaseKey's).
instance Monoid Cleanup where
  mempty = newCleanup
  Cleanup k1 m1 `mappend` Cleanup k2 m2 = Cleanup (max k1 k2)
                                                  (IM.unionWith (>>) m1 m2)

type ReleaseKey = Int

newCleanup :: Cleanup
newCleanup = Cleanup 0 mempty

-- | Run all cleanup actions.
runCleanup :: Cleanup -> IO ()
runCleanup (Cleanup _ m) = sequenceA_ m

-- | Register a cleanup action.
registerCleanup :: CL' m => IO () -> m ReleaseKey
registerCleanup m = do i <- use nextReleaseKey
                       releaseMap . at i .= Just m
                       nextReleaseKey += 1
                       return i

-- | Return a previously-registered cleanup action without running
-- it. This returns control over this resource to the caller.
unregisterCleanup :: CL' m => ReleaseKey -> m (Maybe (IO ()))
unregisterCleanup k = do m <- use (releaseMap . at k)
                         releaseMap . at k .= Nothing
                         return m

-- | Run a previously-registered cleanup action. It will not be run again.
releaseItem :: CL' m => ReleaseKey -> m ()
releaseItem k = do use (releaseMap . at k) >>= liftIO . sequenceA_
                   releaseMap . at k .= Nothing

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error'.
runCL :: OpenCLState -> CL a -> IO (a, Cleanup)
runCL s m = R.runCL s (runStateT m newCleanup)

-- | Run a 'CL' action in an environment with a fresh cleanup
-- record. This lets the caller embed an action whose resource
-- management should be kept separate from the ambient context.
nestCL :: CL a -> CL (a, Cleanup)
nestCL m = ask >>= liftIO . flip runCL m

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error'. This function behaves identically to
-- 'runCL' with the exception that the 'Cleanup' action is returned as
-- an unadorned 'IO' action. This may help isolate callers from any
-- awareness of OpenCL, but makes the types a bit more ambiguous.
runCLIO :: OpenCLState -> CL a -> IO (a, IO ())
runCLIO s m = (_2 %~ runCleanup) <$> runCL s m

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error', and all cleanup actions are run before
-- returning the result of the computation.
runCLClean :: OpenCLState -> CL a -> IO a
runCLClean s m = runCLIO s m >>= (\(x,c) -> x <$ c)

-- | Run a 'CL' action that discards any accumulated cleanup actions.
runCL' :: OpenCLState -> CL a -> IO a
runCL' = (fmap fst .) . runCL

-- | A class for things that can be released from OpenCL.
class CLReleasable a where
  -- | Decrement the reference count of a memory object.
  releaseObject :: a -> IO Bool

-- * Images

instance CLReleasable (CLImage n a) where
  releaseObject (CLImage _ m) = clReleaseMemObject m

-- | Allocate a new 2D or 3D image of the given dimensions and
-- format. The image is registered for cleanup.
allocImageFmt :: (Integral a, Functor f, Foldable f, ValidImage n b, CL' m)
              => [CLMemFlag] -> CLImageFormat -> f a
              -> m (CLImage n b, ReleaseKey)
allocImageFmt flags fmt dims =
  do img <- I.allocImageFmt flags fmt dims
     k <- registerCleanup $ () <$ releaseObject img
     return (img,k)

-- | Allocate a new 2D or 3D image of the given dimensions. The image
-- format is the default for the the return type (e.g. the type
-- 'CLImage OneChan Float' is associated with a default format of
-- 'CLImageFormat CL_R CL_FLOAT') . The image is registered for
-- cleanup, and the key used to perform an early cleanup of the image
-- is returned.
allocImageKey :: forall f a n b m.
                 (Integral a, Foldable f, Functor f, ValidImage n b, CL' m)
              => [CLMemFlag] -> f a -> m (CLImage n b, ReleaseKey)
allocImageKey flags = allocImageFmt flags fmt
  where fmt = defaultFormat (Proxy::Proxy (CLImage n b))

-- | Allocate a new 2D or 3D image of the given dimensions. The image
-- format is the default for the the return type (e.g. the type
-- 'CLImage OneChan Float' is associated with a default format of
-- 'CLImageFormat CL_R CL_FLOAT') . The image is registered for
-- cleanup.
allocImage :: (Integral a, Functor f, Foldable f, ValidImage n b, CL' m)
           => [CLMemFlag] -> f a -> m (CLImage n b)
allocImage flags = fmap fst . allocImageKey flags

-- | Allocate a new 2D or 3D image of the given dimensions. The image
-- format is the default for the the return type (e.g. the type
-- 'CLImage OneChan Float' is associated with a default format of
-- 'CLImageFormat CL_R CL_FLOAT'). The image is /not/ registered for
-- cleanup.
allocImage_ :: forall f a n b m.
               (Integral a, Foldable f, Functor f, ValidImage n b, CL' m)
            => [CLMemFlag] -> f a -> m (CLImage n b)
allocImage_ flags = I.allocImageFmt flags fmt
  where fmt = defaultFormat (Proxy::Proxy (CLImage n b))

-- | Initialize a new 2D or 3D image of the given dimensions with a
-- 'Vector' of pixel data. Note that the pixel data is /flattened/
-- across however many channels each pixel may represent. For example,
-- if we have a three channel RGB image with a data type of 'Float',
-- then we expect a 'Vector Float' with a number of elements equal to
-- 3 times the number of pixels. The image is /not/ registered for
-- cleanup.
initImageFmt_ :: forall a f n b m.
                 (Integral a, Foldable f, Functor f, Storable b, ValidImage n b,
                  CL' m)
               => [CLMemFlag] -> CLImageFormat -> f a -> V.Vector b
               -> m (CLImage n b)
initImageFmt_ flags fmt dims v =
  do imageCompatible fmt (Proxy::Proxy (CLImage n b))
     when (V.length v /= fromIntegral (F.product dims)*numChan (Proxy::Proxy n))
          (throwError "Vector is not the same size as the desired image")
     c <- clContext <$> ask
     case F.toList (fromIntegral <$> dims) of
       [w,h]   -> fmap (CLImage (w,h,1)) . liftIO . V.unsafeWith v $
                    clCreateImage2D c flags fmt w h 0 . castPtr
       [w,h,d] -> fmap (CLImage (w,h,d)) . liftIO . V.unsafeWith v $
                    clCreateImage3D c flags fmt w h d 0 0 . castPtr
       _       -> throwError "Only 2D and 3D images are currently supported!"


-- | Initialize a new 2D or 3D image of the given dimensions with a
-- 'Vector' of pixel data. Note that the pixel data is /flattened/
-- across however many channels each pixel may represent. For example,
-- if we have a three channel RGB image with a data type of 'Float',
-- then we expect a 'Vector Float' with a number of elements equal to
-- 3 times the number of pixels. The image is /not/ registered for
-- cleanup.
initImageFmt :: (Integral a, Functor f, Foldable f, Storable b, ValidImage n b,
                 CL' m)
             => [CLMemFlag] -> CLImageFormat -> f a -> V.Vector b
             -> m (CLImage n b, ReleaseKey)
initImageFmt flags fmt dims v =
  do img <- I.initImageFmt flags fmt dims v
     k <- registerCleanup $ () <$ releaseObject img
     return (img, k)

-- | Initialize an image of the given dimensions with the a 'Vector'
-- of pixel data. A default image format is deduced from the return
-- type. See 'initImage'' for more information on requirements of the
-- input 'Vector'. The image is registered for cleanup, and the key
-- used to perform an early cleanup of the image is returned.
initImageKey :: forall f a n b m.
                (Integral a, Foldable f, Functor f, ValidImage n b, Storable b,
                 CL' m)
             => [CLMemFlag] -> f a -> V.Vector b -> m (CLImage n b, ReleaseKey)
initImageKey flags = initImageFmt flags fmt
  where fmt = defaultFormat (Proxy::Proxy (CLImage n b))

-- | Initialize an image of the given dimensions with the a 'Vector'
-- of pixel data. A default image format is deduced from the return
-- type. See 'initImage'' for more information on requirements of the
-- input 'Vector'. The image is registered for cleanup.
initImage :: (Integral a, Foldable f, Functor f, ValidImage n b, Storable b,
              CL' m)
          => [CLMemFlag] -> f a -> V.Vector b -> m (CLImage n b)
initImage flags = (fmap fst .) . initImageKey flags

-- | Initialize an image of the given dimensions with the a 'Vector'
-- of pixel data. A default image format is deduced from the return
-- type. See 'initImage'' for more information on requirements of the
-- input 'Vector'. The image is /not/ registered for cleanup.
initImage_ :: forall f a n b m.
              (Integral a, Foldable f, Functor f, ValidImage n b, Storable b,
               CL' m)
           => [CLMemFlag] -> f a -> V.Vector b -> m (CLImage n b)
initImage_ flags = initImageFmt_ flags fmt
  where fmt = defaultFormat (Proxy::Proxy (CLImage n b))

-- * Buffers

instance CLReleasable (CLBuffer a) where
  releaseObject (CLBuffer _ m) = clReleaseMemObject m

-- | Allocate a new buffer object of the given number of elements. The
-- buffer is registered for cleanup, and the key used to perform an
-- early cleanup of the buffer is returned.
allocBufferKey :: (Storable a, CL' m)
               => [CLMemFlag] -> Int -> m (CLBuffer a, ReleaseKey)
allocBufferKey flags n = do b <- B.allocBuffer flags n
                            k <- registerCleanup $ () <$ releaseObject b
                            return (b, k)

-- | Allocate a new buffer object of the given number of elements. The
-- buffer is registered for cleanup.
allocBuffer :: (Storable a, CL' m) => [CLMemFlag] -> Int -> m (CLBuffer a)
allocBuffer flags n = fst <$> allocBufferKey flags n

-- | Allocate a new buffer object and write a 'Vector''s contents to
-- it. The buffer is registered for cleanup, and the key used to
-- perform an early cleanup of the buffer is returned.
initBufferKey :: Storable a
              => [CLMemFlag] -> V.Vector a -> CL (CLBuffer a, ReleaseKey)
initBufferKey flags v = do b <- B.initBuffer flags v
                           k <- registerCleanup $ () <$ releaseObject b
                           return (b,k)

-- | Allocate a new buffer object and write a 'Vector''s contents to
-- it. The buffer is registered for cleanup.
initBuffer :: Storable a => [CLMemFlag] -> V.Vector a -> CL (CLBuffer a)
initBuffer flags v = fst <$> initBufferKey flags v
