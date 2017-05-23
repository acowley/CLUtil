{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, ConstraintKinds,
             ScopedTypeVariables #-}
-- | Functions for working with OpenCL images where pixel formats are
-- represented by "Linear" finite vector types on the Haskell side.
module CLUtil.Image.Linear (
  module CLUtil.Image,
  -- * Initializing images
  initImageFmt, initImage, initImageFmtRes, initImageRes,
  -- * Working with images
  readImage', readImageAsync', readImageAsync, readImage,
  writeImageAsync, writeImage,
  -- * Buffer Image Interop
  copyBufferToImage,
  -- * Working with vectors of pixels
  LinearChan
  ) where
import Control.Applicative ((<$>))
import CLUtil.Async
import CLUtil.Buffer (CLBuffer)
import qualified CLUtil.BufferImageInterop as B
import CLUtil.Image hiding
  (readImage', readImageAsync', readImageAsync, readImage,
   initImageFmt, initImage,
   writeImageAsync, writeImage)
import qualified CLUtil.Image as I
import CLUtil.CL (CL')
import qualified CLUtil.Resource as Res
import Control.Parallel.OpenCL
import qualified Data.Foldable as F
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable)
import Linear (V1, V2, V3, V4)

type family LinearChan (a::NumChan) :: * -> *
type instance LinearChan 'OneChan   = V1
type instance LinearChan 'TwoChan   = V2
type instance LinearChan 'ThreeChan = V3
type instance LinearChan 'FourChan  = V4

-- | Flatten a 'Vector' when the representation of @Vector (t a)@ and
-- @Vecor a@ are the same modulo differences in number of elements.
flatten :: (Storable a, Storable (t a)) => Vector (t a) -> Vector a
flatten = V.unsafeCast

unflatten :: (Storable a, Storable (t a)) => Vector a -> Vector (t a)
unflatten = V.unsafeCast

-- | Initialize a new 2D or 3D image of the given dimensions with a
-- 'Vector' of pixel data. Note that the pixel data is /flattened/
-- across however many channels each pixel may represent. For example,
-- if we have a three channel RGB image with a data type of 'Float',
-- then we expect a 'Vector Float' with a number of elements equal to
-- 3 times the number of pixels. The image resource is registered for
-- cleanup.
initImageFmtRes :: (Integral a, F.Foldable f, Functor f,
                    Storable b, Storable (LinearChan n b),
                    ValidImage n b, Res.CL' s m)
                => [CLMemFlag] -> CLImageFormat -> f a -> Vector (LinearChan n b)
                -> m (CLImage n b)
initImageFmtRes flags fmt dims =
  fmap fst . Res.initImageFmt flags fmt dims . flatten

-- | Initialize a new 2D or 3D image of the given dimensions with a
-- 'Vector' of pixel data. Note that the pixel data is /flattened/
-- across however many channels each pixel may represent. For example,
-- if we have a three channel RGB image with a data type of 'Float',
-- then we expect a 'Vector Float' with a number of elements equal to
-- 3 times the number of pixels.
initImageFmt :: (Integral a, F.Foldable f, Functor f,
                 Storable b, Storable (LinearChan n b),
                 ValidImage n b, CL' m)
             => [CLMemFlag] -> CLImageFormat -> f a -> Vector (LinearChan n b)
             -> m (CLImage n b)
initImageFmt flags fmt dims = I.initImageFmt flags fmt dims . flatten

-- | Initialize an image of the given dimensions with the a 'Vector'
-- of pixel data. A default image format is deduced from the return
-- type. See 'initImage'' for more information on requirements of the
-- input 'Vector'.
initImage :: (Integral a, F.Foldable f, Functor f, ValidImage n b,
              Storable b, Storable (LinearChan n b), CL' m)
          => [CLMemFlag] -> f a -> Vector (LinearChan n b) -> m (CLImage n b)
initImage flags dims = I.initImage flags dims . flatten

-- | Initialize an image of the given dimensions with the a 'Vector'
-- of pixel data. A default image format is deduced from the return
-- type. See 'initImage'' for more information on requirements of the
-- input 'Vector'. The image is registered for cleanup.
initImageRes :: (Integral a, F.Foldable f, Functor f, ValidImage n b,
                 Storable b, Storable (LinearChan n b), Res.CL' s m)
             => [CLMemFlag] -> f a -> Vector (LinearChan n b) -> m (CLImage n b)
initImageRes flags dims = Res.initImage flags dims . flatten


-- | @readImage' mem origin region events@ reads back a 'Vector' of
-- the image @mem@ from coordinate @origin@ of size @region@
-- (i.e. @region ~ (width,height,depth)@) after waiting for @events@
-- to finish. This operation is non-blocking. The resulting 'CLAsync'
-- value includes a 'CLEvent' that must be waited upon before using
-- the result of the read operation. See the
-- "Control.Parallel.CLUtil.Monad.Async" module for utilities for
-- working with asynchronous computations.
readImageAsync' :: (Storable a, Storable (LinearChan n a), ChanSize n, CL' m)
                => CLImage n a -> (Int,Int,Int) -> (Int,Int,Int) -> [CLEvent]
                -> m (CLAsync (Vector (LinearChan n a)))
readImageAsync' img origin region waitForIt =
  fmap unflatten <$> I.readImageAsync' img origin region waitForIt

-- | @readImage' mem origin region events@ reads back a 'Vector' of
-- the image @mem@ from coordinate @origin@ of size @region@
-- (i.e. @region ~ (width,height,depth)@) after waiting for @events@
-- to finish. This operation blocks until the operation is complete.
readImage' :: (Storable a, ChanSize n, Storable (LinearChan n a), CL' m)
          => CLImage n a -> (Int,Int,Int) -> (Int,Int,Int) -> [CLEvent]
          -> m (Vector (LinearChan n a))
readImage' img origin region waitForIt =
  unflatten <$> I.readImage' img origin region waitForIt

-- | Read the entire contents of an image into a 'Vector'. This
-- operation blocks until the read is complete.
readImage :: (Storable a, Storable (LinearChan n a), ChanSize n, CL' m)
          => CLImage n a -> m (Vector (LinearChan n a))
readImage img@(CLImage dims _) = readImage' img (0,0,0) dims []

-- | Non-blocking complete image read. The resulting 'CLAsync' value
-- includes a 'CLEvent' that must be waited upon before using the
-- result of the read operation. See the
-- "Control.Parallel.CLUtil.Monad.Async" module for utilities for
-- working with asynchronous computations.
readImageAsync :: (Storable a, Storable (LinearChan n a), ChanSize n, CL' m)
                => CLImage n a -> m (CLAsync (Vector (LinearChan n a)))
readImageAsync img@(CLImage dims _) = readImageAsync' img (0,0,0) dims []

-- | Write a 'Vector''s contents to a 2D or 3D image. The 'Vector'
-- must be the same size as the target image. NOTE: Multi-dimensional
-- pixels must be unpacked into a flat array. This means that, if you
-- want to upload RGBA pixels to a 2D image, you must provide a
-- 'Vector CFloat' of length @4 * imageWidth * imageHeight@.
writeImageAsync :: (Storable a, Storable (LinearChan n a), ChanSize n, CL' m)
                => CLImage n a -> Vector (LinearChan n a) -> Blockers
                -> m (CLAsync ())
writeImageAsync img = I.writeImageAsync img . flatten

-- | Perform a blocking write of a 'Vector''s contents to an
-- image. See 'writeImageAsync' for more information.
writeImage :: (Storable a, Storable (LinearChan n a), ChanSize n, CL' m)
           => CLImage n a -> Vector (LinearChan n a) -> m ()
writeImage img = I.writeImage img . flatten

-- | Copy a buffer to an image where each buffere element is a vector
-- that maps into a multi-channel pixel type.
copyBufferToImage :: forall n a m. CL' m
                  => CLBuffer (LinearChan n a) -> CLImage n a -> m ()
copyBufferToImage buf (CLImage dims obj) =
  B.copyBufferToImage buf (CLImage dims obj :: CLImage1 a)
