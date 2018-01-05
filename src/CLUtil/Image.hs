{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures,
             PolyKinds, ScopedTypeVariables, RankNTypes,
             GeneralizedNewtypeDeriving, EmptyDataDecls #-}
-- | Typed monadic interface for working with OpenCL images.
module CLUtil.Image (
  -- * Image types
  CLImage(..), NumChan(..), CLImage1, CLImage2, CLImage3, CLImage4,
  ChanSize(..), ChanCompatible(..), ValidImage,
  HalfFloat, NormInt8(..), NormWord8(..), NormInt16(..), NormWord16(..),

  -- * Creating images
  allocImage, allocImageFmt, initImage, initImageFmt, defaultFormat,

  -- * Working with images
  readImage', readImage, readImageAsync', readImageAsync,
  writeImage, writeImageAsync,
  copyImageAsync, copyImage, imageCompatible,

    -- * Efficient access to image contents
  withImage, withImageAsync, withImageRW, withImageRWAsync
  ) where
import Control.Applicative ((<$>))
import Control.Exception (evaluate)
import Control.Monad (when)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Int (Int8, Int16, Int32)
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word (Word8, Word16, Word32)
import Foreign.C.Types (CFloat, CInt)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import CLUtil.Async
import CLUtil.CL
import CLUtil.State (clContext, clQueue)
import Control.Parallel.OpenCL

-- | The number of channels for image types.
data NumChan = OneChan | TwoChan | ThreeChan | FourChan

-- | An uninhabited type that corresponds to OpenHasCLs @CL_HALF_FLOAT@
-- type.
data HalfFloat

instance Storable HalfFloat where
  sizeOf _ = error "HalfFloat is uninhabited"
  alignment _ = error "HalfFloat is uninhabited"
  peek _ = error "HalfFloat is uninhabited"
  poke _ _ = error "HalfFloat is uninhabited"

-- | A type corresponding to OpenHasCLs normalized signed 8-bit
-- integer. Values of this type are represented in Haskell as 'Int8',
-- but in an OpenCL kernel will take on values between zero and one.
newtype NormInt8 = NormInt8 Int8
  deriving (Num, Show, Real, Integral, Eq, Enum, Bounded, Ord, Storable)

-- | A type corresponding to OpenHasCLs normalized unsigned 8-bit
-- integer. Values of this type are represented in Haskell as 'Word8',
-- but in an OpenCL kernel will take on values between zero and one.
newtype NormWord8 = NormWord8 Word8
  deriving (Num, Show, Real, Integral, Eq, Enum, Bounded, Ord, Storable)

-- | A type corresponding to OpenHasCLs normalized signed 16-bit
-- integer. Values of this type are represented in Haskell as 'Int16',
-- but in an OpenCL kernel will take on values between zero and one.
newtype NormInt16 = NormInt16 Int16
  deriving (Num, Show, Real, Integral, Eq, Enum, Bounded, Ord, Storable)

-- | A type corresponding to OpenHasCLs normalized unsigned 16-bit
-- integer. Values of this type are represented in Haskell as
-- 'Word16', but in an OpenCL kernel will take on values between zero
-- and one.
newtype NormWord16 = NormWord16 Word16
  deriving (Num, Show, Real, Integral, Eq, Enum, Bounded, Ord, Storable)

-- |A @CLImage n a@ is an image with @n@ channels whose every
-- component is of type @a@.
data CLImage (n::NumChan) a = CLImage { imageDims   :: (Int,Int,Int)
                                      , imageObject :: CLMem }

instance HasCLMem (CLImage n a) where
  getCLMem (CLImage _ m) = m

-- | A 'CLImage' with one channel per pixel.
type CLImage1 = CLImage 'OneChan

-- | A 'CLImage' with two channels per pixel.
type CLImage2 = CLImage 'TwoChan

-- | A 'CLImage' with three channels per pixel.
type CLImage3 = CLImage 'ThreeChan

-- | A 'CLImage' with four channels per pixel.
type CLImage4 = CLImage 'FourChan

-- Kind-polymorphic proxy to pass types around.
-- data Proxy a = Proxy

-- | Predicate to determine if a 'NumChan' is compatible with a
-- 'CLChannelOrder'. 'NumChan' says nothing about the semantics of the
-- channels, only the cardinality, so a single 'NumChan' may be
-- compatible with several 'CLChannelOrder' variants. That said, we
-- also provide a default 'CLChannelOrder' for each 'NumChan' to
-- facilitate the creation of default image formats.
class ChanCompatible (a::NumChan) where
  chanCompatible :: Proxy a -> CLChannelOrder -> Bool
  defaultChan :: Proxy a -> CLChannelOrder

instance ChanCompatible 'OneChan where
  chanCompatible _ CL_R         = True
  chanCompatible _ CL_A         = True
  chanCompatible _ CL_INTENSITY = True
  chanCompatible _ CL_LUMINANCE = True
  chanCompatible _ _            = False
  defaultChan _ = CL_R

instance ChanCompatible 'TwoChan where
  chanCompatible _ CL_RG = True
  chanCompatible _ CL_RA = True
  chanCompatible _ _     = False
  defaultChan _ = CL_RG

instance ChanCompatible 'ThreeChan where
  chanCompatible _ CL_RGB = True
  chanCompatible _ _      = False
  defaultChan _ = CL_RGB

instance ChanCompatible 'FourChan where
  chanCompatible _ CL_RGBA = True
  chanCompatible _ CL_ARGB = True
  chanCompatible _ CL_BGRA = True
  chanCompatible _ _       = False
  defaultChan _ = CL_RGBA

-- | A mapping from Haskell types to 'CLChannelType' variants.
class TypeCompatible a where
  typeCompatible :: Proxy a -> CLChannelType

instance TypeCompatible Int8 where
  typeCompatible _  = CL_SIGNED_INT8

instance TypeCompatible Word8 where
  typeCompatible _ = CL_UNSIGNED_INT8

instance TypeCompatible Int16 where
  typeCompatible _ = CL_SIGNED_INT16

instance TypeCompatible Word16 where
  typeCompatible _ = CL_UNSIGNED_INT16

instance TypeCompatible Int32 where
  typeCompatible _ = CL_SIGNED_INT32

instance TypeCompatible CInt where
  typeCompatible _ = CL_SIGNED_INT32

instance TypeCompatible Word32 where
  typeCompatible _ = CL_UNSIGNED_INT32

instance TypeCompatible Float where
  typeCompatible _ = CL_FLOAT

instance TypeCompatible CFloat where
  typeCompatible _ = CL_FLOAT

instance TypeCompatible HalfFloat where
  typeCompatible _ = CL_HALF_FLOAT

instance TypeCompatible NormInt8 where
  typeCompatible _ = CL_SNORM_INT8

instance TypeCompatible NormWord8 where
  typeCompatible _ = CL_UNORM_INT8

instance TypeCompatible NormInt16 where
  typeCompatible _ = CL_SNORM_INT16

instance TypeCompatible NormWord16 where
  typeCompatible _ = CL_UNORM_INT16

-- | A mapping from 'NumChan' variants used as types to value-level
-- integers.
class ChanSize (a::NumChan) where
  numChan :: Proxy a -> Int

instance ChanSize 'OneChan   where numChan _ = 1
instance ChanSize 'TwoChan   where numChan _ = 2
instance ChanSize 'ThreeChan where numChan _ = 3
instance ChanSize 'FourChan  where numChan _ = 4

-- | NOTE: This is an EVIL 'Storable' instance that lets us treat a
-- 'CLImage' as its underlying 'CLMem' value for the sake of
-- interoperating with OpenCL. The 'Storable' instance does /not/ let
-- you roundtrip a value using 'peek' and 'poke'.
instance Storable (CLImage n a) where
  sizeOf _ = sizeOf (undefined::CLMem)
  alignment _ = alignment (undefined::CLMem)
  peek = fmap (CLImage (error "Tried to peek a CLImage")) . peek . castPtr
  poke ptr (CLImage _ m) = poke (castPtr ptr) m

-- | Compute a default 'CLImageFormat' for a given 'CLImage' type.
defaultFormat :: forall n b. (ValidImage n b)
              => Proxy (CLImage n b) -> CLImageFormat
defaultFormat _ = CLImageFormat (defaultChan (Proxy::Proxy n))
                                (typeCompatible (Proxy::Proxy b))

-- | Raise an error in if a 'CLImageFormat' is not compatible with a
-- 'CLImage' type.
imageCompatible :: forall n b m. (ValidImage n b, HasCL m)
                => CLImageFormat -> Proxy (CLImage n b) -> m ()
imageCompatible (CLImageFormat order dtype) _
  | not (chanCompatible (Proxy::Proxy n) order) =
      throwError $ "Image format specifies channels "++
                   show order++
                   ", which is incompatible with the CLImage channel count."
  | fromEnum (typeCompatible (Proxy::Proxy b)) /= fromEnum dtype =
      throwError $ "Image channel data type "++show dtype++
                   " is incompatible with the CLImage channel data type."
  | otherwise = return ()

-- | Common constraint for 'CLImage' type parameters.
type ValidImage n b = (ChanCompatible n, ChanSize n, TypeCompatible b)

-- | Allocate a new 2D or 3D image of the given dimensions and
-- format. The image is /not/ registered for cleanup.
allocImageFmt :: forall a f n b m.
                 (Integral a, Foldable f, Functor f, ValidImage n b, HasCL m)
              => [CLMemFlag] -> CLImageFormat -> f a -> m (CLImage n b)
allocImageFmt flags fmt dims =
  do imageCompatible fmt (Proxy::Proxy (CLImage n b))
     c <- clContext <$> ask
     case F.toList (fromIntegral <$> dims) of
       [w,h]   -> fmap (CLImage (w,h,1)) . liftIO $
                    clCreateImage2D c flags fmt w h 0 nullPtr
       [w,h,d] -> fmap (CLImage (w,h,d)) . liftIO $
                    clCreateImage3D c flags fmt w h d 0 0 nullPtr
       _       -> throwError "Only 2D and 3D images are currently supported!"

-- | Allocate a new 2D or 3D image of the given dimensions. The image
-- format is the default for the the return type (e.g. the type
-- 'CLImage OneChan Float' is associated with a default format of
-- 'CLImageFormat CL_R CL_FLOAT') . The image is registered for
-- cleanup, and the key used to perform an early cleanup of the image
-- is returned.
allocImage :: forall f a n b m.
              (Integral a, Foldable f, Functor f, ValidImage n b, HasCL m)
           => [CLMemFlag] -> f a -> m (CLImage n b)
allocImage flags = allocImageFmt flags fmt
  where fmt = defaultFormat (Proxy::Proxy (CLImage n b))

-- | Initialize a new 2D or 3D image of the given dimensions with a
-- 'Vector' of pixel data. Note that the pixel data is /flattened/
-- across however many channels each pixel may represent. For example,
-- if we have a three channel RGB image with a data type of 'Float',
-- then we expect a 'Vector Float' with a number of elements equal to
-- 3 times the number of pixels.
initImageFmt :: forall a f n b m.
                (Integral a, Foldable f, Functor f, Storable b, ValidImage n b,
                 HasCL m)
             => [CLMemFlag] -> CLImageFormat -> f a -> V.Vector b
             -> m (CLImage n b)
initImageFmt flags fmt dims v =
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

-- | Initialize an image of the given dimensions with the a 'Vector'
-- of pixel data. A default image format is deduced from the return
-- type. See 'initImage'' for more information on requirements of the
-- input 'Vector'.
initImage :: forall a f n b m.
             (Integral a, Foldable f, Functor f, ValidImage n b, Storable b,
              HasCL m)
          => [CLMemFlag] -> f a -> V.Vector b -> m (CLImage n b)
initImage flags = initImageFmt flags fmt
  where fmt = defaultFormat (Proxy::Proxy (CLImage n b))

-- | Write a 'Vector''s contents to a 2D or 3D image. The 'Vector'
-- must be the same size as the target image. NOTE: Multi-dimensional
-- pixels must be unpacked into a flat array. This means that, if you
-- want to upload RGBA pixels to a 2D image, you must provide a
-- 'Vector CFloat' of length @4 * imageWidth * imageHeight@.
writeImageAsync :: forall n a m. (Storable a, ChanSize n, HasCL m)
                => CLImage n a -> V.Vector a -> Blockers -> m (CLAsync ())
writeImageAsync (CLImage dims@(w,h,d) mem) v bs =
  do q <- clQueue <$> ask
     when (w*h*d*numChan (Proxy::Proxy n) /= V.length v)
          (throwError "Vector length is not equal to image dimensions!")
     ev <- liftIO . V.unsafeWith v $ \ptr ->
             clEnqueueWriteImage q mem True (0,0,0) dims 0 0 (castPtr ptr)
                                 (getBlockers bs)
     return . clAsync ev $ return ()

-- | Perform a blocking write of a 'Vector''s contents to an
-- image. See 'writeImageAsync' for more information.
writeImage :: forall n a m. (Storable a, ChanSize n, HasCL m)
           => CLImage n a -> V.Vector a -> m ()
--writeImage img v = writeImageAsync img v >>= waitOne
writeImage (CLImage dims@(w,h,d) mem) v =
  do q <- clQueue <$> ask
     when (w*h*d*numChan (Proxy::Proxy n) /= V.length v)
          (throwError "Vector length is not equal to image dimensions!")
     liftIO . V.unsafeWith v $ \ptr -> do
       ev <- clEnqueueWriteImage q mem True (0,0,0) dims 0 0 (castPtr ptr) []
       _ <- clWaitForEvents [ev] >> clReleaseEvent ev
       return ()

tripZipAll :: (a -> a -> Bool) -> (a,a,a) -> (a,a,a) -> Bool
tripZipAll = ((tripAll id .) .) . tripZip

tripZip :: (a -> a -> b) -> (a,a,a) -> (a,a,a) -> (b,b,b)
tripZip f (x1,x2,x3) (y1,y2,y3) = (f x1 y1, f x2 y2, f x3 y3)

tripAll :: (a -> Bool) -> (a,a,a) -> Bool
tripAll f (x,y,z) = f x && f y && f z

-- | @readImage' mem origin region events@ reads back a 'Vector' of
-- the image @mem@ from coordinate @origin@ of size @region@
-- (i.e. @region ~ (width,height,depth)@) after waiting for @events@
-- to finish. This operation is non-blocking. The resulting 'CLAsync'
-- value includes a 'CLEvent' that must be waited upon before using
-- the result of the read operation. See the
-- "Control.Parallel.CLUtil.Monad.Async" module for utilities for
-- working with asynchronous computations.
readImageAsync' :: forall n a m. (Storable a, ChanSize n, HasCL m)
                => CLImage n a -> (Int,Int,Int) -> (Int,Int,Int) -> [CLEvent]
                -> m (CLAsync (V.Vector a))
readImageAsync' (CLImage dims@(w,h,d) mem) origin region waitForIt =
  do when (not $ tripAll (>0) region)
          (throwError "Each dimension of requested region must be positive!")
     when (not $ tripAll (>=0) origin)
          (throwError "Each dimension of requested origin must be nonnegative!")
     when (not $ tripZipAll (<=) (tripZip (+) origin region) dims)
          (throwError "Requested region extends oustide the image!")
     q <- clQueue <$> ask
     v <- liftIO $ VM.new n
     ev <- liftIO . VM.unsafeWith v $ \ptr ->
             clEnqueueReadImage q mem True origin region 0 0
                                (castPtr ptr) waitForIt
     return . clAsync ev $ liftIO $ V.unsafeFreeze v
  where n = fromIntegral $ w*h*d*numChan (Proxy::Proxy n)

-- | @readImage' mem origin region events@ reads back a 'Vector' of
-- the image @mem@ from coordinate @origin@ of size @region@
-- (i.e. @region ~ (width,height,depth)@) after waiting for @events@
-- to finish. This operation blocks until the operation is complete.
readImage' :: (Storable a, ChanSize n, HasCL m)
           => CLImage n a -> (Int,Int,Int) -> (Int,Int,Int) -> [CLEvent]
           -> m (V.Vector a)
readImage' img origin region waitForIt =
  readImageAsync' img origin region waitForIt >>= waitOne

-- | Read the entire contents of an image into a 'Vector'. This
-- operation blocks until the read is complete.
readImage :: (Storable a, ChanSize n, HasCL m) => CLImage n a -> m (V.Vector a)
readImage img@(CLImage dims _) = readImage' img (0,0,0) dims []

-- | Non-blocking complete image read. The resulting 'CLAsync' value
-- includes a 'CLEvent' that must be waited upon before using the
-- result of the read operation. See the
-- "Control.Parallel.CLUtil.Monad.Async" module for utilities for
-- working with asynchronous computations.
readImageAsync :: (Storable a, ChanSize n, HasCL m)
               => CLImage n a -> Blockers -> m (CLAsync (V.Vector a))
readImageAsync img@(CLImage dims _) =
  readImageAsync' img (0,0,0) dims . getBlockers

-- | Provides access to a memory-mapped 'VM.MVector' of a
-- 'CLImage'. The result of applying the given function to the vector
-- is evaluated to WHNF, but the caller should ensure that this is
-- sufficient to not require hanging onto a reference to the vector
-- data, as this reference will not be valid. Returning the vector
-- itself is right out. The 'CLMapFlag's supplied determine if we have
-- read-only, write-only, or read/write access to the 'VM.MVector'.
withImageAsync_ :: forall n a r m. (ChanSize n, Storable a, HasCL m)
                => [CLMapFlag] -> CLImage n a
                -> (forall s. VM.MVector s a -> ST s r) -> m (CLAsync r)
withImageAsync_ flags (CLImage dims mem) f =
  do q <- clQueue <$> ask
     liftIO $ do
       (ev, (ptr, _pitch, sz)) <- clEnqueueMapImage q mem False flags
                                                    (0,0,0) dims []
       let go = do fp <- newForeignPtr_ $ castPtr ptr
                   x <- evaluate =<<
                        (unsafeSTToIO
                         $ f . VM.unsafeFromForeignPtr0 fp
                         $ fromIntegral sz `quot` sizeOf (undefined::a))
                   ev' <- clEnqueueUnmapMemObject q mem ptr []
                   _ <- clWaitForEvents [ev'] >> clReleaseEvent ev'
                   return x
       return . clAsync ev $ liftIO go

-- | Provides read/write access to a memory-mapped 'VM.MVector' of a
-- 'CLImage'. The caller should ensure that this is sufficient to not
-- require hanging onto a reference to the vector data, as this
-- reference will not be valid. Returning the vector itself is right
-- out.
withImageRWAsync :: (ChanSize n, Storable a, HasCL m)
                 => CLImage n a -> (forall s. VM.MVector s a -> ST s r)
                 -> m (CLAsync r)
withImageRWAsync = withImageAsync_ [CL_MAP_READ, CL_MAP_WRITE]

-- | Provides read/write access to a memory-mapped 'VM.MVector' of a
-- 'CLImage'. The caller should ensure that this is sufficient to not
-- require hanging onto a reference to the vector data, as this
-- reference will not be valid. Returning the vector itself is right
-- out.
withImageRW :: (ChanSize n, Storable a)
            => CLImage n a -> (forall s. VM.MVector s a -> ST s r) -> CL r
withImageRW img f = withImageRWAsync img f >>= waitOne

-- | Provides read-only access to a memory-mapped 'V.Vector' of a
-- 'CLImage'. The result of applying the given function to the vector
-- is evaluated to WHNF, but the caller should ensure that this is
-- sufficient to not require hanging onto a reference to the vector
-- data, as this reference will not be valid. Returning the vector
-- itself is right out.
withImageAsync :: (ChanSize n, Storable a, HasCL m)
               => CLImage n a -> (V.Vector a -> r) -> m (CLAsync r)
withImageAsync img f =
  withImageAsync_ [CL_MAP_READ] img (fmap f . V.unsafeFreeze)

-- | Provides read/write access to a memory-mapped 'V.Vector' of a
-- 'CLImage'. The result of applying the given function to the vector
-- is evaluated to WHNF, but the caller should ensure that this is
-- sufficient to not require hanging onto a reference to the vector
-- data, as this reference will not be valid. Returning the vector
-- itself is right out.
withImage :: (ChanSize n, Storable a, HasCL m)
          => CLImage n a -> (V.Vector a -> r) -> m r
withImage img f = withImageAsync img f >>= waitOne
-- FIXME: Add some error checking to copyImageAsync to make sure the
-- origins make sense given the image dimensions, and that the region
-- makes sense with everything else.

-- | @copyImageAsync src dst srcOrigin dstOrigin region@ enqueues an
-- asynchronous copy from @src@ to @dst@.
copyImageAsync :: HasCL m
               => CLImage n a -> CLImage n a
               -> (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
               -> m (CLAsync ())
copyImageAsync src dst srcOrigin dstOrigin region =
  do q <- clQueue <$> ask
     ev <- liftIO $ clEnqueueCopyImage q srcMem dstMem srcOrigin dstOrigin region []
     return . clAsync ev $ return ()
  where CLImage _srcDim srcMem = src
        CLImage _dstDim dstMem = dst

-- | @copyImage src dst@ copies from 'CLImage' @src@ to @dst@.
copyImage :: HasCL m => CLImage n a -> CLImage n a -> m ()
copyImage src@(CLImage sdims _) dst@(CLImage ddims _)
  | sdims /= ddims = throwError "copyImage's arguments must be the same size"
  | otherwise = copyImageAsync src dst (0,0,0) (0,0,0) sdims >>= waitOne
