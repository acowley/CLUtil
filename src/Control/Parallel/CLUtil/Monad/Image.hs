{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures, PolyKinds, 
             ScopedTypeVariables #-}
-- | Typed monadic interface for working with OpenCL images.
module Control.Parallel.CLUtil.Monad.Image where
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Int (Int8, Int16, Int32)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word (Word8, Word16, Word32)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Control.Parallel.CLUtil
import Control.Parallel.CLUtil.Monad.CL

-- | The number of channels for image types.
data NumChan = OneChan | TwoChan | ThreeChan | FourChan

-- |A @CLImage n a@ is an image with @n@ channels whose every
-- component is of type @a@.
data CLImage (n::NumChan) a = CLImage { imageDims   :: (Int,Int,Int)
                                      , imageObject :: CLMem }

-- Kind-polymorphic proxy to pass types around.
data Proxy a = Proxy

-- | Predicate to determine if a 'NumChan' is compatible with a
-- 'CLChannelOrder'. 'NumChan' says nothing about the semantics of the
-- channels, only the cardinality, so a single 'NumChan' may be
-- compatible with several 'CLChannelOrder' variants. That said, we
-- also provide a default 'CLChannelOrder' for each 'NumChan' to
-- facilitate the creation of default image formats.
class ChanCompatible (a::NumChan) where
  chanCompatible :: Proxy a -> CLChannelOrder -> Bool
  defaultChan :: Proxy a -> CLChannelOrder

instance ChanCompatible OneChan where
  chanCompatible _ CL_R         = True
  chanCompatible _ CL_A         = True
  chanCompatible _ CL_INTENSITY = True
  chanCompatible _ CL_LUMINANCE = True
  chanCompatible _ _            = False
  defaultChan _ = CL_R

instance ChanCompatible TwoChan where
  chanCompatible _ CL_RG = True
  chanCompatible _ CL_RA = True
  chanCompatible _ _     = False
  defaultChan _ = CL_RG

instance ChanCompatible ThreeChan where
  chanCompatible _ CL_RGB = True
  chanCompatible _ _      = False
  defaultChan _ = CL_RGB

instance ChanCompatible FourChan where
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

-- | A mapping from 'NumChan' variants used as types to value-level
-- integers.
class ChanSize (a::NumChan) where
  numChan :: Proxy a -> Int

instance ChanSize OneChan   where numChan _ = 1
instance ChanSize TwoChan   where numChan _ = 2
instance ChanSize ThreeChan where numChan _ = 3
instance ChanSize FourChan  where numChan _ = 4

-- | NOTE: This is an EVIL 'Storable' instance that lets us treat a
-- 'CLImage' as its underlying 'CLMem' value for the sake of
-- interoperating with OpenCL. The 'Storable' instance does /not/ let
-- you roundtrip a value using 'peek' and 'poke'.
instance Storable (CLImage n a) where
  sizeOf _ = sizeOf (undefined::CLMem)
  alignment _ = alignment (undefined::CLMem)
  peek = fmap (CLImage (0,0,0)) . peek . castPtr
  poke ptr (CLImage _ m) = poke (castPtr ptr) m


-- | Compute a default 'CLImageFormat' for a given 'CLImage' type.
defaultFormat :: forall n b. (ValidImage n b)
              => Proxy (CLImage n b) -> CLImageFormat
defaultFormat _ = CLImageFormat (defaultChan (Proxy::Proxy n)) 
                                (typeCompatible (Proxy::Proxy b))

-- | Raise an error in if a 'CLImageFormat' is not compatible with a
-- 'CLImage' type.
imageCompatible :: forall n b. (ValidImage n b)
                => CLImageFormat -> Proxy (CLImage n b) -> CL ()
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
type ValidImage n b = (ChanCompatible n, TypeCompatible b)

-- | Allocate a new 2D or 3D image of the given dimensions.
allocImage' :: forall a f n b.
               (Integral a, Foldable f, Functor f, ValidImage n b)
            => [CLMemFlag] -> CLImageFormat -> f a -> CL (CLImage n b)
allocImage' flags fmt dims =
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
-- 'CLImageFormat CL_R CL_FLOAT') .
allocImage :: forall f a n b. 
              (Integral a, Foldable f, Functor f, ValidImage n b)
           => [CLMemFlag] -> f a -> CL (CLImage n b)
allocImage flags = allocImage' flags fmt
  where fmt = defaultFormat (Proxy::Proxy (CLImage n b))

-- | Write a 'Vector''s contents to a 2D or 3D image.
writeImage :: forall n a. Storable a => CLImage n a -> Vector a -> CL ()
writeImage (CLImage dims@(w,h,d) mem) v = 
  do q <- clQueue <$> ask
     when (w*h*d /= V.length v)
          (throwError "Vector length is not equal to image dimensions!")
     ev <- liftIO . V.unsafeWith v $ \ptr ->
             clEnqueueWriteImage q mem True (0,0,0) dims 0 0 (castPtr ptr) []
     when (ev /= nullPtr)
          (do okay "wait for event" $ clWaitForEvents [ev]
              okay "release event" $ clReleaseEvent ev)

tripZipAll :: (a -> a -> Bool) -> (a,a,a) -> (a,a,a) -> Bool
tripZipAll = ((tripAll id .) .) . tripZip

tripZip :: (a -> a -> b) -> (a,a,a) -> (a,a,a) -> (b,b,b)
tripZip f (x1,x2,x3) (y1,y2,y3) = (f x1 y1, f x2 y2, f x3 y3)

tripAll :: (a -> Bool) -> (a,a,a) -> Bool
tripAll f (x,y,z) = f x && f y && f z

-- | @readImage' mem origin region events@ reads back a 'Vector' of
-- the image @mem@ from coordinate @origin@ of size @region@
-- (i.e. @region ~ (width,height,depth)@) after waiting for @events@
-- to finish.
readImage' :: forall n a. (Storable a, ChanSize n)
           => CLImage n a -> (Int,Int,Int) -> (Int,Int,Int) -> [CLEvent]
           -> CL (Vector a)
readImage' (CLImage dims@(w,h,d) mem) origin region waitForIt =
  do when (not $ tripAll (>0) region)
          (throwError "Each dimension of requested region must be positive!")
     when (not $ tripAll (>=0) origin)
          (throwError "Each dimension of requested origin must be nonnegative!")
     when (not $ tripZipAll (<) (tripZip (+) origin region) dims)
          (throwError "Requested region extends oustide the image!")
     q <- clQueue <$> ask
     v <- liftIO $ VM.new n
     ev <- liftIO . VM.unsafeWith v $ \ptr ->
             clEnqueueReadImage q mem True origin region 0 0
                                (castPtr ptr) waitForIt
     when (ev /= nullPtr)
          (do okay "wait for event" $ clWaitForEvents [ev]
              okay "release event" $ clReleaseEvent ev)
     liftIO $ V.unsafeFreeze v
  where n = fromIntegral $ w*h*d*numChan (Proxy::Proxy n)

-- | Read the entire contents of an image into a 'Vector'.
readImage :: (Storable a, ChanSize n) => CLImage n a -> CL (Vector a)
readImage img@(CLImage dims _) = readImage' img (0,0,0) dims []
