-- Port of the quasicrystal animation code at:
-- http://mainisusuallyafunction.blogspot.com/2011/10/quasicrystals-as-sums-of-waves-in-plane.html
-- This module displays the animation in a gloss window.
import System.GPU.CLUtil
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Graphics.Gloss hiding (Vector, scale)
import System.IO.Unsafe

pixels :: Int
pixels = 800

scale :: Float
scale = 128

angles :: Int -> Vector Float
angles n = V.map (* (pi / fromIntegral n)) $ V.enumFromN 0 n

mkPicture :: Vector Word8 -> Picture
mkPicture = flip (bitmapOfForeignPtr pixels pixels) False 
          . (\(x,_,_) -> x)
          . V.unsafeToForeignPtr

main = do s <- ezInit CL_DEVICE_TYPE_ALL
          k <- kernelFromFile s "QuasiCrystalRGBA.cl" "quasiCrystal"
          let numPix = pixels * pixels
              pixels' = fromIntegral pixels :: CInt
              numAngles = 7
              allAngles = angles numAngles
              frame :: Float -> Picture
              frame phase = unsafePerformIO $
                            mkPicture `fmap`
                            runKernel s k pixels' scale phase
                                      numAngles allAngles
                                      (Out (numPix*4)) (Work2D pixels pixels)
              {-# NOINLINE frame #-}
          -- gloss 1.6 API
          -- animate (InWindow "Quasicrystal" (pixels,pixels) (0,0)) black frame
          -- gloss 1.5 API
          animateInWindow "Quasicrystal" (pixels,pixels) (0,0) black frame
