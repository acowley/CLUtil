{-# LANGUAGE ConstraintKinds #-}
-- | Helpers for working with scratch memory. See 'scratchImage' for a
-- discussion of applicability.
module Control.Parallel.CLUtil.Scratch where
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Parallel.CLUtil.CL (CL, runCleanup)
import Control.Parallel.CLUtil.Image
import Control.Parallel.OpenCL
import Data.IORef
import Data.Traversable (sequenceA)
import Linear (V2(..))

-- | One often wants to present a pseudo-functional interface such
-- that inputs are not modified. Rather than create a new output value
-- from whole cloth, it is much more efficient to re-use a previously
-- allocated output value. This means that anybody holding onto a
-- reference to a previously computed value will have their data
-- changed from under their feet, so care must be taken to avoid such
-- an eventuality. The result of @scratchImage@ is a function that
-- returns a potentially new 'CLImage' of identical dimensions to the
-- input image. Calling this function again with an input image of the
-- same size will return the /same/ no-longer-new 'CLImage'. If the
-- 'CLImage' provided as input to the returned function is of a
-- different size to the previous invocation, then the old output
-- 'CLImage' is released, and a new one is allocated.
scratchImage :: ValidImage d a => IO (CLImage d a -> CL (CLImage d a))
scratchImage = aux <$> newIORef Nothing
  where aux r img =
          let (w,h,_) = imageDims img
              fresh = do x@(img',_) <- allocImageKey [CL_MEM_READ_WRITE] [w,h]
                         liftIO . writeIORef r $ Just x
                         return img'
          in liftIO (readIORef r) >>= \r' -> case r' of
               Nothing -> fresh
               Just (img',k)
                 | imageDims img' == imageDims img -> return img'
                 | otherwise -> runCleanup k >> fresh

-- | Similar to 'scratchImage' but returns /two/ scratch images. This
-- is helpful if a processing stage is implemented in two passes
-- that bounce back and forth (ping-pong).
pingPongImage :: ValidImage d a => IO (CLImage d a -> CL (V2 (CLImage d a)))
pingPongImage = do p1 <- scratchImage
                   p2 <- scratchImage
                   return $ (sequenceA .) . V2 <$> p1 <*> p2
