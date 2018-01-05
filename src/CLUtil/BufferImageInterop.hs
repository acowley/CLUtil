{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
-- | Utilities for working with 'CLBuffer's and 'CLImage's together.
module CLUtil.BufferImageInterop where
import CLUtil.State (clQueue)
import CLUtil.Buffer (CLBuffer(..))
import CLUtil.CL (HasCL, ask, throwError, liftIO)
import CLUtil.Image (CLImage(..), CLImage1)
import CLUtil.Async
import Control.Parallel.OpenCL

-- | Copy the contents of a 'CLBuffer' to a 'CLImage1' of the same
-- element type. An error is raised if the 'CLBuffer''s size is
-- different than the total size of the 'CLImage1'. Returns a
-- 'CLEvent' that may be waited upon for the copy operation to finish.
copyBufferToImageAsync :: HasCL m => CLBuffer a -> CLImage1 b -> m (CLAsync ())
copyBufferToImageAsync (CLBuffer bufLen bufObj) (CLImage imgDims imgObj)
  | bufLen /= dimProd = throwError "Buffer is not the same size as image"
  | otherwise = flip clAsync (return ()) `fmap`
    do q <- fmap clQueue ask
       liftIO $ clEnqueueCopyBufferToImage q bufObj imgObj 0 (0,0,0) imgDims []
  where dimProd = let (w,h,d) = imgDims in w*h*d

-- | Copy the contents of a 'CLBuffer' to a 'CLImage1' of the same
-- element type. An error is raised if the 'CLBuffer''s size is
-- different than the total size of the 'CLImage1'. Blocks until the
-- copy operation is complete.
copyBufferToImage :: HasCL m => CLBuffer a -> CLImage1 b -> m ()
copyBufferToImage buf img = copyBufferToImageAsync buf img >>= waitOne

-- | Copy the contents of a 'CLImage' to a 'CLBuffer' of the same
-- element type. An error is raised if the 'CLBuffer''s size is
-- different than the total size of the 'CLImage1'. Returns a
-- 'CLEvent' that may be waited upon for the copy operation to finish.
copyImageToBufferAsync :: HasCL m => CLImage1 a -> CLBuffer a -> m (CLAsync ())
copyImageToBufferAsync (CLImage imgDims imgObj) (CLBuffer bufLen bufObj)
  | dimProd /= bufLen = throwError "Buffer is not the same size as image"
  | otherwise = flip clAsync (return ()) `fmap`
    do q <- fmap clQueue ask
       liftIO $ clEnqueueCopyImageToBuffer q imgObj bufObj (0,0,0) imgDims 0 []
  where dimProd = let (w,h,d) = imgDims in w*h*d

-- | Copy the contents of a 'CLImage' to a 'CLBuffer' of the same
-- element type. An error is raised if the 'CLBuffer''s size is
-- different than the total size of the 'CLImage1'. Blocks until the
-- copy operation is complete.
copyImageToBuffer :: HasCL m => CLImage1 a -> CLBuffer a -> m ()
copyImageToBuffer img buf = copyImageToBufferAsync img buf >>= waitOne
