{-# LANGUAGE ScopedTypeVariables #-}
-- | Utilities for working with 'CLBuffer's and 'CLImage's together.
module Control.Parallel.CLUtil.Monad.BufferImageInterop where
import Control.Parallel.CLUtil.State (clQueue)
import Control.Parallel.CLUtil.Monad.Buffer (CLBuffer(..))
import Control.Parallel.CLUtil.Monad.CL (CL, ask, throwError, liftIO)
import Control.Parallel.CLUtil.Monad.Image (CLImage(..), CLImage1)
import Control.Parallel.OpenCL

-- | Copy the contents of a 'CLBuffer' to a 'CLImage1' of the same
-- element type. An error is raised if the 'CLBuffer''s size is
-- different than the total size of the 'CLImage1'. Returns a
-- 'CLEvent' that may be waited upon for the copy operation to finish.
copyBufferToImageAsync :: forall a. CLBuffer a -> CLImage1 a -> CL CLEvent
copyBufferToImageAsync (CLBuffer bufLen bufObj) (CLImage imgDims imgObj)
  | bufLen /= dimProd = throwError "Buffer is not the same size as image"
  | otherwise = 
    do q <- fmap clQueue ask
       liftIO $ clEnqueueCopyBufferToImage q bufObj imgObj 0 (0,0,0) imgDims []
  where dimProd = let (w,h,d) = imgDims in w*h*d

-- | Copy the contents of a 'CLBuffer' to a 'CLImage1' of the same
-- element type. An error is raised if the 'CLBuffer''s size is
-- different than the total size of the 'CLImage1'. Blocks until the
-- copy operation is complete.
copyBufferToImage :: CLBuffer a -> CLImage1 a -> CL ()
copyBufferToImage buf img = 
  do ev <- copyBufferToImageAsync buf img
     liftIO $ clWaitForEvents [ev] >> clReleaseEvent ev >> return ()

-- | Copy the contents of a 'CLImage' to a 'CLBuffer' of the same
-- element type. An error is raised if the 'CLBuffer''s size is
-- different than the total size of the 'CLImage1'. Returns a
-- 'CLEvent' that may be waited upon for the copy operation to finish.
copyImageToBufferAsync :: forall a. CLImage1 a -> CLBuffer a -> CL CLEvent
copyImageToBufferAsync (CLImage imgDims imgObj) (CLBuffer bufLen bufObj)
  | dimProd /= bufLen = throwError "Buffer is not the same size as image"
  | otherwise = 
    do q <- fmap clQueue ask
       liftIO $ clEnqueueCopyImageToBuffer q imgObj bufObj (0,0,0) imgDims 0 []
  where dimProd = let (w,h,d) = imgDims in w*h*d

-- | Copy the contents of a 'CLImage' to a 'CLBuffer' of the same
-- element type. An error is raised if the 'CLBuffer''s size is
-- different than the total size of the 'CLImage1'. Blocks until the
-- copy operation is complete.
copyImageToBuffer :: forall a. CLImage1 a -> CLBuffer a -> CL ()
copyImageToBuffer img buf =
  do ev <- copyImageToBufferAsync img buf
     liftIO $ clWaitForEvents [ev] >> clReleaseEvent ev >> return ()
