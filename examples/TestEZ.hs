module TestEZ where
import Control.Parallel.CLUtil
import qualified Data.Vector.Storable as V

test1 :: IO ()
test1 = do dev <- ezInit CL_DEVICE_TYPE_CPU
           runCLClean dev $
             do k <- getKernel "VecEZ.cl" "vecAdd"
                v3 <- runKernel k v1 v2 (Out 4) (Work1D 1)
                liftIO $ print (v3::Vector Float)
  where v1 = V.fromList [1,2,3,4::Float]
        v2 = V.fromList [5,6,7,8::Float]

test2 :: IO ()
test2 = do dev <- ezInit CL_DEVICE_TYPE_CPU
           runCLClean dev $
             do k <- getKernel "VecEZ.cl" "funnyBusiness"
                (v3,v4) <- runKernel k v1 v2 
                             (Out 12) -- v3 is a 12-element output
                             (Out 3)  -- v4 is a 3-element output
                             (Work1D $ 12 `div` 4)
                liftIO $ do print $ V.sum (v3::Vector Double)
                            print $ V.sum (v4::Vector CInt)
  where v1 = V.enumFromN  0 12 :: Vector Double
        v2 = V.enumFromN 12 12 :: Vector Double

test3 :: IO ()
test3 = do dev <- ezInit CL_DEVICE_TYPE_ALL
           runCLClean dev $
             do k <- getKernel "VecEZ2.cl" "floaty"
                v3 <- runKernel k v1 v2 (Out 16) (Work2D 4 4)
                      :: CL (Vector Float)
                liftIO $ mapM_ (\i -> print (V.slice i 4 v3)) [0,4..12]
  where v1 = V.enumFromN  0 16 :: Vector Float
        v2 = V.enumFromN 16 16 :: Vector Float

