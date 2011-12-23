import System.GPU.CLUtil
import qualified Data.Vector.Storable as V

test1 = do s <- ezInit CL_DEVICE_TYPE_CPU
           k <- kernelFromFile s "VecEZ.cl" "vecAdd"
           let v1 = V.fromList [1,2,3,4::Float]
               v2 = V.fromList [5,6,7,8::Float]
           v3 <- runKernel s k v1 v2 (Out 4) (Work1D 1)
           print (v3::Vector Float)

test2 = do s <- ezInit CL_DEVICE_TYPE_CPU
           k <- kernelFromFile s "VecEZ.cl" "funnyBusiness"
           let v1 = V.enumFromN  0 12 :: Vector Double
               v2 = V.enumFromN 12 12 :: Vector Double
           (v3,v4) <- runKernel s k v1 v2 
                                (Out 12) -- v3 is a 12-element output
                                (Out 3)  -- v4 is a 3-element output
                                (Work1D $ 12 `div` 4)
           print $ V.sum (v3::Vector Double)
           print $ V.sum (v4::Vector CInt)

test3 = do s <- ezInit CL_DEVICE_TYPE_GPU
           k <- kernelFromFile s "VecEZ2.cl" "floaty"
           let v1 = V.enumFromN  0 16 :: Vector Float
               v2 = V.enumFromN 16 16 :: Vector Float
           v3 <- runKernel s k v1 v2 (Out 16) (Work2D 4 4)
                 :: IO (Vector Float)
           mapM_ (\i -> print (V.slice i 4 v3)) [0,4..12]
