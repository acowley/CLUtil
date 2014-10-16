-- | Examples where CLUtil allocates any needed output buffers.
module TestEZ where
import CLUtil.CL.ProgramCache
import qualified Data.Vector.Storable as V

test1 :: IO (Vector Float)
test1 = do dev <- ezInit CL_DEVICE_TYPE_CPU
           runCLClean dev $
             do k <- getKernel "VecEZ.cl" "vecAdd"
                runKernel k v1 v2 (Out 4) (Work1D 1)
  where v1 = V.fromList [1,2,3,4::Float]
        v2 = V.fromList [5,6,7,8::Float]

test2 :: IO (Vector Double, Vector CInt)
test2 = do dev <- ezInit CL_DEVICE_TYPE_CPU
           runCLClean dev $
             do k <- getKernel "VecEZ.cl" "funnyBusiness"
                runKernel k v1 v2 
                  (Out 12) -- v3 is a 12-element output
                  (Out 3)  -- v4 is a 3-element output
                  (Work1D $ 12 `div` 4)
  where v1 = V.enumFromN  0 12 :: Vector Double
        v2 = V.enumFromN 12 12 :: Vector Double

test3 :: IO [Vector Float]
test3 = do dev <- ezInit CL_DEVICE_TYPE_ALL
           runCLClean dev $
             do k <- getKernel "VecEZ2.cl" "floaty"
                v3 <- runKernel k v1 v2 (Out 16) (Work2D 4 4)
                      :: CL (Vector Float)
                return $ map (\i -> (V.slice i 4 v3)) [0,4..12]
  where v1 = V.enumFromN  0 16 :: Vector Float
        v2 = V.enumFromN 16 16 :: Vector Float
