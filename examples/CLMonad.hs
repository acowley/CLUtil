module CLMonad where
import Control.Applicative ((<$))
import Control.Monad
import CLUtil.Resource
import qualified Data.Vector.Storable as V

-- Pre-allocate buffers for arguments and result.
mkVadd :: Int -> CL (Vector CFloat -> Vector CFloat -> CL (Vector CFloat))
mkVadd n = do k <- kernelFromFile "examples/VecAdd.cl" "vadd1D"
              [b1,b2] <- replicateM 2 $ allocBuffer [CL_MEM_READ_ONLY] n
              r <- allocBuffer [CL_MEM_WRITE_ONLY] n
              return $ \v1 v2 ->
                do writeBuffer b1 v1
                   writeBuffer b2 v2
                   () <- runKernel k b1 b2 r (Work1D n)
                   readBuffer r

testPre :: IO (Vector CFloat)
testPre = do gpu <- clDeviceGPU
             (vadd, clean) <- runCL gpu $ mkVadd 6
             r <- runCL' gpu $ vadd v1 v2
             r <$ (runCleanup clean >> clReleaseDevice gpu)
  where v1 = V.fromList [1,2,3,4,5,6]
        v2 = V.fromList [7,8,9,10,11,12]

main :: IO ()
main = testPre >>= showOutput

showOutput :: Vector CFloat -> IO ()
showOutput = putStrLn . ("Result is " ++) . show

-- A basic test that doesn't pre-allocate buffers.
testNoPre :: IO (Vector CFloat)
testNoPre = do gpu <- clDeviceGPU
               r <- runCLClean gpu $
                      do k <- kernelFromFile "examples/VecAdd.cl" "vadd2D"
                         runKernel k v1 v2 (Out 6) (Work2D 3 2)
               r' <- vectorDup r
               r' <$ clReleaseDevice gpu
  where v1,v2 :: Vector CFloat
        v1 = V.fromList [1,2,3,4,5,6]
        v2 = V.fromList [7,8,9,10,11,12]
