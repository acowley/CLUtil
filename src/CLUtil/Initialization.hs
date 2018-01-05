-- | Simple device initialization and release.
module CLUtil.Initialization where
import Control.Monad (void)
import Control.Parallel.OpenCL
import CLUtil.State
import Data.Traversable (traverse)

clStateInit :: CLDeviceID -> IO OpenCLState
clStateInit dev =
  do context <- clCreateContext [] [dev] putStrLn
     OpenCLState dev context `fmap`
       clCreateCommandQueue context dev []
                            -- [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE]

-- |Initialize the first device of the given type.
ezInit :: CLDeviceType -> IO OpenCLState
ezInit t = do (platform:_) <- clGetPlatformIDs
              (dev:_) <- clGetDeviceIDs platform t
              clStateInit dev

-- |Release a context and command queue.
ezRelease :: OpenCLState -> IO ()
ezRelease (OpenCLState _ c q) =
  void $ clReleaseContext c >> clReleaseCommandQueue q
{-# DEPRECATED ezRelease "Use clReleaseDevice" #-}

-- | Release a context and command queue.
clReleaseDevice :: OpenCLState -> IO ()
clReleaseDevice = ezRelease

-- |Initialize the first GPU device found.
clDeviceGPU :: IO OpenCLState
clDeviceGPU = ezInit CL_DEVICE_TYPE_GPU

-- |Initialize the first CPU device found.
clDeviceCPU :: IO OpenCLState
clDeviceCPU = ezInit CL_DEVICE_TYPE_CPU

-- |Initialize a device by selecting the first device of the given
-- type that passes the given predicate. This may be used to
-- distinguish between devices of the same type.
clDeviceSelect :: CLDeviceType -> (CLDeviceID -> IO Bool)
               -> IO (Maybe OpenCLState)
clDeviceSelect t f = do (platform:_) <- clGetPlatformIDs
                        devs <- clGetDeviceIDs platform t
                        firstM devs >>= traverse clStateInit
  where firstM [] = return Nothing
        firstM (x:xs) = f x >>= \q -> case q of
                          True -> return $ Just x
                          False -> firstM xs
