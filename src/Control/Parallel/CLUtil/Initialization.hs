-- | Simple device initialization and release.
module Control.Parallel.CLUtil.Initialization where
import Control.Monad (void)
import Control.Parallel.OpenCL
import Control.Parallel.CLUtil.State
import Data.Traversable (traverse)
import Foreign.Ptr (nullPtr)

clStateInit :: CLDeviceID -> IO OpenCLState
clStateInit dev =
  do context <- clCreateContext [] [dev] putStrLn
     OpenCLState dev context `fmap` 
       clCreateCommandQueue context dev
                            [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE]

-- |Initialize the first device of the given type.
ezInit :: CLDeviceType -> IO OpenCLState
ezInit t = do (dev:_) <- clGetDeviceIDs nullPtr t
              clStateInit dev

-- |Release a context and command queue.
ezRelease :: OpenCLState -> IO ()
ezRelease (OpenCLState _ c q) = 
  void $ clReleaseContext c >> clReleaseCommandQueue q

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
clDeviceSelect t f = clGetDeviceIDs nullPtr t
                     >>= firstM >>= traverse clStateInit 
  where firstM [] = return Nothing
        firstM (x:xs) = f x >>= \q -> case q of
                          True -> return $ Just x
                          False -> firstM xs
