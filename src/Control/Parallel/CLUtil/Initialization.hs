-- | Simple device initialization and release.
module Control.Parallel.CLUtil.Initialization where
import Control.Monad (void)
import Control.Parallel.OpenCL
import Control.Parallel.CLUtil.State
import Foreign.Ptr (nullPtr)

-- |Initialize the first device of the given type.
ezInit :: CLDeviceType -> IO OpenCLState
ezInit t = do (dev:_) <- clGetDeviceIDs nullPtr t
              context <- clCreateContext [] [dev] putStrLn
              q <- clCreateCommandQueue context dev []
              return $ OpenCLState dev context q

-- |Release a context and command queue.
ezRelease :: OpenCLState -> IO ()
ezRelease (OpenCLState _ c q) = 
  void $ clReleaseContext c >> clReleaseCommandQueue q

