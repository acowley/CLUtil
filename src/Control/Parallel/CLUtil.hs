-- |High-level interfaces for working with 'Vector's and the OpenCL
-- library.
module Control.Parallel.CLUtil (
  -- * Initialization and kernel compiliation
  ezInit, ezRelease, loadProgram, loadProgramFastMath, 
  loadProgramFile, loadProgramFileFastMath, kernelFromFile, 
  OpenCLState(..),
  -- * Variable arity kernel execution 
  -- |Similar in spirit to "Text.Printf"
  OutputSize(..), NumWorkItems(..), WorkGroup(..),
  LocalMem(..), localFloat, localDouble, localInt, localWord32,
  runKernel, KernelArgs,
  runKernelCPS, KernelArgsCPS,
  runKernelAsync, KernelArgsAsync,
  module Control.Parallel.CLUtil.VectorBuffers,
  -- * Working with asynchronous kernels
  initOutputBuffer,
  IOAsync(..), waitIOAsync, waitIOAsyncs,
  -- * Re-exports for convenience
  module Control.Parallel.OpenCL, Vector, CInt, CFloat
  ) where
import Control.Parallel.OpenCL
import Control.Monad (void, (>=>))
import Data.Vector.Storable (Vector)
import Foreign.C.Types (CInt, CFloat)
import Foreign.Ptr (nullPtr)
import Control.Parallel.CLUtil.State
import Control.Parallel.CLUtil.VectorBuffers
import Control.Parallel.CLUtil.KernelArgs
import Control.Parallel.CLUtil.KernelArgsCPS
import Control.Parallel.CLUtil.KernelArgsAsync
import Control.Parallel.CLUtil.KernelArgTypes

-- |Allocate a buffer whose contents are undefined.
initOutputBuffer :: Integral a => OpenCLState -> [CLMemFlag] -> a -> IO CLMem
initOutputBuffer s flags n = clCreateBuffer (clContext s) flags (n, nullPtr)

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

-- |Load program source using a previously initialized
-- 'OpenCLState'. The returned function may be used to create
-- executable kernels defined in the supplied program source.
loadProgram :: OpenCLState -> String -> IO (String -> IO CLKernel)
loadProgram state src = do p <- clCreateProgramWithSource (clContext state) src
                           clBuildProgram p [clDevice state] 
                                          "-cl-strict-aliasing"
                           return $ clCreateKernel p

-- |Load a program from a file using a previously initialized
-- 'OpenCLState'. The returned function may be used to create
-- executable kernels defined in the program file.
loadProgramFile :: OpenCLState -> FilePath -> IO (String -> IO CLKernel)
loadProgramFile s = readFile >=> loadProgram s

-- |Load program source using a previously initialized
-- 'OpenCLState'. The returned function may be used to create
-- executable kernels with the @-cl-fast-relaxed-math@ option from
-- supplied program source.
loadProgramFastMath :: OpenCLState -> String -> IO (String -> IO CLKernel)
loadProgramFastMath state src = 
  do p <- clCreateProgramWithSource (clContext state) src
     clBuildProgram p [clDevice state] 
                    "-cl-strict-aliasing -cl-fast-relaxed-math"
     return $ clCreateKernel p

-- |Load a program from a file using a previously initialized
-- 'OpenCLState'. The returned function may be used to create
-- executable kernels with the @-cl-fast-relaxed-math@ option from the
-- loaded program.
loadProgramFileFastMath :: OpenCLState -> FilePath -> IO (String -> IO CLKernel)
loadProgramFileFastMath s = readFile >=> loadProgramFastMath s

-- |Load program source from the given file and build the named
-- kernel.
kernelFromFile :: OpenCLState -> FilePath -> String -> IO CLKernel
kernelFromFile state file kname = 
  readFile file >>= loadProgram state >>= ($ kname)
