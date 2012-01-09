{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, OverlappingInstances, 
             RankNTypes #-}
-- |High-level interfaces for working with 'Vector's and the OpenCL
-- library.
module System.GPU.CLUtil (
  -- * Initialization and kernel compiliation
  ezInit, ezRelease, loadProgram, kernelFromFile, OpenCLState(..),
  -- * Variable arity kernel execution 
  -- |Similar in spirit to "Text.Printf"
  OutputSize(..), NumWorkItems(..), 
  runKernel, KernelArgs,
  runKernelCPS, KernelArgsCPS,
  runKernelAsync, KernelArgsAsync,
  module System.GPU.CLUtil.VectorBuffers,
  -- * Working with asynchronous kernels
  initOutputBuffer,
  CLAsync(..), waitCLAsync, waitCLAsyncs,
  -- * Re-exports for convenience
  module System.GPU.OpenCL, Vector, CInt
  ) where
import System.GPU.OpenCL
import Control.Monad (void)
import Data.Vector.Storable (Vector)
import Foreign.C.Types (CInt)
import Foreign.Ptr (nullPtr)
import System.GPU.CLUtil.State
import System.GPU.CLUtil.VectorBuffers
import System.GPU.CLUtil.KernelArgs
import System.GPU.CLUtil.KernelArgsCPS
import System.GPU.CLUtil.KernelArgsAsync
import System.GPU.CLUtil.KernelArgTypes

-- |Allocate a buffer whose contents are undefined.
initOutputBuffer :: Integral a => OpenCLState -> [CLMemFlag] -> a -> IO CLMem
initOutputBuffer s flags n = clCreateBuffer (clContext s) flags (n, nullPtr)

-- |Initialize the first device of the given type.
ezInit :: CLDeviceType -> IO OpenCLState
ezInit t = do (dev:_) <- clGetDeviceIDs nullPtr t
              context <- clCreateContext [dev] putStrLn
              q <- clCreateCommandQueue context dev []
              return $ OpenCLState dev context q

-- |Release a context and command queue.
ezRelease :: OpenCLState -> IO ()
ezRelease (OpenCLState _ c q) = 
  void $ clReleaseContext c >> clReleaseCommandQueue q

-- |Load program source using a previously-initialized 'OpenCLState'
-- record. The returned function may be used to create executable
-- kernels defined in the supplied program source.
loadProgram :: OpenCLState -> String -> IO (String -> IO CLKernel)
loadProgram state src = do p <- clCreateProgramWithSource (clContext state) src
                           clBuildProgram p [clDevice state] 
                                          "-cl-strict-aliasing"
                           return $ clCreateKernel p
-- Another option for the clBuildProgram call is "-cl-fast-relaxed-math"

-- |Load program source from the given file and build the named
-- kernel.
kernelFromFile :: OpenCLState -> FilePath -> String -> IO CLKernel
kernelFromFile state file kname = 
  readFile file >>= loadProgram state >>= ($ kname)




