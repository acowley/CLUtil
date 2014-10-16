-- | Utilities for loading OpenCL programs from source.
module CLUtil.Load where
import Control.Monad ((>=>))
import Control.Parallel.OpenCL
import CLUtil.State

-- |Load program source using a previously initialized
-- 'OpenCLState'. The returned function may be used to create
-- executable kernels defined in the supplied program source.
loadProgram :: OpenCLState -> String -> IO (String -> IO CLKernel)
loadProgram state src = do p <- clCreateProgramWithSource (clContext state) src
                           clBuildProgram p [clDevice state] ""
                                          -- "-cl-strict-aliasing"
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
