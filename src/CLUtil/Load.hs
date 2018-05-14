-- | Utilities for loading OpenCL programs from source.
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module CLUtil.Load
    ( OpenCLSource(..)
    , loadProgramWOptions
    , loadProgram
    , loadProgramFastMath
    , loadProgramFile
    , kernelFromSourceWOptions
    , kernelFromSource
    , kernelFromFile
    )
where

import Control.Monad ((>=>))
import Control.Parallel.OpenCL
import CLUtil.State

class OpenCLSource source where
  -- | Prepare a source to be loaded
  prepSource :: source -> String

instance OpenCLSource String where
  prepSource = id

-- |Load a program from an OpenCLSource using a string listing the build options and a previously initialized
-- 'OpenCLState' The returned function may be used to create
-- executable kernels from the loaded program.
loadProgramWOptions :: (OpenCLSource s) => String -> OpenCLState -> s -> IO (String -> IO CLKernel)
loadProgramWOptions options state src =
  do  p <- clCreateProgramWithSource (clContext state) $ prepSource src
      clBuildProgram p [clDevice state] options
      return $ clCreateKernel p

-- |Load a program using a previously initialized
-- 'OpenCLState'. The returned function may be used to create
-- executable kernels defined in the program file.
loadProgram :: (OpenCLSource source) => OpenCLState -> source -> IO (String -> IO CLKernel)
loadProgram = loadProgramWOptions "-cl-strict-aliasing"

-- |Load program source using a previously initialized
-- 'OpenCLState'. The returned function may be used to create
-- executable kernels with the @-cl-fast-relaxed-math@ option from
-- supplied program source.
loadProgramFastMath :: (OpenCLSource source) => OpenCLState -> source -> IO (String -> IO CLKernel)
loadProgramFastMath = loadProgramWOptions "-cl-fast-relaxed-math"
                    -- "-cl-strict-aliasing -cl-fast-relaxed-math"

-- | Build the named kernel from source.
kernelFromSource :: (OpenCLSource source) => OpenCLState -> source -> String -> IO CLKernel
kernelFromSource state source kname = loadProgram state source >>= ($ kname)

-- | Build the named kernel from source with options.
kernelFromSourceWOptions :: (OpenCLSource source) => String -> OpenCLState -> source -> String -> IO CLKernel
kernelFromSourceWOptions options state source kname = loadProgramWOptions options state source >>= ($ kname)

-- | Load program from file.
loadProgramFile :: OpenCLState -> FilePath -> IO (String -> IO CLKernel)
loadProgramFile s = readFile >=> loadProgram s

-- | Build named kernel from source file.
kernelFromFile :: OpenCLState -> FilePath -> String -> IO CLKernel
kernelFromFile s file kname = readFile file >>= loadProgram s >>= ($ kname)
