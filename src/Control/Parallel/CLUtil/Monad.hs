-- | A convenient monadic interface for working with OpenCL.
module Control.Parallel.CLUtil.Monad (
    -- * The CL Monad
    CL, runCL, runCL', releaseObject,
    -- * Kernels
    getKernel, KernelArgsCL, runKernelCL,
    -- * Buffer Objects
    CLBuffer(..), allocBuffer, initBuffer, readBuffer, readBuffer', writeBuffer,
    -- * Image Objects
    CLImage(..), allocImage, allocImage', initImage, initImage',
    readImage, readImage', writeImage,
    NumChan(..), CLImage1, CLImage2, CLImage3, CLImage4
  ) where

import Control.Parallel.CLUtil.Monad.CL
import Control.Parallel.CLUtil.Monad.Buffer
import Control.Parallel.CLUtil.Monad.Image
import Control.Parallel.CLUtil.Monad.KernelArgsCL
import Control.Parallel.CLUtil.Monad.ProgramCache
