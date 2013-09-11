-- | A convenient monadic interface for working with OpenCL.
module Control.Parallel.CLUtil.Monad (
    -- * The CL Monad
    CL, runCL, runCL', runCLIO, runCLError, releaseObject, throwError, liftIO,
    registerCleanup, Cleanup(runCleanup),
    -- * Kernels
    getKernel, KernelArgsCL, runKernelCL, runKernelCLAsync,
    -- * Buffer Objects
    CLBuffer(..), allocBuffer, allocBuffer_, initBuffer, initBuffer_,
    readBuffer, readBuffer', writeBuffer,
    -- * Image Objects
    CLImage(..), allocImage, allocImageFmt, allocImage_, allocImageFmt_,
    initImage, initImage_, initImageFmt, initImageFmt_,
    readImage, readImage', writeImage,
    NumChan(..), HalfFloat, 
    NormInt8(..), NormWord8(..), NormInt16(..), NormWord16(..),
    CLImage1, CLImage2, CLImage3, CLImage4,
    -- * Buffer-Image Interoperation
    copyBufferToImage, copyBufferToImageAsync,
    copyImageToBuffer, copyImageToBufferAsync,
    -- * Asynchonous Computations
    CLAsync, waitAll, waitAll_, waitAll', waitAllUnit, waitOne,
    readImageAsync', readImageAsync,
    writeImageAsync, readBufferAsync, readBufferAsync', writeBufferAsync
  ) where

import Control.Parallel.CLUtil.Monad.CL
import Control.Parallel.CLUtil.Monad.Buffer
import Control.Parallel.CLUtil.Monad.Image
import Control.Parallel.CLUtil.Monad.KernelArgsCL
import Control.Parallel.CLUtil.Monad.KernelArgsCLAsync (runKernelCLAsync)
import Control.Parallel.CLUtil.Monad.ProgramCache
import Control.Parallel.CLUtil.Monad.BufferImageInterop
import Control.Parallel.CLUtil.Monad.Async
