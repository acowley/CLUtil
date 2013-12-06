-- |High-level interfaces for working with 'Vector's and the OpenCL
-- library.
module Control.Parallel.CLUtil (
  -- * Initialization
  ezInit, ezRelease, OpenCLState(..),

  -- * Running OpenCL computations
  CL, runCL, runCL', runCLIO, runCLError, runCLClean, nestCL,

  -- * Mangaging images and buffers
  Cleanup, registerCleanup, unregisterCleanup, ReleaseKey,
  runCleanup, cleanupAll, releaseObject,

  -- * Kernels
  getKernel, KernelArgsCL, runKernel, runKernelAsync,

  -- * Operations in the @CL@ monad
  ask, throwError, liftIO, okay,

  -- * Buffer Objects
  CLBuffer(..), allocBuffer, allocBufferKey, allocBuffer_,
  initBuffer, initBufferKey, initBuffer_,
  readBuffer, readBuffer', writeBuffer, withSharedVector, withSharedMVector,

  -- * Image Objects
  CLImage(..), allocImage, allocImageKey,
  allocImageFmt, allocImage_, allocImageFmt_,
  initImage, initImageKey, initImage_, initImageFmt, initImageFmt_,
  readImage, readImage', writeImage, copyImage,
  NumChan(..), HalfFloat,
  NormInt8(..), NormWord8(..), NormInt16(..), NormWord16(..),
  CLImage1, CLImage2, CLImage3, CLImage4,

  -- * Buffer-Image Interoperation
  copyBufferToImage, copyBufferToImageAsync,
  copyImageToBuffer, copyImageToBufferAsync,

  -- * Asynchonous Computations
  CLAsync, waitAll, waitAll_, waitAll', waitAllUnit, waitOne,
  readImageAsync', readImageAsync, copyImageAsync,
  writeImageAsync, readBufferAsync, readBufferAsync', writeBufferAsync,

  -- * OpenCL kernel arguments
  OutputSize(..), NumWorkItems(..), WorkGroup(..),
  LocalMem(..), localFloat, localDouble, localInt, localWord32,

  -- * Re-exports for convenience
  module Control.Parallel.OpenCL, Vector, CInt, CFloat
  ) where
import Control.Parallel.OpenCL
import Data.Vector.Storable (Vector)
import Foreign.C.Types (CInt, CFloat)
import Control.Parallel.CLUtil.CL
import Control.Parallel.CLUtil.Initialization
import Control.Parallel.CLUtil.Buffer
import Control.Parallel.CLUtil.Image
import Control.Parallel.CLUtil.BufferImageInterop
import Control.Parallel.CLUtil.KernelArgsCL
import Control.Parallel.CLUtil.KernelArgsCLAsync (runKernelAsync)
import Control.Parallel.CLUtil.KernelArgTypes
import Control.Parallel.CLUtil.State
import Control.Parallel.CLUtil.Async
