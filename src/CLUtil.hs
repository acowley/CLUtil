-- |High-level interfaces for working with 'Vector's and the OpenCL
-- library.
module CLUtil (
  -- * Initialization
  ezInit, clDeviceGPU, clDeviceCPU, clDeviceSelect,
  ezRelease, OpenCLState(..),

  -- * Running OpenCL computations
  CL, runCL,
  -- CL, runCL, runCL', runCLIO, runCLError, runCLClean, nestCL, clInitState,

  -- * Mangaging images and buffers
  -- Cleanup, registerCleanup, unregisterCleanup, ReleaseKey,
  -- runCleanup, cleanupAll, releaseObject, HasCLMem(getCLMem),

  -- * Managing images and buffers
  HasCLMem(getCLMem),

  -- * Kernels
  KernelArgsCL, runKernel, runKernelAsync,

  -- * Operations in the @CL@ monad
  ask, throwError, liftIO, -- okay,

  -- * Buffer Objects
  -- CLBuffer(..), allocBuffer, allocBufferKey, allocBuffer_, 
  -- initBuffer, initBufferKey, initBuffer_,
  CLBuffer(..), allocBuffer, initBuffer, 
  readBuffer, readBuffer', writeBuffer, withSharedVector, withSharedMVector,

  -- * Image Objects
  CLImage(..), allocImage, initImage,
  -- CLImage(..), allocImage, allocImageKey, 
  -- allocImageFmt, allocImage_, allocImageFmt_,
  -- initImage, initImageKey, initImage_, initImageFmt, initImageFmt_,
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
  LocalMem(..), localFloat, localDouble, localInt, localWord32, vectorDup,

  -- * Re-exports for convenience
  module Control.Parallel.OpenCL, Vector, CInt, CFloat, Word8, Storable
  ) where
import Control.Parallel.OpenCL
import Data.Vector.Storable (Vector)
import Foreign.C.Types (CInt, CFloat)

import CLUtil.CL
import CLUtil.Initialization
import CLUtil.Buffer
import CLUtil.Image
import CLUtil.BufferImageInterop
import CLUtil.KernelArgsCL
import CLUtil.KernelArgsCLAsync (runKernelAsync)
import CLUtil.KernelArgTypes
import CLUtil.State
import CLUtil.Async

import Data.Word (Word8)
import Foreign.Storable (Storable)
