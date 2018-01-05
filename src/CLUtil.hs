-- |High-level interfaces for working with 'Vector's and the OpenCL
-- library.
module CLUtil (
  -- * Initialization
  ezInit, clDeviceGPU, clDeviceCPU, clDeviceSelect,
  ezRelease, clReleaseDevice, OpenCLState(..),

  -- * Running OpenCL computations
  CL, HasCL, runCL,

  -- * Managing images and buffers
  HasCLMem(getCLMem),

  -- * Kernels
  loadProgram, loadProgramFile, kernelFromFile,
  KernelArgs, runKernel, runKernelAsync,

  -- * Operations in the @CL@ monad
  ask, throwError, liftIO,

  -- * Buffer Objects
  CLBuffer(..), allocBuffer, initBuffer, withBuffer,
  readBuffer, readBuffer', writeBuffer, withSharedVector, withSharedMVector,

  -- * Image Objects
  CLImage(..), allocImage, initImage,
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
  module Control.Parallel.OpenCL, Vector,
  CInt, CFloat, Word8, Word16, Word32, Int8, Int16, Int32, Storable
  ) where
import Control.Parallel.OpenCL
import Data.Vector.Storable (Vector)
import Foreign.C.Types (CInt, CFloat)

import CLUtil.CL
import CLUtil.Initialization
import CLUtil.Buffer
import CLUtil.Image
import CLUtil.BufferImageInterop
-- import CLUtil.KernelArgsCL
-- import CLUtil.KernelArgsCLAsync (runKernelAsync)
import CLUtil.KernelArgs
import CLUtil.State
import CLUtil.Async

import CLUtil.Load

import Data.Int
import Data.Word
import Foreign.Storable (Storable)
