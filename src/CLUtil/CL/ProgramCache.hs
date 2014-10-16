{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
             ImpredicativeTypes, MultiParamTypeClasses, ScopedTypeVariables #-}
-- | When initializing a program that makes use of OpenCL, a common
-- need is to load several kernels from the same source program
-- file. These simple caching utilities free you from needing to
-- couple the initialization of various processing stages due to
-- overlapped resource usage (i.e. programs or kernels). Instead,
-- initializers may independently use 'getKernel' without worrying if
-- a program or kernel has already been loaded by someone else.
module CLUtil.CL.ProgramCache (
  -- * Initialization
  ezInit, clDeviceGPU, clDeviceCPU, clDeviceSelect,
  ezRelease, OpenCLState(..),

  -- * Running OpenCL computations
  CL, runCL, runCL', runCLIO, runCLClean, nestCL, clInitState,

  -- * Mangaging images and buffers
  Cleanup, registerCleanup, unregisterCleanup, ReleaseKey,
  runCleanup, releaseItem, releaseObject, HasCLMem(getCLMem),

  -- * Kernels
  getKernel, getKernelFromSource, KernelArgsCL, runKernel, runKernelAsync,
  HasCache,

  -- * Buffer Objects
  CLBuffer(..), R.allocBuffer, R.allocBufferKey,
  R.initBuffer, R.initBufferKey,
  readBuffer, readBuffer', writeBuffer, withSharedVector, withSharedMVector,

  -- * Image Objects
  CLImage(..), R.allocImage, R.allocImageKey, R.allocImageFmt,
  R.initImage, R.initImageKey, R.initImageFmt,
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
import CLUtil.CL.Resource hiding (CL, CL', runCL, runCL', runCLIO,
                                  nestCL, runCLClean)
import qualified CLUtil.CL as Base
import qualified CLUtil.CL.Resource as R
import CLUtil hiding (allocBuffer, initBuffer, allocImage, initImage, CL, runCL)
import CLUtil.Load
import CLUtil.State
import Control.Applicative
import Control.Lens (_2, (%~), Lens', lens, use, (.=))
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Parallel.OpenCL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Storable as V

-- | A kernel cache for a particular program file.
type KCache = (String -> IO CLKernel, Map String CLKernel)

-- | A program cache.
type Cache = Map String KCache

-- | Provide access to a 'Cache' value as part of some potentially
-- larger state.
class HasCache a where
  cachel :: Lens' a Cache

instance HasCache Cache where
  cachel = lens id (flip const)

instance HasCache (a,Cache) where
  cachel = _2

type CL = StateT (R.Cleanup, Cache) Base.CL
type CL' s m = (MonadState s m, HasCache s, R.CL' s m)

-- | Initialize some mutable state for running OpenCL
-- computations. This maintains a reference to a cache of loaded
-- programs and resource allocations. The first returned function may
-- be used to run 'CL' computations in a shared state. The second
-- returned value will release all remaining resources and the OpenCL
-- device itself.
clInitState :: OpenCLState -> IO (forall a. CL a -> IO a, IO ())
clInitState dev =
  do accState <- newIORef (newCleanup, emptyCache)
     let done = readIORef accState
                >>= runCleanup . fst
                >> ezRelease dev
         goCL :: CL a -> IO a
         goCL m = do (cleanSt, cacheSt) <- readIORef accState
                     (r,(cln,csh)) <- (runExceptT
                                      . flip runReaderT dev
                                      $ runStateT m (cleanSt,cacheSt))
                                      >>= either error return
                     writeIORef accState (cln,csh)
                     return r
     return (goCL, done)


loadKernel' :: MonadIO m => String -> KCache -> m (CLKernel, KCache)
loadKernel' kName p@(mk, cache) = 
  case M.lookup kName cache of
    Nothing -> do k <- liftIO $ mk kName
                  return (k, (mk, M.insert kName k cache))
    Just k -> return (k, p)

-- | An empty program cache.
emptyCache :: Cache
emptyCache = M.empty

loadKernelAux :: CL' s m
              => (String -> OpenCLState -> IO (String -> IO CLKernel))
              -> String -> String -> m CLKernel
loadKernelAux prepProgram progName kerName =
  do cache <- use cachel
     (k,c) <- case M.lookup progName cache of
                Nothing -> do mk <- ask >>= liftIO . prepProgram progName
                              loadKernel' kerName (mk, M.empty)
                Just p -> loadKernel' kerName p
     cachel .= M.insert progName c cache
     return k

-- * Cached Kernel Loads

-- | Get a kernel given a program file name and a kernel name. If the
-- kernel was already loaded, it is returned. If not, and the program
-- was previously loaded, the loaded program is used to provide the
-- requested kernel. If the program has not yet been loaded, it is
-- loaded from the source file.
getKernel ::  CL' s m
           => FilePath -> String -> m CLKernel
getKernel = loadKernelAux $ flip loadProgramFile

-- | Get a kernel given program source and a kernel name. If the
-- kernel was already loaded, it is returned. If not, and the program
-- source was previously loaded, the loaded program is used to provide
-- the requested kernel. If the program has not yet been loaded, it is
-- loaded from the given source code.
getKernelFromSource :: CL' s m => String -> String -> m CLKernel
getKernelFromSource = loadKernelAux $ flip loadProgram

-- * Running CL Actions

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error'.
runCL :: OpenCLState -> CL a -> IO (a, Cleanup)
runCL s m = Base.runCL s . fmap (fmap fst) $
            runStateT m (R.newCleanup, emptyCache)

-- | Run a 'CL' action in an environment with a fresh cleanup
-- record. This lets the caller embed an action whose resource
-- management should be kept separate from the ambient context.
nestCL :: CL a -> CL (a, Cleanup)
nestCL m = ask >>= liftIO . flip runCL m

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error'. This function behaves identically to
-- 'runCL' with the exception that the 'Cleanup' action is returned as
-- an unadorned 'IO' action. This may help isolate callers from any
-- awareness of OpenCL, but makes the types a bit more ambiguous.
runCLIO :: OpenCLState -> CL a -> IO (a, IO ())
runCLIO s m = (_2 %~ runCleanup) <$> runCL s m

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error', and all cleanup actions are run before
-- returning the result of the computation.
runCLClean :: OpenCLState -> CL a -> IO a
runCLClean s m = runCLIO s m >>= (\(x,c) -> x <$ c)

-- | Run a 'CL' action that discards any accumulated cleanup actions.
runCL' :: OpenCLState -> CL a -> IO a
runCL' = (fmap fst .) . runCL
