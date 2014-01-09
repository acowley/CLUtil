{-# LANGUAGE FlexibleContexts #-}
-- | When initializing a program that makes use of OpenCL, a common
-- need is to load several kernels from the same source program
-- file. These simple caching utilities free you from needing to
-- couple the initialization of various processing stages due to
-- overlapped resource usage (i.e. programs or kernels). Instead,
-- initializers may independently use 'getKernel' without worrying if
-- a program or kernel has already been loaded by someone else.
module Control.Parallel.CLUtil.ProgramCache
  (getKernel, getKernelFromSource, emptyCache, Cache) where
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Parallel.OpenCL
import Control.Parallel.CLUtil.Load
import Control.Parallel.CLUtil.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | A kernel cache for a particular program file.
type KCache = (String -> IO CLKernel, Map String CLKernel)

-- | A program cache.
type Cache = Map String KCache

loadKernel' :: (MonadIO m) => String -> KCache -> m (CLKernel, KCache)
loadKernel' kName p@(mk, cache) = 
  case M.lookup kName cache of
    Nothing -> do k <- liftIO $ mk kName
                  return (k, (mk, M.insert kName k cache))
    Just k -> return (k, p)

-- | An empty program cache.
emptyCache :: Cache
emptyCache = M.empty

loadKernelAux :: (MonadState Cache m, MonadIO m, MonadReader OpenCLState m)
              => (String -> OpenCLState -> IO (String -> IO CLKernel))
              -> String -> String -> m CLKernel
loadKernelAux prepProgram progName kerName =
  do cache <- get
     (k,c) <- case M.lookup progName cache of
                Nothing -> do mk <- ask >>= liftIO . prepProgram progName
                              loadKernel' kerName (mk, M.empty)
                Just p -> loadKernel' kerName p
     put $ M.insert progName c cache
     return k

-- | Get a kernel given a program file name and a kernel name. If the
-- kernel was already loaded, it is returned. If not, and the program
-- was previously loaded, the loaded program is used to provide the
-- requested kernel. If the program has not yet been loaded, it is
-- loaded from the source file.
getKernel :: (MonadState Cache m, MonadIO m, MonadReader OpenCLState m)
           => FilePath -> String -> m CLKernel
getKernel = loadKernelAux $ flip loadProgramFile

-- | Get a kernel given program source and a kernel name. If the
-- kernel was already loaded, it is returned. If not, and the program
-- source was previously loaded, the loaded program is used to provide
-- the requested kernel. If the program has not yet been loaded, it is
-- loaded from the given source code.
getKernelFromSource :: ( MonadState Cache m, MonadIO m
                       , MonadReader OpenCLState m)
                    => String -> String -> m CLKernel
getKernelFromSource = loadKernelAux $ flip loadProgram
