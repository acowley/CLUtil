{-# LANGUAGE FlexibleContexts #-}
module Control.Parallel.CLUtil.Monad.ProgramCache
  (getKernel, emptyCache, Cache) where
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Parallel.CLUtil
import Data.Map (Map)
import qualified Data.Map as M

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

-- | Get a kernel given a program file name and a kernel name. If the
-- kernel was already loaded, it is returned. If not, and the program
-- was previously loaded, the loaded program is used to provide the
-- requested kernel. If the program has not yet been loaded, it is
-- loaded from disk.
getKernel :: (MonadState Cache m, MonadIO m, MonadReader OpenCLState m)
           => String -> String -> m CLKernel
getKernel progName kerName = 
  do cache <- get
     (k,c) <- 
       case M.lookup progName cache of
         Nothing -> do mk <- ask >>= liftIO . flip loadProgramFile progName
                       loadKernel' kerName (mk, M.empty)
         Just p -> loadKernel' kerName p
     put $ M.insert progName c cache
     return k
