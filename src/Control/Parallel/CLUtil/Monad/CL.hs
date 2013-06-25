{-# LANGUAGE FlexibleContexts #-}
-- | A monad transformer stack for working with OpenCL.
module Control.Parallel.CLUtil.Monad.CL 
  (CL, runCL, runCL', CLReleasable(..),
   ask, throwError, liftIO, okay
  ) where
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Error (ErrorT(..), throwError)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT, evalStateT)
import qualified Control.Monad.Trans.Reader as R
import Control.Parallel.CLUtil
import Control.Parallel.CLUtil.Monad.ProgramCache (Cache, emptyCache)

-- | The 'CL' type captures an 'OpenCLState' in a 'Reader' environment
-- and returns error messages for operations that fail.
type CL = ErrorT String (StateT Cache (ReaderT OpenCLState IO))

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error'.
runCL :: OpenCLState -> CL a -> IO a
runCL s m = either error id <$> runReaderT (evalStateT (runErrorT m) emptyCache) s

-- | Run a 'CL' action with a given 'OpenCLState'. The result is
-- 'Either' an error message or the result of the action.
runCL' :: OpenCLState -> CL a -> IO (Either String a)
runCL' s m = runReaderT (evalStateT (runErrorT m) emptyCache) s

-- | Lift 'R.ask' up to the top of the 'CL' transformer stack.
ask :: CL OpenCLState
ask = lift (lift R.ask)

-- | Run an IO action that returns a 'Bool'. If the action returns
-- 'True', then return () in the error monad. If the action returns
-- 'False', throw an error message.
okay :: MonadIO m => String -> IO Bool -> ErrorT String m ()
okay msg m = do f <- liftIO m
                if f then return () else throwError $ "Failed: " ++ msg

-- | A class for things that can be released from OpenCL.
class CLReleasable a where
  -- | Decrement the reference count of a memory object.
  releaseObject :: a -> IO Bool
