-- | A monad transformer stack for working with OpenCL.
module Control.Parallel.CLUtil.Monad.CL (CL, runCL, runCL',
                                         ask, throwError, liftIO, okay) where
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Error (ErrorT(..), throwError)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R
import Control.Parallel.CLUtil (OpenCLState)

-- | The 'CL' type captures an 'OpenCLState' in a 'Reader' environment
-- and returns error messages for operations that fail.
type CL = ErrorT String (ReaderT OpenCLState IO)

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error'.
runCL :: OpenCLState -> CL a -> IO a
runCL s m = either error id <$> runReaderT (runErrorT m) s

-- | Run a 'CL' action with a given 'OpenCLState'. The result is
-- 'Either' an error message or the result of the action.
runCL' :: OpenCLState -> CL a -> IO (Either String a)
runCL' s m = runReaderT (runErrorT m) s

-- | Lift 'R.ask' up to the top of the 'CL' transformer stack.
ask :: CL OpenCLState
ask = lift R.ask

-- | Run an IO action that returns a 'Bool'. If the action returns
-- 'True', then return () in the error monad. If the action returns
-- 'False', throw an error message.
okay :: MonadIO m => String -> IO Bool -> ErrorT String m ()
okay msg m = do f <- liftIO m
                if f then return () else throwError $ "Failed: " ++ msg
