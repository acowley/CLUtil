{-# LANGUAGE FlexibleContexts #-}
-- | A monad transformer stack for working with OpenCL.
module Control.Parallel.CLUtil.Monad.CL 
  (CL, runCL, runCL', runCLError, CLReleasable(..),
   ask, throwError, liftIO, okay, registerCleanup, Cleanup(..),
  ) where
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Error (ErrorT(..))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.RWS.Strict (RWST(..), evalRWST)
import qualified Control.Monad.RWS.Strict as MTL
import Control.Parallel.CLUtil
import Control.Parallel.CLUtil.Monad.ProgramCache (Cache, emptyCache)
import Data.Monoid

-- | Release resources used by OpenCL.
newtype Cleanup = Cleanup { runCleanup :: IO () }

instance Monoid Cleanup where
  mempty = Cleanup $ return ()
  Cleanup a `mappend` Cleanup b = Cleanup $ a >> b

-- | The 'CL' type captures OpenCL context as well as resource
-- initialization and cleanup helpers. For initialization, OpenCL
-- program compilations are cached by 'CL' values to help with
-- decoupling initialization steps that may refer to the same OpenCL
-- source files. Cleanup actions are captured in a composite 'IO'
-- action built from every action supplied to 'registerCleanup'; this
-- action is returned by 'runCL' and 'runCLError'. Note that the
-- "Control.Parallel.CLUtil.Monad.Image" and
-- "Control.Parallel.CLUtil.Buffer" modules provide allocation and
-- initialization methods that perform this registration
-- automatically.
--
-- Automatic registration for cleanup is convenient when resources are
-- intended to be used persistently over the life of an application
-- and should simply be cleaned up at the end. Functions that allocate
-- or initialize resources that end with an underscore
-- (e.g. 'Control.Parallel.CLUtil.Monad.Image.allocImage_') do /not/
-- register the newly created objects for cleanups. This means that
-- the caller is taking responsibility for calling 'releaseObject' on
-- resources to be freed.
type CL = RWST OpenCLState Cleanup Cache (ErrorT String IO)

-- | Register a cleanup action. NOTE: The return value of the supplied
-- action is discarded. A composite cleanup action is returned by
-- calls to 'runCL' or 'runCLError'.
registerCleanup :: IO a -> CL ()
registerCleanup = MTL.tell . Cleanup . (() <$)

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error'.
runCL :: OpenCLState -> CL a -> IO (a, Cleanup)
runCL s m = runErrorT (evalRWST m s emptyCache) >>= either error return

-- | Run a 'CL' action that discards any accumulated cleanup action.
runCL' :: OpenCLState -> CL a -> IO a
runCL' = (fmap fst .) . runCL

-- | Run a 'CL' action with a given 'OpenCLState'. The result is
-- 'Either' an error message or the result of the action.
runCLError :: OpenCLState -> CL a -> IO (Either String (a, Cleanup))
runCLError s m =  runErrorT (evalRWST m s emptyCache)

-- | Lift 'R.ask' up to the top of the 'CL' transformer stack.
ask :: CL OpenCLState
ask = MTL.ask

-- | Run an IO action that returns a 'Bool'. If the action returns
-- 'True', then return () in the error monad. If the action returns
-- 'False', throw an error message.
okay :: (MonadError String m, MonadIO m) => String -> IO Bool -> m ()
okay msg m = liftIO m >>= flip when (throwError $ "Failed: "++msg)

-- | A class for things that can be released from OpenCL.
class CLReleasable a where
  -- | Decrement the reference count of a memory object.
  releaseObject :: a -> IO Bool
