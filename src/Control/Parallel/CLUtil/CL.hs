{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
-- | A monad transformer stack for working with OpenCL. Includes a
-- lightweight resource management layer in the style of the
-- @resourcet@ package.
module Control.Parallel.CLUtil.CL 
  (-- * Running OpenCL computations
   CL, runCL, runCL', runCLIO, runCLError, runCLClean,
   -- * Operations in the @CL@ monad
   ask, throwError, liftIO, okay, getKernel,
   -- * Managing resources
   registerCleanup, unregisterCleanup, runCleanup, cleanupAll, ReleaseKey,
   -- * Releasable objects
   CLReleasable(..),
   -- * Internal types
   Cleanup(..), CLState(..)
  ) where
import Control.Applicative
import Control.Lens
import Control.Monad.Error (ErrorT(..))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Parallel.CLUtil.ProgramCache as C
import Control.Parallel.CLUtil.State (OpenCLState)
import Control.Parallel.OpenCL (CLKernel)
import Data.Foldable (sequenceA_)
import Data.IntMap.Strict (IntMap)
import Data.Monoid

-- | Release resources used by OpenCL.
data Cleanup = Cleanup { _nextReleaseKey :: !Int
                       , _releaseMap     :: !(IntMap (IO ()))  }
makeLenses ''Cleanup

type ReleaseKey = Int

newCleanup :: Cleanup
newCleanup = Cleanup 0 mempty

data CLState = CLState { _clCleanup :: !Cleanup
                       , _clCache   :: !C.Cache }
makeLenses ''CLState

freshState :: CLState
freshState = CLState newCleanup C.emptyCache

-- | Run all remaining cleanup actions.
cleanupAll :: Cleanup -> IO ()
cleanupAll (Cleanup _ m) = sequenceA_ m

-- | Get a kernel given a program file name and a kernel name. If the
-- kernel was already loaded, it is returned. If not, and the program
-- was previously loaded, the loaded program is used to provide the
-- requested kernel. If the program has not yet been loaded, it is
-- loaded from the source file.
getKernel :: String -> String -> CL CLKernel
getKernel f k = zoom clCache (C.getKernel f k)

-- | The 'CL' type captures OpenCL context as well as resource
-- initialization and cleanup helpers. For initialization, OpenCL
-- program compilations are cached by 'CL' values to help with
-- decoupling initialization steps that may refer to the same OpenCL
-- source files. Cleanup actions are captured in a map of actions
-- built from every action supplied to 'registerCleanup'; this action
-- is returned by 'runCL' and 'runCLError'. Note that the
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
type CL = ReaderT OpenCLState (StateT CLState (ErrorT String IO))

-- | Register a cleanup action. NOTE: The return value of the supplied
-- action is discarded. A composite cleanup action is returned by
-- calls to 'runCL' or 'runCLError'.
registerCleanup :: IO () -> CL ReleaseKey
registerCleanup m = zoom clCleanup $
                    do i <- use nextReleaseKey
                       releaseMap . at i .= Just m
                       nextReleaseKey += 1
                       return i

-- | Run a previously-registered cleanup action. It will not be run again.
runCleanup :: ReleaseKey -> CL ()
runCleanup k = do use (clCleanup . releaseMap . at k) >>= liftIO . sequenceA_
                  clCleanup . releaseMap . at k .= Nothing

-- | Return a previously-registered cleanup action without running
-- it. This returns control over this resource to the caller.
unregisterCleanup :: ReleaseKey -> CL (Maybe (IO ()))
unregisterCleanup k = do m <- use (clCleanup . releaseMap . at k)
                         clCleanup . releaseMap . at k .= Nothing
                         return m

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error'.
runCL :: OpenCLState -> CL a -> IO (a, Cleanup)
runCL s m = runErrorT (runStateT (runReaderT m s) freshState) >>=
            either error (return . over _2 _clCleanup)

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error'. This function behaves identically to
-- 'runCL' with the exception that the 'Cleanup' action is returned as
-- an unadorned 'IO' action. This may help isolate callers from any
-- awareness of OpenCL, but makes the types a bit more ambiguous.
runCLIO :: OpenCLState -> CL a -> IO (a, IO ())
runCLIO s m = aux <$> runCL s m
  where aux (r,c) = (r, cleanupAll c)

-- | Run a 'CL' action with a given 'OpenCLState'. Any errors are
-- raised by calling 'error', and all cleanup actions are run before
-- returning the result of the computation.
runCLClean :: OpenCLState -> CL a -> IO a
runCLClean s m = runCLIO s m >>= (\(x,c) -> x <$ c)

-- | Run a 'CL' action that discards any accumulated cleanup action.
runCL' :: OpenCLState -> CL a -> IO a
runCL' = (fmap fst .) . runCL

-- | Run a 'CL' action with a given 'OpenCLState'. The result is
-- 'Either' an error message or the result of the action.
runCLError :: OpenCLState -> CL a -> IO (Either String (a, Cleanup))
runCLError s m = fmap (over _2 _clCleanup) <$>
                 runErrorT (runStateT (runReaderT m s) freshState)

-- | Run an IO action that returns a 'Bool'. If the action returns
-- 'True', then return () in the error monad. If the action returns
-- 'False', throw an error message.
okay :: (MonadError String m, MonadIO m) => String -> IO Bool -> m ()
okay msg m = liftIO m >>= flip when (throwError $ "Failed: "++msg)

-- | A class for things that can be released from OpenCL.
class CLReleasable a where
  -- | Decrement the reference count of a memory object.
  releaseObject :: a -> IO Bool
