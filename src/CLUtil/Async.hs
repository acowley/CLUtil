{-# LANGUAGE GADTs, DataKinds, KindSignatures,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeFamilies, TypeOperators #-}
-- | Utilities for dealing with asynchronous results. If a 'CLEvent'
-- is paired with a 'CL' action that produces a result, then we can
-- wait on a list of events, then return a list of results with the
-- 'waitAll' function. If our asynchronous computations produce values
-- of differing types, then we can make use of a heterogenous list to
-- accumulate these promised results and to represent the results.
module CLUtil.Async
  (HList(..), (<+>), (++), (&:), singAsync, waitReleaseEvent,
   waitAll, waitAll', waitAll_, waitAllUnit, waitOne, CLAsync(..),
   clAsync, sequenceAsync,
   Blockers, blocker, getBlockers, releaseBlockers) where
import Control.Arrow (first)
import Control.Monad.IO.Class
import Control.Parallel.OpenCL
import Data.Monoid

-- | A basic heterogenous list type.
data HList :: [*] -> * where
  HNil :: HList '[]
  (:&) :: t -> HList ts -> HList (t ': ts)
infixr 5 :&

-- | A 'CLEvent' that will fire when the result of an associated 'CL'
-- computation is ready.
newtype CLAsync a = CLAsync { getCLAsync :: ([CLEvent], IO a) }

-- | Require that a kernel invocation wait until some set of
-- previously-started operations have finished.
type Blockers = CLAsync ()

blocker :: CLEvent -> Blockers
blocker = flip clAsync (return ())

getBlockers :: Blockers -> [CLEvent]
getBlockers = fst . getCLAsync

-- | Wait for and release 'Blockers'
releaseBlockers :: Blockers -> IO ()
releaseBlockers bs = let evs = getBlockers bs
                     in clWaitForEvents evs >> mapM_ clReleaseEvent evs

instance Monoid (CLAsync ()) where
  mempty = CLAsync ([], return ())
  mappend = sequenceAsync

instance Functor CLAsync where
  fmap f (CLAsync x) = CLAsync $ fmap (fmap f) x

-- | Wait for an event, then immediately release it, decrementing its
-- reference count.
waitReleaseEvent :: CLEvent -> IO ()
waitReleaseEvent ev = clWaitForEvents [ev] >> clReleaseEvent ev >> return ()

-- | Constructor for a simple, single event 'CLAsync'.
clAsync :: CLEvent -> IO a -> CLAsync a
clAsync ev m = CLAsync ([ev], m)

-- | Sequence two 'CLAsync's, returning the result of the second.
sequenceAsync :: CLAsync () -> CLAsync b -> CLAsync b
sequenceAsync (CLAsync (ev1, r1)) (CLAsync (ev2, r2)) =
  CLAsync (ev1<>ev2, r1 >> r2)

-- | Helper for lifting 'HList''s cons operation into an
-- 'Applicative'.
(&:) :: Applicative m => m a -> m (HList bs) -> m (HList (a ': bs))
x &: xs = (:&) <$> x <*> xs
infixr 5 &:

-- | Helper for producing a single-element 'HList' from a 'CLAsync'.
singAsync :: Applicative m => m (CLAsync a) -> m (HList '[CLAsync a])
singAsync e = (:&) <$> e <*> pure HNil

-- | Block until the results of all given 'CL' actions are ready. Each
-- action must produce the same type of value. Events are released.
waitAll :: MonadIO m => [CLAsync a] -> m [a]
waitAll = aux . first concat . unzip . map getCLAsync
  where aux (evs,xs) = liftIO $ do _ <- clWaitForEvents evs
                                   mapM_ clReleaseEvent evs
                                   sequence xs

-- | Block until the results of all given 'CL' actions are ready, then
-- discard all of those results. Events are released
waitAll_ :: MonadIO m => [CLAsync a] -> m ()
waitAll_ = aux . first concat . unzip . map getCLAsync
  where aux (evs,xs) = do liftIO $ do _ <- clWaitForEvents evs
                                      mapM_ clReleaseEvent evs
                                      sequence_ xs

-- | Block until all the given 'CL' actions have finished. All actions
-- are being run solely for their side effects. This specialization of
-- 'waitAll_' is intended to help type inference determine the result
-- of kernel invocations. Events are released.
waitAllUnit :: MonadIO m => [CLAsync ()] -> m ()
waitAllUnit = waitAll_

-- | Block on a single asynchronous computation. The 'CLEvent' is
-- released.
waitOne :: MonadIO m => CLAsync a -> m a
waitOne = fmap head . waitAll . (:[])

type family (as :: [*]) ++ (bs :: [*]) :: [*]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs = a ': (as ++ bs)

-- | Append 'HList's.
(<+>) :: HList as -> HList bs -> HList (as ++ bs)
HNil <+> bs = bs
(a :& as) <+> bs = a :& (as <+> bs)
infixr 5 <+>

type family MapSnd (as :: [*]) :: [*]
type instance MapSnd '[] = '[]
type instance MapSnd (CLAsync a ': as) = IO a ': MapSnd as

type family Sequence (as :: [*]) :: [*]
type instance Sequence '[] = '[]
type instance Sequence (m a ': as) = a ': Sequence as

class HasEvents rs where
  getEvents :: HList rs -> [CLEvent]

instance HasEvents '[] where
  getEvents _ = []

instance HasEvents ts => HasEvents (CLAsync a ': ts) where
  getEvents (CLAsync (e,_) :& xs) = e ++ getEvents xs

class SequenceH ts where
  sequenceH :: HList ts -> IO (HList (Sequence (MapSnd ts)))

instance SequenceH '[] where
  sequenceH _ = pure HNil

instance SequenceH ts => SequenceH (CLAsync a ': ts) where
  sequenceH (CLAsync (_,x) :& xs) = (:&) <$> x <*> sequenceH xs

-- | Block until the results of all given 'CL' actions are ready. Each
-- action may produce a different type of value.
waitAll' :: (SequenceH xs, HasEvents xs, MonadIO m)
         => HList xs -> m (HList (Sequence (MapSnd xs)))
waitAll' xs = liftIO $ let evs = getEvents xs
                       in do _ <- clWaitForEvents evs
                             mapM_ clReleaseEvent evs
                             sequenceH xs
