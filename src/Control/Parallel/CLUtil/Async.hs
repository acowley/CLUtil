{-# LANGUAGE GADTs, DataKinds, KindSignatures, 
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeFamilies, TypeOperators #-}
-- | Utilities for dealing with asynchronous results. If a 'CLEvent'
-- is paired with a 'CL' action that produces a result, then we can
-- wait on a list of events, then return a list of results with the
-- 'waitAll' function. If our asynchronous computations produce values
-- of differing types, then we can make use of a heterogenous list to
-- accumulate these promised results and to represent the results.
module Control.Parallel.CLUtil.Async 
  (HList(..), (<+>), (++), (&:), singAsync, waitReleaseEvent,
   waitAll, waitAll', waitAll_, waitAllUnit, waitOne, CLAsync,
   clAsync, ioAsync, sequenceAsync) where
import Control.Applicative
import Control.Arrow (first)
import Control.Parallel.OpenCL
import Control.Parallel.CLUtil.CL
import Data.Monoid
import Foreign.Ptr (nullPtr)

-- | A basic heterogenous list type.
data HList :: [*] -> * where
  HNil :: HList '[]
  (:&) :: t -> HList ts -> HList (t ': ts)
infixr 5 :&

-- | A 'CLEvent' that will fire when the result of an associated 'CL'
-- computation is ready.
-- type CLAsync a = ([CLEvent], CL a)
type CLAsync a = ([IO ()], CL a)

-- | Wait for an event, then immediately release it, decrementing its
-- reference count.
waitReleaseEvent :: CLEvent -> IO ()
waitReleaseEvent ev = clWaitForEvents [ev] >> clReleaseEvent ev >> return ()

-- | Constructor for a simple, single event 'CLAsync'.
clAsync :: CLEvent -> CL a -> CLAsync a
clAsync = (,) . pure . waitReleaseEvent

-- | Make a 'CLAsync' that waits for an opaque 'IO' action.
ioAsync :: IO () -> CL a -> CLAsync a
ioAsync = (,) . pure

-- | Sequence two 'CLAsync's, returning the result of the second.
sequenceAsync :: CLAsync () -> CLAsync b -> CLAsync b
sequenceAsync (ev1, r1) (ev2, r2) = (ev1<>ev2, r1 >> r2)

-- | Helper for lifting 'HList''s cons operation into an
-- 'Applicative'.
(&:) :: Applicative m => m a -> m (HList bs) -> m (HList (a ': bs))
x &: xs = (:&) <$> x <*> xs
infixr 5 &:

-- | Helper for producing a single-element 'HList' from a 'CLAsync'.
singAsync :: CL (CLAsync a) -> CL (HList '[CLAsync a])
singAsync e = (:&) <$> e <*> pure HNil

-- | Block until the results of all given 'CL' actions are ready. Each
-- action must produce the same type of value.
waitAll :: [CLAsync a] -> CL [a]
waitAll = aux . first concat . unzip
  where aux (evs,xs) = 
          do liftIO $ sequence_ evs
             -- okay "Waiting for events" $ clWaitForEvents evs
             -- mapM_ (okay "Releasing event" . clReleaseEvent) evs
             sequence xs

-- | Block until the results of all given 'CL' actions are ready, then
-- discard all of those results.
waitAll_ :: [CLAsync a] -> CL ()
waitAll_ = aux . first concat . unzip
  where aux (evs,xs) = do liftIO $ sequence_ evs
                          -- okay "Waiting for events" $ clWaitForEvents evs
                          -- mapM_ (okay "Releasing event" . clReleaseEvent) evs
                          sequence_ xs

-- | Block until all the given 'CL' actions have finished. All actions
-- are being run solely for their side effects. This specialization of
-- 'waitAll_' is intended to help type inference determine the result
-- of kernel invocations.
waitAllUnit :: [CLAsync ()] -> CL ()
waitAllUnit = waitAll_

-- | Block on a single asynchronous computation.
waitOne :: CLAsync a -> CL a
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
type instance MapSnd ((t,a) ': as) = a ': MapSnd as

type family Sequence (as :: [*]) :: [*]
type instance Sequence '[] = '[]
type instance Sequence (m a ': as) = a ': Sequence as

class HasEvents rs where
  getEvents :: HList rs -> [IO ()]

instance HasEvents '[] where
  getEvents _ = []

instance HasEvents ts => HasEvents (CLAsync a ': ts) where
  getEvents ((e,_) :& xs) = e ++ getEvents xs

class SequenceH ts where
  sequenceH :: HList ts -> CL (HList (Sequence (MapSnd ts)))

instance SequenceH '[] where
  sequenceH _ = pure HNil

instance SequenceH ts => SequenceH (CLAsync a ': ts) where
  sequenceH ((_,x) :& xs) = (:&) <$> x <*> sequenceH xs

-- | Block until the results of all given 'CL' actions are ready. Each
-- action may produce a different type of value.
waitAll' :: (SequenceH xs, HasEvents xs)
         => HList xs -> CL (HList (Sequence (MapSnd xs)))
waitAll' xs = do liftIO $ sequence_ (getEvents xs)
                 -- okay "Waiting for events" $ clWaitForEvents evs
                 -- mapM_ (okay "Releasing event" . clReleaseEvent) evs
                 sequenceH xs
