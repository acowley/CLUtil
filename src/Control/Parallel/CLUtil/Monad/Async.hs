{-# LANGUAGE GADTs, DataKinds, KindSignatures, 
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeFamilies, TypeOperators #-}
-- | Utilities for dealing with asynchronous results. If a 'CLEvent'
-- is paired with a 'CL' action that produces a result, then we can
-- wait on a list of events, then return a list of results with the
-- 'waitAll' function. If our asynchronous computations produce values
-- of differing types, then we can make use of a heterogenous list to
-- accumulate these promised results and to represent the results.
module Control.Parallel.CLUtil.Monad.Async 
  (HList(..), (<+>), (++), 
   waitAll, waitAll', waitAll_, waitAllUnit, waitOne, CLAsync) where
import Control.Applicative
import Control.Parallel.OpenCL
import Control.Parallel.CLUtil.Monad.CL
import Foreign.Ptr (nullPtr)

-- | A basic heterogenous list type.
data HList :: [*] -> * where
  HNil :: HList '[]
  (:&) :: t -> HList ts -> HList (t ': ts)
infixr 5 :&

-- | A 'CLEvent' that will fire when the result of an associated 'CL'
-- computation is ready.
type CLAsync a = (CLEvent, CL a)

-- | Block until the results of all given 'CL' actions are ready. Each
-- action must produce the same type of value.
waitAll :: [CLAsync a] -> CL [a]
waitAll = aux . unzip
  where aux (evs,xs) = 
          do liftIO $ clWaitForEvents evs >> mapM_ clReleaseEvent evs
             -- okay "Waiting for events" $ clWaitForEvents evs
             -- mapM_ (okay "Releasing event" . clReleaseEvent) evs
             sequence xs

-- | Block until the results of all given 'CL' actions are ready, then
-- discard all of those results.
waitAll_ :: [CLAsync a] -> CL ()
waitAll_ = aux . unzip
  where aux (evs,xs) = do liftIO $ do clWaitForEvents evs
                                      mapM_ clReleaseEvent evs
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
  getEvents :: HList rs -> [CLEvent]

instance HasEvents '[] where
  getEvents _ = []

instance HasEvents ts => HasEvents ((CLEvent,a) ': ts) where
  getEvents ((e,_) :& xs) = e : getEvents xs

class SequenceH m ts where
  sequenceH :: Applicative m => HList ts -> m (HList (Sequence (MapSnd ts)))

instance SequenceH m '[] where
  sequenceH _ = pure HNil

instance SequenceH m ts => SequenceH m ((e, m a) ': ts) where
  sequenceH ((_,x) :& xs) = (:&) <$> x <*> sequenceH xs

-- | Block until the results of all given 'CL' actions are ready. Each
-- action may produce a different type of value.
waitAll' :: (SequenceH CL xs, HasEvents xs)
         => HList xs -> CL (HList (Sequence (MapSnd xs)))
waitAll' xs = do liftIO $ clWaitForEvents evs >> mapM_ clReleaseEvent evs
                 -- okay "Waiting for events" $ clWaitForEvents evs
                 -- mapM_ (okay "Releasing event" . clReleaseEvent) evs
                 sequenceH xs
  where evs = filter (/= nullPtr) $ getEvents xs
