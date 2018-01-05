{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs,
             KindSignatures, MultiParamTypeClasses,
             ScopedTypeVariables, TypeApplications, TypeFamilies,
             TypeOperators, UndecidableInstances #-}
-- |Types used for passing arguments to OpenCL kernels, and dealing
-- with asynchronous execution.
module CLUtil.KernelArgs (
  -- * Executing a kernel
  runKernel, runKernelAsync,
  -- * Input arguments
  NumWorkItems(..), WorkGroup(..), KernelArgs,
  -- * Local memory
  LocalMem(..), localFloat, localDouble, localInt, localWord32,
  -- * Output arguments
  OutputSize(..), vectorDup
) where
import CLUtil.Async (CLAsync(..), clAsync, Blockers, getBlockers)
import CLUtil.Buffer (CLBuffer)
import CLUtil.CL (HasCL, ask, liftIO)
import CLUtil.Image (CLImage)
import CLUtil.State (clContext, clQueue, OpenCLState)
import CLUtil.VectorBuffers (withVectorBuffer)
import Control.Monad (void, when)
import Control.Parallel.OpenCL
import Data.Either (partitionEithers)
import Data.Int
import Data.Monoid ((<>))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Void (Void)
import Data.Word
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import GHC.TypeLits
import Linear (V2, V3, V4)

-- | A vector that will be written to by an OpenCL kernel. The
-- parameter is the number of elements in the vector. /NOTE/: The
-- resultant 'V.Vector' produced by 'CLUtil.KernelArgsCL.runKernel'
-- will wrap memory allocated by OpenCL. If you release the OpenCL
-- device (and its associated buffers), things will go sideways. In
-- the case when you want to cleanup an OpenCL context, but hold on to
-- a 'V.Vector' created by an 'OutputSize' kernel parameter, copy the
-- 'V.Vector' with 'vectorDup'.
newtype OutputSize = Out Int

-- | Copy a 'V.Vector': This must be done if you want to release
-- an OpenCL context, but hold on to a result returned by using
-- 'OutputSize' as a parameter to 'CLUtil.KernelArgsCL.runKernel'.
vectorDup :: V.Storable a => V.Vector a -> IO (V.Vector a)
vectorDup v = do v' <- VM.new (V.length v)
                 V.unsafeCopy v' v
                 V.unsafeFreeze v'

-- |The number of global work items to enqueue. May be 1, 2, or 3D.
data NumWorkItems = Work1D Int | Work2D Int Int | Work3D Int Int Int
                    deriving (Eq,Ord,Show)

-- |Convert a 'NumWorkItems' into the format expected by
-- "Control.Parallel.OpenCL".
workItemsList :: NumWorkItems -> [Int]
workItemsList (Work1D n) = [n]
workItemsList (Work2D n m) = [n,m]
workItemsList (Work3D n m o) = [n,m,o]

-- | Specify local workgroup sizes for each dimension.
newtype WorkGroup = WorkGroup { workGroupSizes :: [Int] }

-- |A local memory buffer of the given length. The phantom type
-- encodes the element type of the buffer.
newtype LocalMem a = Local Int

-- |@localFloat n@ creates a local memory buffer of @n@ 'Float's.
localFloat :: Int -> LocalMem Float
localFloat = Local

-- |@localDouble n@ creates a local memory buffer of @n@ 'Doubles's.
localDouble :: Int -> LocalMem Double
localDouble = Local

-- |@localInt n@ creates a local memory buffer of @n@ 'Int's.
localInt :: Int -> LocalMem Int
localInt = Local

-- |@localWord32 n@ creates a local memory buffer @n@ 'Word32's.
localWord32 :: Int -> LocalMem Word32
localWord32 = Local

-- -- |Wraps a 'CLEvent' and a list of cleanup actions to support
-- -- asynchronous kernel executions.
-- data IOAsync = IOAsync { asyncEvent :: CLEvent
--                        , cleanupActions :: [IO ()] }

-- -- |Wait for an asynchronous operation to complete, then cleanup
-- -- associated resources.
-- waitIOAsync :: IOAsync -> IO ()
-- waitIOAsync (IOAsync ev cleanup) = clWaitForEvents [ev] >>
--                                    clReleaseEvent ev >>
--                                    sequence_ cleanup

-- -- |Wait for a list of asynchronous operations to complete, then
-- -- cleanup associated resources.
-- waitIOAsyncs :: [IOAsync] -> IO ()
-- waitIOAsyncs asyncs = do _ <- clWaitForEvents evs
--                          mapM_ clReleaseEvent evs
--                          sequence_ $ concatMap cleanupActions asyncs
--   where evs = map asyncEvent asyncs

-- * Kernel Calls

-- | Execute an OpenCL kernel, blocking until it is complete.
runKernel :: forall a.
  KernelArgs 'KernelSync 'NoWorkGroups 'UnknownWorkItems 'Z a => CLKernel -> a
runKernel k = prepArg k 0 kc
  where kc :: KernelCall 'KernelSync 'NoWorkGroups 'UnknownWorkItems 'Z a
        kc = KernelCall undefined undefined undefined []

-- | Execute an OpenCL kernel, the returned 'CLAsync' value may be
-- passed to 'waitOne' to block until the result is ready.
runKernelAsync :: forall a.
  KernelArgs 'KernelAsync 'NoWorkGroups 'UnknownWorkItems 'Z a => CLKernel -> a
runKernelAsync k = prepArg k 0 kc
  where kc :: KernelCall 'KernelAsync 'NoWorkGroups 'UnknownWorkItems 'Z a
        kc = KernelCall undefined undefined mempty []

-- | What to do with resources associated with a kernel argument that
-- were acquired immediately before the kernel invocation.
data PostExec = ReadOutput (IO (IO (CLEvent, ForeignPtr ()), Int))
              | FreeInput (IO ())

partitionPost :: [PostExec] -> ([IO (IO (CLEvent, ForeignPtr ()), Int)], [IO ()])
partitionPost = partitionEithers . map postToEither
  where postToEither (ReadOutput r) = Left r
        postToEither (FreeInput m) = Right m

data NumOutputs = Z | S NumOutputs

type OneOutput = 'S 'Z
type TwoOutputs = 'S OneOutput
type ThreeOutputs = 'S TwoOutputs

-- | A function that takes a a continuation for preparing a kernel
-- argument and returns an action to be performed after a kernel has
-- executed.
type KernelArgCont = (OpenCLState -> [Int] -> IO (Maybe PostExec, [Int]))
                   -> IO (CLEvent, [PostExec])

-- | To prepare an argument, we take a continuation, perform some IO
-- (to allocate memory, or just grab a pointer), then run the
-- continuation.
type KernelArgPrep = KernelArgCont -> IO (CLEvent, [PostExec])

nestArgPreps :: OpenCLState
             -> [Int]
             -> IO CLEvent
             -> [KernelArgPrep]
             -> IO (CLEvent, [PostExec])
nestArgPreps s outputSizes finish = go [] outputSizes
  where go cleanups [] [] = finish >>= \ev -> return (ev, cleanups)
        go cleanups sizes (prep:preps) =
          prep (\f -> do (cleanup, sizes') <- f s sizes
                         go (maybe cleanups (:cleanups) cleanup)
                            sizes'
                            preps)
        go _ _ _ = error "Impossible: leftover output sizes"

data KernelSized = UnknownWorkItems | KnownWorkItems
data KernelSync = KernelSync | KernelAsync
data KernelWorkGroups = HasWorkGroups | NoWorkGroups

type family WorkGroupField (a :: KernelWorkGroups) where
  WorkGroupField 'HasWorkGroups = WorkGroup
  WorkGroupField 'NoWorkGroups = Void

type family WorkItemsField (a :: KernelSized) where
  WorkItemsField 'UnknownWorkItems = Void
  WorkItemsField 'KnownWorkItems = NumWorkItems

type family BlockersField (a :: KernelSync) where
  BlockersField 'KernelSync = Void
  BlockersField 'KernelAsync = Blockers

data KernelCall (s :: KernelSync)
                (g :: KernelWorkGroups)
                (w :: KernelSized)
                (o :: NumOutputs)
                a =
  KernelCall { kernelWorkGroupSizes :: WorkGroupField g
             , kernelWorkItems      :: WorkItemsField w
             , kernelBlockers       :: BlockersField s
             , kernelArguments      :: [KernelArgPrep]
             }

class KernelWorkGroupSizes (g :: KernelWorkGroups) where
  kernelWorkGroupsMaybe :: KernelCall s g w o a -> Maybe WorkGroup

instance KernelWorkGroupSizes 'HasWorkGroups where
  kernelWorkGroupsMaybe = Just . kernelWorkGroupSizes

instance KernelWorkGroupSizes 'NoWorkGroups where
  kernelWorkGroupsMaybe = const Nothing

setWorkGroups :: KernelCall s 'NoWorkGroups w o a
              -> WorkGroup
              -> KernelCall s 'HasWorkGroups w o b
setWorkGroups kc wg =
  KernelCall wg (kernelWorkItems kc) (kernelBlockers kc) (kernelArguments kc)

setWorkItems :: KernelCall s g 'UnknownWorkItems o a
             -> NumWorkItems
             -> KernelCall s g 'KnownWorkItems o b
setWorkItems kc wi = KernelCall (kernelWorkGroupSizes kc)
                                wi
                                (kernelBlockers kc)
                                (kernelArguments kc)

addKernelArgument :: KernelArgPrep
                  -> KernelCall s g w o a
                  -> KernelCall s g w o b
addKernelArgument prep kc = kc { kernelArguments = prep : kernelArguments kc }

addKernelOutput :: KernelArgPrep
                -> KernelCall s g w o a
                -> KernelCall s g w ('S o) b
addKernelOutput prep kc = kc { kernelArguments = prep : kernelArguments kc }

class KernelBlockers (s :: KernelSync) where
  kernelBlockersList :: KernelCall s g w o a -> [CLEvent]

instance KernelBlockers 'KernelSync where
  kernelBlockersList = const []

instance KernelBlockers 'KernelAsync where
  kernelBlockersList = getBlockers . kernelBlockers

addBlockers :: Blockers
            -> KernelCall 'KernelAsync g w o a
            -> KernelCall 'KernelAsync g w o b
addBlockers bs kc = kc { kernelBlockers = kernelBlockers kc <> bs }

-- | Wrap an output buffer in a 'Vector'.
mkRead :: Storable a => (ForeignPtr (), Int) -> Vector a
mkRead (ptr, num) = V.unsafeFromForeignPtr0 (castForeignPtr ptr) num

-- | Wait for all of our outputs to be mapped, then return the
-- pointers and the vector lengths.
waitForOutputs :: [IO (IO (CLEvent, a), b)] -> IO ([CLEvent], [(a, b)])
waitForOutputs xs = do (actions,nums) <- unzip <$> sequence xs
                       (evs, ptrs) <- unzip <$> sequence actions
                       return (evs, zip ptrs nums)

-- | A type class of things we can pass to OpenCL kernels. This
-- amounts to various types with a 'Storable' instance, 'Vector'
-- values, work group-local memory specifications, the total number of
-- work items to compute, and an optional arrangement of work
-- groups. We can also request the allocation of output buffers, but
-- note that this is somewhat inefficient. The 'Storable' values you
-- can pass are those that have meaning in OpenCL kernels: scalar
-- numbers, small vectors like 'Linear.V2.V2',
-- 'CLUtil.Buffer.CLBuffer' buffer objects, and 'CLUtil.Image.CLImage'
-- images.
class KernelArgs s g w o a where
  prepArg :: CLKernel -> CLuint -> KernelCall s g w o a -> a

stoPrepArg :: (Storable a, KernelArgs s g w o r)
           => CLKernel -> CLuint -> KernelCall s g w o (a -> r) -> a -> r
stoPrepArg k arg prep x = prepArg k (arg+1) (addKernelArgument load prep)
  where load cont = do clSetKernelArgSto k arg x
                       cont (\_ szs -> return (Nothing, szs))

instance KernelArgs s g w o r => KernelArgs s g w o (Float -> r) where
  prepArg = stoPrepArg

instance KernelArgs s g w o r => KernelArgs s g w o (Double -> r) where
  prepArg = stoPrepArg

instance KernelArgs s g w o r => KernelArgs s g w o (Word32 -> r) where
  prepArg = stoPrepArg

instance KernelArgs s g w o r => KernelArgs s g w o (Word16 -> r) where
  prepArg = stoPrepArg

instance KernelArgs s g w o r => KernelArgs s g w o (Word8 -> r) where
  prepArg = stoPrepArg

instance KernelArgs s g w o r => KernelArgs s g w o (Int32 -> r) where
  prepArg = stoPrepArg

instance KernelArgs s g w o r => KernelArgs s g w o (Int16 -> r) where
  prepArg = stoPrepArg

instance KernelArgs s g w o r => KernelArgs s g w o (Int8 -> r) where
  prepArg = stoPrepArg

instance (Storable a, KernelArgs s g w o r) =>
  KernelArgs s g w o (V2 a -> r) where
  prepArg = stoPrepArg

instance (Storable a, KernelArgs s g w o r) =>
  KernelArgs s g w o (V3 a -> r) where
  prepArg = stoPrepArg

instance (Storable a, KernelArgs s g w o r) =>
  KernelArgs s g w o (V4 a -> r) where
  prepArg = stoPrepArg

instance KernelArgs s g w o r => KernelArgs s g w o (CLBuffer a -> r) where
  prepArg = stoPrepArg

instance KernelArgs s g w o r => KernelArgs s g w o (CLImage n a -> r) where
  prepArg = stoPrepArg

-- We make unsafe use of Vector arguments by passing pointers directly
-- to OpenCL. Perhaps we should wait for the kernel to finish before
-- returning a result when using a Vector in that way? We can simply
-- restrict Vector arguments to synchronous kernel evaluations. We
-- should have a TypeError context for an instance supporting Vector
-- arguments for Async kernel evaluations.

kernelCPS :: [Int] -> OpenCLState -> CLKernel -> NumWorkItems -> Maybe WorkGroup
          -> [CLEvent] -> [KernelArgPrep]
          -> IO (CLEvent, [PostExec])
kernelCPS outputSizes s k wi wg bs prep =
  nestArgPreps s outputSizes runK prep
  where runK = clEnqueueNDRangeKernel (clQueue s)
                                      k
                                      (workItemsList wi)
                                      (maybe [] workGroupSizes wg)
                                      bs

-- | Helper for calling kernelCPS from the terminal KernelArgs instances
auxKernelCPS :: (KernelBlockers s, KernelWorkGroupSizes g)
             => [Int] -> OpenCLState -> CLKernel
             -> KernelCall s g 'KnownWorkItems o a
             -> IO (CLEvent, [PostExec])
auxKernelCPS outputSizes s k prep = kernelCPS outputSizes s k
                                              (kernelWorkItems prep)
                                              (kernelWorkGroupsMaybe prep)
                                              (kernelBlockersList prep)
                                              (kernelArguments prep)

instance {-# OVERLAPPABLE #-} (HasCL m, KernelWorkGroupSizes g) =>
  KernelArgs 'KernelSync g 'KnownWorkItems 'Z (m ()) where
  prepArg k _ prep = ask >>= \s ->
    liftIO $ do
      (ev,posts) <- auxKernelCPS [] s k prep
      let (o, cleanup) = partitionPost posts
      when (not (null o)) (error "Impossible: Outputs aren't bound 0!")
      _ <- clWaitForEvents [ev]
      sequence_ cleanup

instance {-# OVERLAPPABLE #-} (HasCL m, KernelWorkGroupSizes g) =>
  KernelArgs 'KernelAsync g 'KnownWorkItems 'Z (m (CLAsync ())) where
  prepArg k _ prep = ask >>= \s ->
    liftIO $ do
      (ev,posts) <- auxKernelCPS [] s k prep
      let (o, cleanup) = partitionPost posts
      when (not (null o)) (error "Impossible: Outputs aren't bound 0!")
      sequence_ cleanup
      return $ clAsync ev (pure ())

instance {-# OVERLAPPABLE #-} forall a m g.
  (Storable a, HasCL m, KernelWorkGroupSizes g) =>
  KernelArgs 'KernelAsync g 'KnownWorkItems OneOutput (m (CLAsync (Vector a))) where
  prepArg k _ prep = ask >>= \s ->
    liftIO $ do
      (ev,posts) <- auxKernelCPS [sizeOf (undefined::a)] s k prep
      let (o, cleanup) = partitionPost posts
      (evs, r1) <- case o of
                     [f] -> do (evs, [r]) <- waitForOutputs [f]
                               return (evs, mkRead r)
                     _ -> error "Impossible: Expected one output"
      sequence_ cleanup
      return $ CLAsync ((ev:evs), pure r1)

instance {-# OVERLAPPABLE #-} forall a b m g.
  (Storable a, Storable b, HasCL m, KernelWorkGroupSizes g) =>
  KernelArgs 'KernelAsync g 'KnownWorkItems TwoOutputs
             (m (CLAsync (Vector a, Vector b))) where
  prepArg k _ prep = ask >>= \s ->
    liftIO $ do
      (ev,posts) <- auxKernelCPS [sizeOf (undefined::a)] s k prep
      let (o, cleanup) = partitionPost posts
      (evs, r1, r2) <-
        case o of
          [f,g] -> do (evs, [ptr1, ptr2]) <- waitForOutputs [f,g]
                      return (evs, mkRead ptr1, mkRead ptr2)
          _ -> error "Impossible: Expected two outputs"
      sequence_ cleanup
      return $ CLAsync ((ev:evs), pure (r1,r2))

instance {-# OVERLAPPABLE #-} forall a b c m g.
  (Storable a, Storable b, Storable c, HasCL m, KernelWorkGroupSizes g) =>
  KernelArgs 'KernelAsync g 'KnownWorkItems ThreeOutputs
             (m (CLAsync (Vector a, Vector b, Vector c))) where
  prepArg k _ prep = ask >>= \s ->
    liftIO $ do
      (ev,posts) <- auxKernelCPS [sizeOf (undefined::a)] s k prep
      let (o, cleanup) = partitionPost posts
      (evs, r1, r2, r3) <-
        case o of
          [f,g, h] -> do (evs, [ptr1, ptr2, ptr3]) <- waitForOutputs [f,g,h]
                         return (evs, mkRead ptr1, mkRead ptr2, mkRead ptr3)
          _ -> error "Impossible: Expected three outputs"
      sequence_ cleanup
      return $ CLAsync ((ev:evs), pure (r1,r2,r3))

instance {-# OVERLAPPABLE #-}
  (Storable a, HasCL m, KernelWorkGroupSizes g) =>
  KernelArgs 'KernelSync g 'KnownWorkItems OneOutput (m (Vector a)) where
  prepArg k _ prep = ask >>= \s ->
    liftIO $ do
      (ev,posts) <- auxKernelCPS [sizeOf (undefined :: a)] s k prep
      let (o, cleanup) = partitionPost posts
      (evs, r1) <- case o of
                     [f] -> do (evs, [r]) <- waitForOutputs [f]
                               return (evs, mkRead r)
                     _ -> error "Impossible: Expected one output"
      sequence_ cleanup
      r1 <$ clWaitForEvents (ev:evs)

instance {-# OVERLAPPABLE #-}
  (Storable a, Storable b, HasCL m, KernelWorkGroupSizes g) =>
  KernelArgs 'KernelSync g 'KnownWorkItems TwoOutputs (m (Vector a, Vector b)) where
  prepArg k _ prep = ask >>= \s ->
    liftIO $ do
      (ev,posts) <- auxKernelCPS [ sizeOf (undefined :: a)
                                 , sizeOf (undefined :: b) ]
                                 s k prep
      let (o, cleanup) = partitionPost posts
      (evs, r1, r2) <- case o of
                         [f,g] -> do (evs, [ptr1, ptr2]) <- waitForOutputs [f,g]
                                     return (evs, mkRead ptr1, mkRead ptr2)
                         _ -> error "Impossible: Expected two outputs"
      sequence_ cleanup
      (r1, r2) <$ clWaitForEvents (ev:evs)

instance {-# OVERLAPPABLE #-}
  (Storable a, Storable b, Storable c, HasCL m, KernelWorkGroupSizes g) =>
  KernelArgs 'KernelSync g 'KnownWorkItems ThreeOutputs
             (m (Vector a, Vector b, Vector c)) where
  prepArg k _ prep = ask >>= \s ->
    liftIO $ do
      (ev,posts) <- auxKernelCPS [ sizeOf (undefined :: a)
                                 , sizeOf (undefined :: b)
                                 , sizeOf (undefined :: c) ]
                                 s k prep
      let (o, cleanup) = partitionPost posts
      (evs, r1, r2, r3) <-
        case o of
          [f,g,h] -> do (evs, [ptr1, ptr2, ptr3]) <- waitForOutputs [f,g,h]
                        return (evs, mkRead ptr1, mkRead ptr2, mkRead ptr3)
          _ -> error "Impossible: Expected three outputs"
      sequence_ cleanup
      (r1, r2, r3) <$ clWaitForEvents (ev:evs)

instance KernelArgs s g 'KnownWorkItems o r =>
  KernelArgs s g 'UnknownWorkItems o (NumWorkItems -> r) where
  prepArg k arg prep = \n -> prepArg k arg (setWorkItems prep n)

instance KernelArgs s 'HasWorkGroups w o r =>
  KernelArgs s 'NoWorkGroups w o (WorkGroup -> r) where
  prepArg k arg prep = \n -> prepArg k arg (setWorkGroups prep n)

instance (Storable a, KernelArgs s g w o r) =>
  KernelArgs s g w o (LocalMem a -> r) where
  prepArg k arg prep =
    \(Local m) -> let sz = m * sizeOf (undefined :: a)
                      local cont = do clSetKernelArg k arg sz nullPtr
                                      cont (\_ szs -> return (Nothing, szs))
                  in prepArg k (arg+1) (addKernelArgument local prep)

instance KernelArgs 'KernelAsync g w o r =>
  KernelArgs 'KernelAsync g w o (Blockers -> r) where
  prepArg k arg prep bs = prepArg k arg (addBlockers bs prep)

instance (KernelArgs 'KernelSync g w o r, Storable a) =>
  KernelArgs 'KernelSync g w o (Vector a -> r) where
  prepArg k arg prep v =
    let load cont =
          cont $ \s szs ->
            withVectorBuffer s v $ \b ->
              let cleanup = FreeInput (void (clReleaseMemObject b))
              in do clSetKernelArgSto k arg b
                    return (Just cleanup, szs)
    in prepArg k (arg+1) (addKernelArgument load prep)

-- A finalizer we attach to the 'ForeignPtr' wrapping a mapped OpenCL
-- memory buffer. This lets us wrap the mapped buffer in a 'Vector',
-- then release it when the 'Vector' is GC'ed.
bufferFinalizer :: CLCommandQueue -> CLMem -> Ptr () -> IO ()
bufferFinalizer q b p = do clEnqueueUnmapMemObject q b p [] >>=
                             -- clWaitForEvents . (:[])
                             void . clReleaseEvent
                           void $ clReleaseMemObject b

instance {-# OVERLAPPING #-} KernelArgs s g w ('S o) r =>
  KernelArgs s g w o (OutputSize -> r) where
  prepArg k arg prep (Out m) = prepArg k (arg+1) (addKernelOutput load prep)
    where load cont = cont allocateOutput
          allocateOutput _ [] = error "No output element size available"
          allocateOutput s (sz:szs) =
            do b <- clCreateBuffer (clContext s)
                                   [ CL_MEM_WRITE_ONLY
                                   , CL_MEM_ALLOC_HOST_PTR ]
                                   (m*sz, nullPtr)
               clSetKernelArgSto k arg b

               -- The goal is to map the output buffer, and wrap that
               -- pointer in a Vector. This would mean that the Vector
               -- is wrapping memory allocated by OpenCL.
               let q = clQueue s
                   getPtr = do (ev,p) <- clEnqueueMapBuffer q b
                                                            True [CL_MAP_READ]
                                                            0 (m*sz) []
                               ptr <- newForeignPtr p (bufferFinalizer q b p)
                               return (ev, ptr)
                   finishOutput = return (getPtr, m)
               return (Just (ReadOutput finishOutput), szs)

-- * Custom Type Errors

instance (TypeError ('Text "Number of work items not specified" ':$$:
                     'Text "You must supply a NumWorkItems argument to run an OpenCL kernel")) =>
  KernelArgs s g 'UnknownWorkItems o (m ()) where
  prepArg = error "unreachable"

instance (TypeError ('Text "Number of work items not specified" ':$$:
                     'Text "You must supply a NumWorkItems argument to run an OpenCL kernel")) =>
  KernelArgs s g 'UnknownWorkItems o (m (Vector a)) where
  prepArg = error "unreachable"

instance (TypeError ('Text "Number of work items not specified" ':$$:
                     'Text "You must supply a NumWorkItems argument to run an OpenCL kernel")) =>
  KernelArgs s g 'UnknownWorkItems o (m (Vector a, Vector b)) where
  prepArg = error "unreachable"

instance (TypeError ('Text "Number of work items not specified" ':$$:
                     'Text "You must supply a NumWorkItems argument to run an OpenCL kernel")) =>
  KernelArgs s g 'UnknownWorkItems o (m (Vector a, Vector b, Vector c)) where
  prepArg = error "unreachable"

instance (TypeError ('Text "Number of work items not specified" ':$$:
                     'Text "You must supply a NumWorkItems argument to run an OpenCL kernel")) =>
  KernelArgs s g 'UnknownWorkItems o (m (CLAsync ())) where
  prepArg = error "unreachable"

instance (TypeError ('Text "Number of work items not specified" ':$$:
                     'Text "You must supply a NumWorkItems argument to run an OpenCL kernel")) =>
  KernelArgs s g 'UnknownWorkItems o (m (CLAsync (Vector a))) where
  prepArg = error "unreachable"

instance (TypeError ('Text "Number of work items not specified" ':$$:
                     'Text "You must supply a NumWorkItems argument to run an OpenCL kernel")) =>
  KernelArgs s g 'UnknownWorkItems o (m (CLAsync (Vector a, Vector b))) where
  prepArg = error "unreachable"

instance (TypeError ('Text "Number of work items not specified" ':$$:
                     'Text "You must supply a NumWorkItems argument to run an OpenCL kernel")) =>
  KernelArgs s g 'UnknownWorkItems o (m (CLAsync (Vector a, Vector b, Vector c))) where
  prepArg = error "unreachable"

instance (TypeError ('Text "You can only set the number of work items once"))
         => KernelArgs s g 'KnownWorkItems o (NumWorkItems -> r) where
  prepArg = error "unreachable"

instance (TypeError ('Text "You can only set the number of work groups once"))
         => KernelArgs s 'HasWorkGroups w o (WorkGroup -> r) where
  prepArg = error "unreachable"

instance (TypeError ('Text "Vector arguments may only be directly used with synchronous kernel executions" ':$$:
                     'Text "For asynchronous kernel execution, copy the Vector into a buffer object (e.g. using 'initBuffer').")) =>
  KernelArgs 'KernelAsync g w o (Vector a -> r) where
  prepArg = error "unreachable"

instance (TypeError ('ShowType ('S n) ':<>: 'Text " outputs were requested, but none are returned")) =>
  KernelArgs 'KernelSync g 'KnownWorkItems ('S n) (m ()) where
  prepArg = error "unreachable"

type family PeanoToNat (p :: NumOutputs) where
  PeanoToNat 'Z = 0
  PeanoToNat ('S n) = 1 + PeanoToNat n

instance {-# OVERLAPPABLE #-} (TypeError ('ShowType (PeanoToNat n) ':<>: 'Text " outputs were requested, but one is returned")) =>
  KernelArgs 'KernelSync g 'KnownWorkItems n (m (Vector r1)) where
  prepArg = error "unreachable"


instance {-# OVERLAPPABLE #-} (TypeError ('ShowType (PeanoToNat n) ':<>: 'Text " outputs were requested, but two are returned")) =>
  KernelArgs 'KernelSync g 'KnownWorkItems n (m (Vector r1, Vector r2)) where
  prepArg = error "unreachable"

instance {-# OVERLAPPABLE #-} (TypeError ('ShowType (PeanoToNat n) ':<>: 'Text " outputs were requested, but three are returned")) =>
  KernelArgs 'KernelSync g 'KnownWorkItems n (m (Vector r1, Vector r2, Vector r3)) where
  prepArg = error "unreachable"
