{-# LANGUAGE RankNTypes, FlexibleInstances, OverlappingInstances #-}
-- |Asynchronous kernel execution /without copying input
-- vectors/. Avoiding the input copies improves performance, but is
-- unsafe as it means the OpenCL driver has a pointer to 'Vector'
-- data.
module Control.Parallel.CLUtil.KernelArgsCPSAsync 
  (KernelArgsCPSAsync, unsafeRunKernelCPSAsync) where
import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow (second)
import Control.Monad (void, when)
import Data.Either (partitionEithers)
import Data.Vector.Storable (Vector)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Storable (Storable)
import Control.Parallel.CLUtil.KernelArgTypes
import Control.Parallel.CLUtil.State
import Control.Parallel.CLUtil.VectorBuffers
import Control.Parallel.OpenCL

data PostExec = ReadOutput (Int -> IO (CLMem,Int))
              | FreeInput (IO ())

postToEither :: PostExec -> Either (Int -> IO (CLMem, Int)) (IO ())
postToEither (ReadOutput r) = Left r
postToEither (FreeInput m) = Right m

partitionPost :: [PostExec] -> ([Int -> IO (CLMem, Int)], [IO ()])
partitionPost = partitionEithers . map postToEither

-- The continuation of a preparation action.
type PrepCont a = ([PostExec] -> [PostExec]) -> IO (a, [PostExec])

-- The preparation action typically runs some IO action, then has the
-- option to update the list of PostExec actions if it chooses to.
type PrepExec a = PrepCont a -> IO (a, [PostExec])

class KernelArgsCPSAsync a where
  -- Identical to the setArg method of the KernelArgs class with the
  -- addition of a list of 'CLAsync' events to wait on before
  -- executing the kernel.
  setArg :: OpenCLState -> CLKernel -> CLuint -> Maybe NumWorkItems ->
            [CLAsync] -> (forall b. [PrepExec b]) -> a

-- Nest a stack of buffer preparation actions. This lets us directly
-- access the pointers underlying 'Vector' arguments without an extra
-- copy.
nestM :: IO a -> [PrepExec a] -> IO (a, [PostExec])
nestM finish = go []
  where go acc [] = (,) <$> finish <*> pure acc
        go acc (m:ms) = m (\f -> go (f acc) ms)

runCPS :: OpenCLState -> CLKernel -> NumWorkItems -> 
          (forall a. [PrepExec a]) -> 
          IO (CLEvent, ([Int -> IO (CLMem, Int)], [IO ()]))
runCPS s k n prep = second partitionPost <$> nestM runK prep
  where runK = clEnqueueNDRangeKernel (clQueue s) k (workItemsList n) [] []


{-
-- This instance runs the kernel synchronously as we must cleanup
-- allocated resources after the kernel runs. The silent change in
-- behavior (asynchronous becoming synchronous) suggests that such a
-- usage is a bug, so we don't expose this instance.
instance KernelArgsCPSAsync (IO ()) where
  setArg s k _ (Just n) blockers prep = do
    waitCLAsyncs blockers
    (exec, (o,cleanup)) <- runCPS s k n prep
    when (not $ null o) 
         (error "Automatic outputs not supported for async kernels!")
    -- Caller did not bind the completion event!
    clWaitForEvents [exec]
    clReleaseEvent exec
    sequence_ cleanup
-}

-- Return an event the user can wait on for a kernel to finish.
instance KernelArgsCPSAsync (IO CLAsync) where
  setArg s k _ (Just n) blockers prep = do
    let q = clQueue s
    waitCLAsyncs blockers
    (exec, (o,cleanup)) <- runCPS s k n prep
    when (not $ null o) 
         (error "Automatic outputs not supported for async kernels!")
    return $ CLAsync exec cleanup

-- Pass an arbitrary 'Storable' as a kernel argument.
instance (Storable a, KernelArgsCPSAsync r) => KernelArgsCPSAsync (a -> r) where
  setArg s k arg n blockers prep =
    \a -> let load = \cont -> clSetKernelArgSto k arg a >>
                              cont id
          in setArg s k (arg+1) n blockers (load : prep)

-- Handle 'Vector' input arguments.
instance (Storable a, KernelArgsCPSAsync r) => 
  KernelArgsCPSAsync (Vector a -> r) where
  setArg s k arg n blockers prep = 
    \v -> let load cont = withVectorBuffer (clContext s) v $
                          \b -> let clean = FreeInput . void $
                                            clReleaseMemObject b
                                in do clSetKernelArgSto k arg b
                                      cont (clean:)
          in setArg s k (arg+1) n blockers (load : prep)

-- Keep track of an argument that specifies the number of work items
-- to execute.
instance KernelArgsCPSAsync r => KernelArgsCPSAsync (NumWorkItems -> r) where
  setArg s k arg _ blockers prep = \n -> setArg s k arg (Just n) blockers prep

-- |Run an asynchronous kernel without copying any argument
-- vectors. This means that the OpenCL kernel may unsafely access the
-- Vector data /after/ the IO action returned by this function is
-- executed. Usage:
-- 
-- > ev <- unsafeRunKernelCPSAsync cluState kernel []
-- > doOtherThings
-- > waitCLAsync ev
unsafeRunKernelCPSAsync :: KernelArgsCPSAsync a => 
                           OpenCLState -> CLKernel -> [CLAsync] -> a
unsafeRunKernelCPSAsync s k blockers = setArg s k 0 Nothing blockers []
