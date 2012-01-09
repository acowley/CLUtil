{-# LANGUAGE FlexibleInstances #-}
-- |Asynchronous kernel execution. Note that performance when running
-- on the CPU can take a hit here due to the need to make copies of
-- input vectors when kicking off kernel execution.
module System.GPU.CLUtil.KernelArgsAsync (KernelArgsAsync, runKernelAsync) where
import Control.Monad (void, when)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Data.Vector.Storable (Vector)
import Foreign (Storable)
import System.GPU.CLUtil.KernelArgTypes
import System.GPU.CLUtil.State
import System.GPU.CLUtil.VectorBuffers
import System.GPU.OpenCL

data PostExec = ReadOutput (Int -> IO (CLMem,Int))
              | FreeInput (IO ())

postToEither :: PostExec -> Either (Int -> IO (CLMem,Int)) (IO ())
postToEither (ReadOutput r) = Left r
postToEither (FreeInput m) = Right m

partitionPost :: [PostExec] -> ([Int -> IO (CLMem, Int)], [IO ()])
partitionPost = partitionEithers . map postToEither

-- A class for running kernels asynchronously. This class does not
-- have instances for returning 'Vector' values. The only possibile
-- return value is a 'CLAsync' that the user can wait on before
-- reading back results.
class KernelArgsAsync a where
  -- Identical to the setArg method of the KernelArgs class with the
  -- addition of a list of 'CLAsync' events to wait on before
  -- executing the kernel.
  setArg :: OpenCLState -> CLKernel -> CLuint -> Maybe NumWorkItems ->
            [CLAsync] -> [IO (Maybe PostExec)] -> a

runPrep :: [IO (Maybe PostExec)] -> IO ([Int -> IO (CLMem, Int)], [IO ()])
runPrep = fmap (partitionPost . catMaybes) . sequence

-- Return an event the user can wait on for a kernel to finish.
instance KernelArgsAsync (IO CLAsync) where
  setArg s k _ (Just n) blockers prep = do
    (o, cleanup) <- runPrep prep
    when (not $ null o) 
         (error "Automatic outputs not supported for async kernels!")
    waitCLAsyncs blockers
    exec <- clEnqueueNDRangeKernel (clQueue s) k (workItemsList n) [] []
    return $ CLAsync exec cleanup

-- Pass an arbitrary 'Storable' as a kernel argument.
instance (Storable a, KernelArgsAsync r) => KernelArgsAsync (a -> r) where
  setArg s k arg n blockers prep = 
    \a -> let load = clSetKernelArg k arg a >> 
                     return Nothing
          in setArg s k (arg+1) n blockers (load : prep)

-- Handle 'Vector' input arguments.
instance (Storable a, KernelArgsAsync r) => 
  KernelArgsAsync (Vector a -> r) where
  setArg s k arg n blockers prep = 
    \v -> let load = do b <- vectorToBuffer (clContext s) v
                        clSetKernelArg k arg b
                        return . Just . FreeInput $ 
                          void (clReleaseMemObject b)
          in setArg s k (arg+1) n blockers (load : prep)

-- Keep track of an argument that specifies the number of work items
-- to execute.
instance KernelArgsAsync r => KernelArgsAsync (NumWorkItems -> r) where
  setArg s k arg _ blockers prep = \n -> setArg s k arg (Just n) blockers prep

-- |Simple interface for asynchronously running an OpenCL
-- kernel. Supports input 'Vector' and 'Storable' arguments. Outputs a
-- 'CLAsync' the user must wait on before inspecting output buffers.
--
-- > (getV, bufOut) <- initOutputVector cluState [] 4
-- > ev <- runKernelAsync cluState kernel [] vIn bufOut (Work1D 4)
-- > doOtherThings
-- > waitCLAsync ev
-- > vOut <- getV
runKernelAsync :: KernelArgsAsync a => OpenCLState -> CLKernel -> [CLAsync] -> a
runKernelAsync s k blockers = setArg s k 0 Nothing blockers []
