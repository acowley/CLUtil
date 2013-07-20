{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
-- |Asynchronous kernel execution. Note that performance when running
-- on the CPU can take a hit here due to the need to make copies of
-- input vectors when kicking off kernel execution.
module Control.Parallel.CLUtil.KernelArgsAsync 
  (KernelArgsAsync, runKernelAsync) where
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.Vector.Storable (Vector)
import Foreign (Storable)
import Control.Parallel.CLUtil.KernelArgTypes
import Control.Parallel.CLUtil.State
import Control.Parallel.CLUtil.VectorBuffers
import Control.Parallel.OpenCL

-- Free an input buffer
type PostExec = IO ()

-- | A class for running kernels asynchronously. This class does not
-- have instances for returning 'Vector' values. The only possibile
-- return value is a 'IOAsync' that the user can wait on before
-- reading back results.
class KernelArgsAsync a where
  -- Identical to the setArg method of the KernelArgs class with the
  -- addition of a list of 'IOAsync' events to wait on before
  -- executing the kernel.
  setArg :: OpenCLState -> CLKernel -> CLuint -> Maybe NumWorkItems ->
            [IOAsync] -> [IO (Maybe PostExec)] -> a

runPrep :: [IO (Maybe PostExec)] -> IO [IO ()]
runPrep = fmap catMaybes . sequence

-- Return an event the user can wait on for a kernel to finish.
instance KernelArgsAsync (IO IOAsync) where
  setArg s k _ (Just n) blockers prep = do
    cleanup <- runPrep prep
    waitIOAsyncs blockers
    exec <- clEnqueueNDRangeKernel (clQueue s) k (workItemsList n) [] []
    return $ IOAsync exec cleanup
  setArg _ _ _ Nothing _ _ = error "The number of work items is missing!"

-- Pass an arbitrary 'Storable' as a kernel argument.
instance (Storable a, KernelArgsAsync r) => KernelArgsAsync (a -> r) where
  setArg s k arg n blockers prep = 
    \a -> let load = clSetKernelArgSto k arg a >> 
                     return Nothing
          in setArg s k (arg+1) n blockers (load : prep)

-- Handle 'Vector' input arguments.
instance (Storable a, KernelArgsAsync r) => 
  KernelArgsAsync (Vector a -> r) where
  setArg s k arg n blockers prep = 
    \v -> let load = do b <- vectorToBuffer (clContext s) v
                        clSetKernelArgSto k arg b
                        return . Just $ void (clReleaseMemObject b)
          in setArg s k (arg+1) n blockers (load : prep)

-- Keep track of an argument that specifies the number of work items
-- to execute.
instance KernelArgsAsync r => KernelArgsAsync (NumWorkItems -> r) where
  setArg s k arg _ blockers prep = \n -> setArg s k arg (Just n) blockers prep

-- |Simple interface for asynchronously running an OpenCL
-- kernel. Supports input 'Vector' and 'Storable' arguments. Outputs a
-- 'IOAsync' the user must wait on before inspecting output buffers.
--
-- > (getV, bufOut) <- initOutputVector cluState [] 4
-- > ev <- runKernelAsync cluState kernel [] vIn bufOut (Work1D 4)
-- > doOtherThings
-- > waitIOAsync ev
-- > vOut <- getV
runKernelAsync :: KernelArgsAsync a => OpenCLState -> CLKernel -> [IOAsync] -> a
runKernelAsync s k blockers = setArg s k 0 Nothing blockers []
