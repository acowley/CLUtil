-- |Types used for passing arguments to OpenCL kernels, and dealing
-- with asynchronous execution.
module CLUtil.KernelArgTypes where
import Control.Parallel.OpenCL
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word

-- | A vector that will be written to by an OpenCL kernel. The
-- parameter is the number of elements in the vector. /NOTE/: The
-- resultant 'V.Vector' produced by 'CLUtil.KernelArgsCL.runKernel'
-- will wrap memory allocated by OpenCL. If you release the OpenCL
-- device (and its associated buffers), things will go sideways. In
-- the case when you want to cleanup an OpenCL context, but hold on to
-- a 'V.Vector' created by an 'OutputSize' kernel parameter, copy the
-- 'V.Vector' with 'vectorDup'.
newtype OutputSize = Out Int

-- | Copy the a 'V.Vector': This must be done if you want to release
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

-- |Wraps a 'CLEvent' and a list of cleanup actions to support
-- asynchronous kernel executions.
data IOAsync = IOAsync { asyncEvent :: CLEvent
                       , cleanupActions :: [IO ()] }

-- |Wait for an asynchronous operation to complete, then cleanup
-- associated resources.
waitIOAsync :: IOAsync -> IO ()
waitIOAsync (IOAsync ev cleanup) = clWaitForEvents [ev] >>
                                   clReleaseEvent ev >>
                                   sequence_ cleanup

-- |Wait for a list of asynchronous operations to complete, then
-- cleanup associated resources.
waitIOAsyncs :: [IOAsync] -> IO ()
waitIOAsyncs asyncs = do _ <- clWaitForEvents evs
                         mapM_ clReleaseEvent evs
                         sequence_ $ concatMap cleanupActions asyncs
  where evs = map asyncEvent asyncs
