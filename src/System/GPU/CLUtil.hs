{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, OverlappingInstances, 
             RankNTypes #-}
-- |High-level interfaces for working with 'Vector's and the OpenCL
-- library.
module System.GPU.CLUtil 
  (vectorToBuffer, bufferToVector, bufferToVectorAsync,
   ezInit, ezRelease, loadProgram, kernelFromFile,
   OutputSize(..), NumWorkItems(..), OpenCLState(..),
   localFloat, localDouble, localInt, runKernel,
   module System.GPU.OpenCL, Vector, CInt, initOutputBuffer,
   CLAsync(..), waitCLAsync, waitCLAsyncs, runKernelAsync) where
import System.GPU.OpenCL
import Control.Applicative
import Control.Monad (void, when)
import Data.List (partition)
import Data.Maybe (catMaybes)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (Storable(sizeOf))

-- |Fill an OpenCL memory buffer with a 'Vector'.
vectorToBuffer :: forall a. Storable a => CLContext -> Vector a -> IO CLMem
vectorToBuffer context v = 
  V.unsafeWith v $ \ptr ->
    -- NOTE: If we use the host ptr, then operations on the CPU are
    -- much faster. But if the GC moves things around, the world will
    -- end.
    clCreateBuffer context 
                   --[CL_MEM_READ_ONLY, CL_MEM_USE_HOST_PTR]
                   [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] 
                   (sz, castPtr ptr)
  where sz = V.length v * sizeOf (undefined::a)

-- |Read an OpenCL memory buffer into a new 'Vector'.
bufferToVector :: forall a. Storable a => 
                  CLCommandQueue -> CLMem -> Int -> [CLEvent] -> IO (Vector a)
bufferToVector q mem count waitForIt = 
  do v <- VM.new count
     _ <- VM.unsafeWith v $ \ptr ->
            clEnqueueReadBuffer q mem True 0 sz (castPtr ptr) waitForIt >>=
            clReleaseEvent
     V.unsafeFreeze v
  where sz = count * sizeOf (undefined::a)

-- |Asynchronously (and unsafely) read an OpenCL memory buffer into a
-- new 'Vector'. The returned action blocks until the read is
-- finished.
bufferToVectorAsync :: forall a. Storable a => 
                  CLCommandQueue -> CLMem -> Int -> [CLEvent] -> 
                  IO (IO (Vector a))
bufferToVectorAsync q mem count waitForIt = 
  do v <- VM.new count
     readEvent <- VM.unsafeWith v $ \ptr ->
                    clEnqueueReadBuffer q mem False 0 sz (castPtr ptr) waitForIt
     return $ clWaitForEvents [readEvent] >> 
              clReleaseEvent readEvent >> 
              V.unsafeFreeze v
  where sz = count * sizeOf (undefined::a)

-- |Allocate a buffer whose contents are undefined.
initOutputBuffer :: Integral a => OpenCLState -> [CLMemFlag] -> a -> IO CLMem
initOutputBuffer s flags n = clCreateBuffer (clContext s) flags (n, nullPtr)

-- |A record capturing the core pieces of state needed to evaluate
-- OpenCL kernels.
data OpenCLState = OpenCLState { clDevice  :: CLDeviceID
                               , clContext :: CLContext
                               , clQueue   :: CLCommandQueue }

-- |Initialize the first device of the given type.
ezInit :: CLDeviceType -> IO OpenCLState
ezInit t = do (dev:_) <- clGetDeviceIDs nullPtr t
              context <- clCreateContext [dev] putStrLn
              q <- clCreateCommandQueue context dev []
              return $ OpenCLState dev context q

-- |Release a context and command queue.
ezRelease :: OpenCLState -> IO ()
ezRelease (OpenCLState _ c q) = 
  void $ clReleaseContext c >> clReleaseCommandQueue q

-- |Load program source using a previously-initialized 'OpenCLState'
-- record. The returned function may be used to create executable
-- kernels defined in the supplied program source.
loadProgram :: OpenCLState -> String -> IO (String -> IO CLKernel)
loadProgram state src = do p <- clCreateProgramWithSource (clContext state) src
                           clBuildProgram p [clDevice state] 
                                          "-cl-strict-aliasing"
                           return $ clCreateKernel p
-- Another option for the clBuildProgram call is "-cl-fast-relaxed-math"

-- |Load program source from the given file and build the named
-- kernel.
kernelFromFile :: OpenCLState -> FilePath -> String -> IO CLKernel
kernelFromFile state file kname = 
  readFile file >>= loadProgram state >>= ($ kname)

-- |A vector that will be written in to. The parameter is the number
-- of elements in the vector.
newtype OutputSize = Out Int

-- |The number of global work items to enqueue. May be 1, 2, or 3D.
data NumWorkItems = Work1D Int | Work2D Int Int | Work3D Int Int Int

workItemsList :: NumWorkItems -> [Int]
workItemsList (Work1D n) = [n]
workItemsList (Work2D n m) = [n,m]
workItemsList (Work3D n m o) = [n,m,o]

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

-- Variable arguments class patterned on Printf.

class KernelArgs a where
  -- Setting an argument requires a state (device, queue, context), a
  -- kernel, the position of the argument, the number of work items
  -- specified so far, and a list of actions that prepare argument
  -- buffers and return the cleanup action to take when the kernel is
  -- finished.
  setArg :: OpenCLState -> CLKernel -> CLuint -> Maybe [Int] -> 
            [IO (Maybe PostExec)] -> a

-- We use a separate class for running kernels asynchronously. This
-- class does not have instances for returning 'Vector' values. The
-- only possibile return value is a 'CLAsync' that the user can wait
-- on before reading back results.
class KernelArgsAsync a where
  -- Identical to the setArg method of the KernelArgs class with the
  -- addition of a list of 'CLAsync' events to wait on before
  -- executing the kernel.
  setArgAsync :: OpenCLState -> CLKernel -> CLuint -> Maybe [Int] ->
                 [CLAsync] -> [IO (Maybe PostExec)] -> a

data PostExec = ReadOutput (Int -> IO (CLMem,Int))
              | FreeInput (IO ())

partitionPost :: [PostExec] -> ([Int -> IO (CLMem, Int)], [IO ()])
partitionPost [] = ([],[])
partitionPost (ReadOutput f : ps) = let (fs, ms) = partitionPost ps
                                    in (f:fs, ms)
partitionPost (FreeInput m : ps) = let (fs,ms) = partitionPost ps
                                   in (fs, m:ms)

mkRead :: Storable a => CLCommandQueue -> (CLMem, Int) -> IO (Vector a)
mkRead q (mem,num) = do v <- bufferToVector q mem num []
                        clReleaseMemObject mem
                        return v

-- |We need a 'CLEvent' and a list of cleanup actions to support
-- asynchronous kernel executions.
data CLAsync = CLAsync { asyncEvent :: CLEvent
                       , cleanupActions :: [IO ()] }

-- |Wait for an asynchronous operation to complete, then cleanup
-- associated resources.
waitCLAsync :: CLAsync -> IO ()
waitCLAsync (CLAsync ev cleanup) = clWaitForEvents [ev] >>
                                   clReleaseEvent ev >>
                                   sequence_ cleanup

-- |Wait for a list of asynchronous operations to complete, then
-- cleanup associated resources.
waitCLAsyncs :: [CLAsync] -> IO ()
waitCLAsyncs asyncs = do clWaitForEvents evs
                         mapM_ clReleaseEvent evs
                         sequence_ $ concatMap cleanupActions asyncs
  where evs = map asyncEvent asyncs

-- Preparing arguments involves providing a cleanup action (if
-- necessary), along with a continuation for the rest of preparation.
type PrepExec a = IO ( Maybe PostExec
                     , IO (a, [Maybe PostExec]) -> IO (a, [Maybe PostExec]))

-- Nest a stack of buffer preparation actions. This lets us safely
-- directly access the pointers underlying 'Vector' arguments.
nestM :: IO a -> [PrepExec a] -> IO (a, [Maybe PostExec])
nestM finish = go []
  where go acc [] = (,) <$> finish <*> pure acc
        go acc (m:ms) = do (pe,k) <- m
                           k $ go (pe:acc) ms

class KAAsync a where
  -- Identical to the setArg method of the KernelArgs class with the
  -- addition of a list of 'CLAsync' events to wait on before
  -- executing the kernel.
  setAA :: OpenCLState -> CLKernel -> CLuint -> Maybe [Int] ->
           [CLAsync] -> (forall b. [PrepExec b]) -> a

-- Synchronous execution of a kernel with no automatic outputs. This
-- is useful for kernels that modify user-managed buffers.
instance KernelArgs (IO ()) where
  setArg s k _ (Just n) prep = do
    let q = clQueue s
    (o, cleanup) <- partitionPost . catMaybes <$> sequence prep
    when (not (null o)) (error "Outputs aren't bound!")
    exec <- clEnqueueNDRangeKernel q k n [] []
    clWaitForEvents [exec]
    clReleaseEvent exec
    sequence_ cleanup

instance KAAsync (IO ()) where
  setAA s k _ (Just n) blockers prep = do
    let q = clQueue s
    waitCLAsyncs blockers
    (exec, posts) <- nestM (clEnqueueNDRangeKernel q k n [] []) prep
    let (o,cleanup) = partitionPost . catMaybes $ posts
    when (not $ null o) 
         (error "Automatic outputs not supported for async kernels!")
    clWaitForEvents [exec]
    sequence_ cleanup

-- Return an event the user can wait on for a kernel to finish.
instance KernelArgsAsync (IO CLAsync) where
  setArgAsync s k _ (Just n) blockers prep = do
    let q = clQueue s
    (o, cleanup) <- partitionPost . catMaybes <$> sequence prep
    when (not $ null o) 
         (error "Automatic outputs not supported for async kernels!")
    waitCLAsyncs blockers
    exec <- clEnqueueNDRangeKernel q k n [] []
    return $ CLAsync exec cleanup

-- Return an event the user can wait on for a kernel to finish.
instance KAAsync (IO CLAsync) where
  setAA s k _ (Just n) blockers prep = do
    let q = clQueue s
    waitCLAsyncs blockers
    (exec, posts) <- nestM (clEnqueueNDRangeKernel q k n [] []) prep
    let (o,cleanup) = partitionPost . catMaybes $ posts
    when (not $ null o) 
         (error "Automatic outputs not supported for async kernels!")
    return $ CLAsync exec cleanup

-- Execute a kernel where the calling context is expecting a single
-- 'Vector' return value.
instance forall a. Storable a => KernelArgs (IO (Vector a)) where
  setArg s k _ (Just n) prep = do
    let q = clQueue s
    (o, cleanup) <- partitionPost . catMaybes <$> sequence prep
    r1 <- case o of
            [] -> error "One output bound, none specified"
            [f] -> do x <- f (sizeOf (undefined::a))
                      return $ mkRead q x
            _ -> error "More outputs specified than bound"
    void $ clFinish q
    exec <- clEnqueueNDRangeKernel q k n [] []
    clWaitForEvents [exec]
    clReleaseEvent exec
    sequence_ cleanup
    r1

-- Execute a kernel where the calling context is expecting two
-- 'Vector' return values.
instance forall a b. (Storable a, Storable b) => 
  KernelArgs (IO (Vector a, Vector b)) where
  setArg s k _ (Just n) prep = do
    let q = clQueue s
    (o, cleanup) <- partitionPost . reverse . catMaybes <$> sequence prep
    (r1,r2) <- case o of
                 [] -> error "Two outputs bound, none specified"
                 [_] -> error "Two outputs bound, one specified"
                 [f,g] -> do x <- f (sizeOf (undefined::a))
                             y <- g (sizeOf (undefined::b))
                             return (mkRead q x, mkRead q y)
                 _ -> error "More outputs specified than bound"
    _ <- clFinish q
    exec <- clEnqueueNDRangeKernel q k n [] []
    clWaitForEvents [exec]
    clReleaseEvent exec
    sequence_ cleanup
    (,) <$> r1 <*> r2

-- Execute a kernel where the calling context is expecting three
-- 'Vector' return values.
instance forall a b c. (Storable a, Storable b, Storable c) => 
  KernelArgs (IO (Vector a, Vector b, Vector c)) where
  setArg s k _ (Just n) prep = do
    let q = clQueue s
    (o, cleanup) <- partitionPost . reverse . catMaybes <$> sequence prep
    (r1,r2,r3) <- case o of
                    [f,g,h] -> do x <- f (sizeOf (undefined::a))
                                  y <- g (sizeOf (undefined::b))
                                  z <- h (sizeOf (undefined::c))
                                  return (mkRead q x, mkRead q y, mkRead q z)
                    _ -> error "Different number of outputs specified than bound"
    _ <- clFinish q
    exec <- clEnqueueNDRangeKernel q k n [] []
    clWaitForEvents [exec]
    clReleaseEvent exec
    sequence_ cleanup
    (,,) <$> r1 <*> r2 <*> r3

-- Pass an arbitrary 'Storable' as a kernel argument.
instance (Storable a, KernelArgs r) => KernelArgs (a -> r) where
  setArg s k arg n prep = \a -> let load = clSetKernelArg k arg a >> 
                                           return Nothing
                                in setArg s k (arg+1) n (load : prep)

instance (Storable a, KernelArgsAsync r) => KernelArgsAsync (a -> r) where
  setArgAsync s k arg n blockers prep = 
    \a -> let load = clSetKernelArg k arg a >> 
                     return Nothing
          in setArgAsync s k (arg+1) n blockers (load : prep)

instance (Storable a, KAAsync r) => KAAsync (a -> r) where
  setAA s k arg n blockers prep =
    \a -> let load = clSetKernelArg k arg a >>
                     return (Nothing, id)
          in setAA s k (arg+1) n blockers (load : prep)

-- Handle 'Vector' input arguments.
instance (Storable a, KernelArgs r) => KernelArgs (Vector a -> r) where
  setArg s k arg n prep = \v -> 
                          let load = do b <- vectorToBuffer (clContext s) v
                                        clSetKernelArg k arg b
                                        return . Just . FreeInput $ 
                                           void (clReleaseMemObject b)
                          in setArg s k (arg+1) n (load : prep)

instance (Storable a, KernelArgsAsync r) => 
  KernelArgsAsync (Vector a -> r) where
  setArgAsync s k arg n blockers prep = 
    \v -> let load = do b <- vectorToBuffer (clContext s) v
                        clSetKernelArg k arg b
                        return . Just . FreeInput $ 
                          void (clReleaseMemObject b)
          in setArgAsync s k (arg+1) n blockers (load : prep)

instance (Storable a, KAAsync r) => KAAsync (Vector a -> r) where
  setAA s k arg n blockers prep = 
    \v -> let load = return (Just (FreeInput $ void (clReleaseMemObject b)),
                             -- Problem: we need to refer to b in the
                             -- cleanup action, but we don't get a b
                             -- unless we give it a pointer to our
                             -- data. This means the continuation has
                             -- to produce the cleanup action.

                     -- do b <- vectorToBuffer (clContext s) v
                     --    clSetKernelArg k arg b
                     --    return . Just . FreeInput $ 
                     --      void (clReleaseMemObject b)
          in setArgAsync s k (arg+1) n blockers (load : prep)


-- Keep track of an argument that specifies the number of work items
-- to execute.
instance KernelArgs r => KernelArgs (NumWorkItems -> r) where
  setArg s k arg _ prep = \n -> setArg s k arg (Just (workItemsList n)) prep

instance KernelArgsAsync r => KernelArgsAsync (NumWorkItems -> r) where
  setArgAsync s k arg _ blockers prep = 
    \n -> setArgAsync s k arg (Just (workItemsList n)) blockers prep

-- Handle 'Vector' outputs by automatically managing the underlying
-- OpenCL buffers.
instance KernelArgs r => KernelArgs (OutputSize -> r) where
  setArg s k arg n prep = 
    \(Out m) -> 
      let alloc sz = do b <- clCreateBuffer (clContext s) 
                                            [CL_MEM_WRITE_ONLY]
                                            (m*sz, nullPtr)
                        clSetKernelArg k arg b
                        return (b,m)
      in setArg s k (arg+1) n (return (Just $ ReadOutput alloc) : prep)

-- |Simple interface for calling an OpenCL kernel. Supports input
-- 'Vector' and 'Storable' arguments, and 'Vector' outputs.
runKernel :: KernelArgs a => OpenCLState -> CLKernel -> a
runKernel s k = setArg s k 0 Nothing []

-- |Simple interface for calling an OpenCL kernel. Supports input
-- 'Vector' and 'Storable' arguments. Outputs a 'CLAsync' the user
-- must wait on before inspecting output buffers.
runKernelAsync :: KernelArgsAsync a => OpenCLState -> CLKernel -> [CLAsync] -> a
runKernelAsync s k blockers = setArgAsync s k 0 Nothing blockers []
