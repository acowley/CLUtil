-- |A simple record that packages up commonly used references to bits
-- of the OpenCL interface.
module System.GPU.CLUtil.State where
import System.GPU.OpenCL

-- |A record capturing the core pieces of state needed to evaluate
-- OpenCL kernels.
data OpenCLState = OpenCLState { clDevice  :: CLDeviceID
                               , clContext :: CLContext
                               , clQueue   :: CLCommandQueue }
