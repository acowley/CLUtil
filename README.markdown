A thin abstraction layer over the [OpenCL
package](http://hackage.haskell.org/package/OpenCL) that facilitates
the use of OpenCL with Vectors.

Example usage:

```haskell
import CLUtil
import qualified Data.Vector.Storable as V

test1 = do s <- ezInit CL_DEVICE_TYPE_CPU
           k <- kernelFromFile s "VecEZ.cl" "vecAdd"
           runCL s $
             do let v1 = V.fromList [1,2,3,4::Float]
                    v2 = V.fromList [5,6,7,8::Float]
                v3 <- runKernel k v1 v2 (Out 4) (Work1D 1)
                liftIO $ print (v3::Vector Float)
```
