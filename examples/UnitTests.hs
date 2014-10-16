module Main where
import qualified Data.Vector.Storable as V
import Test.Tasty
import Test.Tasty.HUnit
import qualified CLMonad as CLMonad
import qualified TestEZ as TestEZ

testCL :: Assertion
testCL = do v1 <- CLMonad.testNoPre
            let v1' = V.zipWith (+)
                        (V.fromList [1..6])
                        (V.fromList [7..12])
            assertEqual "test1" v1' v1
            v2 <- CLMonad.testPre
            assertEqual "test2" v1' v2

testEZ :: Assertion
testEZ = do v1 <- TestEZ.test1
            assertEqual "test1" (V.fromList [36,64,100,144]) v1
            (v2a, v2b) <- TestEZ.test2
            let v2a' = V.map (\x->x*x) $
                       V.zipWith (+)
                         (V.fromList [0..11])
                         (V.fromList [12..23])
                aux = V.sum . V.map truncate
                v2b' = V.fromList [ aux (V.take 4 v2a')
                                  , aux (V.take 4 (V.drop 4 v2a'))
                                  , aux (V.take 4 (V.drop 8 v2a')) ]
            assertEqual "test2a" v2a' v2a
            assertEqual "test2b" v2b' v2b
            rows <- TestEZ.test3
            let v3' = V.zipWith (+)
                        (V.fromList [0..15])
                        (V.fromList [16..31])
                rows' = map (\i -> V.slice i 4 v3') [0,4 .. 12]
            assertEqual "test3" rows' rows

main :: IO ()
main = defaultMain $ testGroup "CLUtil"
       [ testCase "EZ" (return ()) -- testEZ
       , testCase "CL" testCL ]
