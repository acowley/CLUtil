module Main where
import Test.Tasty
import Test.Tasty.HUnit
import qualified CLMonad as CLMonad
import qualified TestEZ as TestEZ

testCL :: Assertion
testCL = return ()

testEZ :: Assertion
testEZ = do v1 <- TestEZ.test1
            assertEqual "test1" (V.fromList [6,8,10,12]) v1
            (v2a, v2b) <- TestEZ.test2
            assertEqual "test2a" (V.fromList [])
            assertEqual "test2b" (V.fromList [])
            [v3a, v3b, v3c, v3d] <- TestEZ.test3
            assertEqual "test3a" (V.fromList [])
            assertEqual "test3b" (V.fromList [])
            assertEqual "test3c" (V.fromList [])
            assertEqual "test3d" (V.fromList [])

main :: IO ()
main = defaultMain $ testGroup "CLUtil"
       [ testCase "EZ" testEZ
       , testCase "CL" testCL ]
