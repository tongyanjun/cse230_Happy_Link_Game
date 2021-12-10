import Test.Tasty
import Test.Tasty.HUnit

import LinkState (isLinkable0, isLinkable1, isLinkable2, isLinkable)

main :: IO ()
main = do
  defaultMain (testGroup "LinkState Tests" [isLinkable0Test, isLinkable1Test, isLinkable2Test, isLinkableTest])

isLinkable0Test :: TestTree
isLinkable0Test = testGroup "Testing isLinkable0"
  [testCase "Testing isLinkable0" $ assertBool "0" (isLinkable0 [['A','A','B','F'],['B','C','C','F'],['D',' ',' ','D']] 2 0 2 3)]

isLinkable1Test :: TestTree
isLinkable1Test = testGroup "Testing isLinkable1"
  [testCase "Testing isLinkable1" $ assertBool "0" (isLinkable1 [['A','A','B','F'],['B','C','C','D'],['D',' ',' ',' ']] 2 0 1 2)]


isLinkable2Test :: TestTree
isLinkable2Test = testGroup "Testing isLinkable2"
  [testCase "Testing isLinkable2" $ assertBool "0" $ not (isLinkable2 [['D',' ',' ',' '],['B','C','C','D'],['D',' ',' ',' ']] 0 0 2 0),
   testCase "Testing isLinkable2" $ assertBool "1" (isLinkable2 [['D',' ',' ',' '],['B','C','C',' '],['D',' ',' ',' ']] 0 0 2 0)]


isLinkableTest :: TestTree
isLinkableTest = testGroup "Testing isLinkable"
  [testCase "Testing isLinkable" $ assertBool "0" (isLinkable [[' ',' ',' ','D'],[' ','C','C','E'],[' ',' ',' ','D']] 0 3 2 3),
   testCase "Testing isLinkable" $ assertBool "1" (isLinkable [[' ',' ',' ','D'],['B','C','C','E'],[' ',' ',' ','D']] 0 3 2 3),
   testCase "Testing isLinkable" $ assertBool "2" $ not (isLinkable [['Q','D','Q',' '],['B','C','C','F'],['D',' ',' ',' ']] 0 1 2 0),
   testCase "Testing isLinkable" $ assertBool "3" (isLinkable [['D',' ',' ',' '],['B','C','C',' '],['D',' ',' ',' ']] 0 0 2 0)]