module Main where

import Data.Maybe (isNothing)
import Data.Tree (Tree(..))
import Data.List (nub)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)
import Test.Tasty.QuickCheck ((==>), Property, testProperty)
import Utils (addTrees, intersectTrees, itemPred, removeTrees)

prop_itemPred_bounded :: Eq a => [a] -> a -> Property
prop_itemPred_bounded ns x =
    nub ns == ns ==>
    not (null ns) ==>
    head ns == x ==>
         isNothing (ns `itemPred` x)

properties :: TestTree
properties = testGroup "Properties"
             [ testProperty "itemPred bounded"
               (prop_itemPred_bounded :: [Int] -> Int -> Property) ]

addTreesTests :: TestTree
addTreesTests =
    testGroup "Unit tests for addTrees"
                  [ testCase "... when trees are equal"
                    ((Just $ Node 0 []) @=? addTrees (Node 0 []) (Node 0 []))
                  , testCase "... when trees do not have same rot"
                    (Nothing @=? addTrees (Node 0 []) (Node 1 []))
                  , testCase "... when the second tree is fully in the first"
                    ((Just $ Node 0 [Node 1 [], Node 2 []]) @=?
                     addTrees (Node 0 [Node 1 [], Node 2 []])
                                  (Node 0 [Node 2 []]))
                  , testCase "... when the second is fully in the first (bis)"
                    ((Just $ Node 0 [Node 1 [], Node 2 [Node 3 []]]) @=?
                     addTrees (Node 0 [Node 1 [], Node 2 [Node 3 []]])
                                  (Node 0 [Node 2 [Node 3 []]]))
                  , testCase "... when the second is not fully in the first"
                    ((Just $ Node 0 [Node 1 []]) @=?
                     addTrees (Node 0 [])
                                  (Node 0 [Node 1 []])) ]

removeTreesTests :: TestTree
removeTreesTests = testGroup "Unit tests for removeTrees"
                   [ testCase "... when trees are equal"
                     (Nothing @=? removeTrees (Node 0 []) (Node 0 []))
                   , testCase "... when trees do not have same rot"
                     ((Just $ Node 0 []) @=? removeTrees (Node 0 []) (Node 1 []))
                   , testCase "... when the second tree is fully in the first"
                     ((Just $ Node 0 [Node 1 []]) @=?
                      removeTrees (Node 0 [Node 1 [], Node 2 []])
                                      (Node 0 [Node 2 []]))
                   , testCase "... when the second is fully in the first (bis)"
                     ((Just $ Node 0 [Node 1 []]) @=?
                      removeTrees (Node 0 [Node 1 [], Node 2 [Node 3 []]])
                                      (Node 0 [Node 2 [Node 3 []]]))
                   , testCase "... when the second is not fully in the first"
                     (Nothing @=?
                      removeTrees (Node 0 [])
                                      (Node 0 [Node 1 []])) ]

intersectTreesTests :: TestTree
intersectTreesTests =
    testGroup "Unit tests for intersectTrees"
                  [ testCase "... when trees are equal"
                    (Just (Node 0 []) @=?
                     intersectTrees (Node 0 []) (Node 0 []))
                  , testCase "... when trees do not have same root"
                    (Nothing @=? intersectTrees (Node 0 []) (Node 1 []))
                  , testCase "... when the second tree is fully in the first"
                    ((Just $ Node 0 [Node 2 []]) @=?
                     intersectTrees (Node 0 [Node 1 [], Node 2 []])
                     (Node 0 [Node 2 []]))
                  , testCase "... when the second is fully in the first (bis)"
                    ((Just $ Node 0 [Node 2 [Node 3 []]]) @=?
                     intersectTrees (Node 0 [Node 1 [], Node 2 [Node 3 []]])
                     (Node 0 [Node 2 [Node 3 []]]))
                  , testCase "... when the second is not fully in the first"
                    ((Just $ Node 0 []) @=?
                     intersectTrees (Node 0 [])
                     (Node 0 [Node 1 []])) ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
            [ addTreesTests, removeTreesTests, intersectTreesTests ]

tests :: TestTree
tests = testGroup "tests" [ properties, unitTests ]

main :: IO ()
main = defaultMain tests
