module Main where

import Data.List (nub)
import Test.QuickCheck ((==>), Property)
import Test.Tasty (defaultMain)
import Test.Tasty.QuickCheck (testProperty)
import Utils (itemPred)

prop_itemPred_bounded :: Eq a => [a] -> a -> Property
prop_itemPred_bounded ns x =
    nub ns == ns ==>
    not (null ns) ==>
    head ns == x ==>
      ns `itemPred` x == Nothing

main :: IO ()
main = defaultMain $
       testProperty "itemPred bounded"
                        (prop_itemPred_bounded :: [Int] -> Int -> Property)
