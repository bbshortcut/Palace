module Main where

import Data.List (nub)
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Property)
import Utils (itemPred)

prop_itemPred_bounded :: Eq a => [a] -> a -> Property
prop_itemPred_bounded ns x =
    nub ns == ns ==>
    not (null ns) ==>
    head ns == x ==>
      ns `itemPred` x == Nothing

main :: IO ()
main = defaultMain
       [ testProperty "itemPred bounded"
                      (prop_itemPred_bounded :: [Int] -> Int -> Property)
       ]
