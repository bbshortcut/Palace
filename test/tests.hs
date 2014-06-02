module Main where

import Data.Monoid (mempty)
import Test.Framework (defaultMainWithOpts)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Property, quickCheckAll)
import Utils (itemPred)

prop_itemPred_safety :: Eq a => [a] -> a -> Property
prop_itemPred_safety ns x =
    not (null ns) ==>
    head ns == x ==>
      ns `itemPred` x == Nothing

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "itemPred safety"
                      (prop_itemPred_safety :: [Int] -> Int -> Property)
       ] mempty
