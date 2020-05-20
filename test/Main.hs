module Main
  ( main
  ) where

import Test.Tasty
import qualified Test.CycleEnumeration as CycleEnumeration

main :: IO ()
main = do
  defaultMain $ testGroup "main"
    [ CycleEnumeration.tests
    ]
