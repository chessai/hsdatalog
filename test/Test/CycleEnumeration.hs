module Test.CycleEnumeration
  ( tests
  ) where

import Datalog.CycleEnumeration
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as Set

tests :: TestTree
tests = testGroup "Cycle Enumeration"
  [ testCase "graphA" $ enumerateGraph graphA @?=
      [ [0, 2]
      , [0, 3, 1, 2]
      ]
{-  , testCase "subgraph of graphA induced by [1,2,3]"
      $ inducedSubgraph (Set.fromList [1,2,3]) graphA @?=
        [ [ (0, "c")
          , (1, "d")
          ]
        , [
          ]
        , [ (0, "h")
          , (2, "i")
          ]
        ]
-}
  ]

graphA :: Graph String
graphA =
  [ [ (2, "a")
    , (3, "b")
    ]
  , [ -- (1, "c")
      (2, "d")
    , (4, "e")
    ]
  , [ (0, "f")
    , (4, "g")
    ]
  , [ (1, "h")
    -- , (3, "i")
    , (4, "j")
    ]
  , [
    ]
  ]


