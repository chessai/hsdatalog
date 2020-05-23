{-# LANGUAGE ScopedTypeVariables #-}

module Datalog.Stratification where

import Datalog.CycleEnumeration (enumerateWeightCycles)
import Datalog.Graph (Graph)
import Datalog.Syntax
import qualified Datalog.Graph as G

computeProgramPrecedenceGraph :: forall rel var. (Ord rel) => Program rel var -> Graph rel Bool
computeProgramPrecedenceGraph = foldr processDecl G.newGraph
  where
    processDecl :: Declaration rel var -> Graph rel Bool -> Graph rel Bool
    processDecl (Rule (Relation rel _) exprs) g = foldr (processExpr rel) g exprs

    processExpr :: rel -> Expr rel var -> Graph rel Bool -> Graph rel Bool
    processExpr relSource (Relation relTarget _, b) = G.addEdge relSource relTarget b
                                                      . G.addVertex relSource
                                                      . G.addVertex relTarget

-- TODO: be more efficient by running scc first and then enumerating cycles in
-- each scc
parityStratifyCheck :: (Ord rel) => Program rel var -> Bool
parityStratifyCheck = check . enumerateWeightCycles . computeProgramPrecedenceGraph
  where
    check :: [[Bool]] -> Bool
    check = all (even . length . filter not)

