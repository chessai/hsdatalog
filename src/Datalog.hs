{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

module Datalog
  ( main
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Data.Foldable (foldl', foldlM)
import Data.Map.Strict (Map)
import Data.Tuple (swap)
import Data.Void (Void)
import Datalog.Cudd (CuddT, DDNode)
import Datalog.CycleEnumeration
import Datalog.Elaboration
import Datalog.Graph
import Datalog.Pretty
import Datalog.Stratification
import Datalog.Syntax

import qualified Data.Map.Strict as Map
import qualified Datalog.Cudd as Cudd

main :: IO ()
main = do
  let progFile = "test.datalog"
  progCode <- readFile progFile
  case parseProgram progFile progCode of
    Left err -> putStrLn err
    Right prog -> do
{-      putStrLn "Input program:"
      printProgram prog
      putStrLn ""

      putStrLn "Renamed program:"
      printProgram (renameProgram prog)
      putStrLn ""

      putStrLn "Predicate Dependency Graph:"
      putStrLn $ prettyGraph $ computePredicateDependencyGraph prog
      putStrLn ""

      putStrLn "Enumerated weight cycles: "
      putStrLn $ pretty $ enumerateWeightCycles $ computePredicateDependencyGraph prog
      putStrLn ""

      putStrLn "Parity Stratification Check: "
      putStrLn $ pretty $ parityStratifyCheck prog
      putStrLn ""
-}
{-
      let (stmts, exprs) = runTacM $ do
            (stmts0, exprs0) <- joinSubgoals
              $ map ((, NotNegated) . fmap ElaborationName)
                [ Relation 20 [Right (Just 9), Right (Just 6), Right (Just 7)]
                , Relation 21 [Right (Just 8), Right (Just 7), Right (Just 9)]
                , Relation 22 [Right (Just 6)]
                ]
            (stmts1, exprs1) <- joinSubgoals exprs0
            pure (stmts0 ++ stmts1, exprs1)
-}

      let (exprs, stmts) = runTacM $ do
            (exprs0, stmts0) <- runWriterT $ selectConstants
              $ id @[Expr Int Name]
              $ map ((, NotNegated) . fmap ElaborationName)
                [ Relation 20 [Left (ConstantInt 3), Right (Just 6), Left (ConstantBitString [True,True,True,False])]
                , Relation 22 [Right (Just 6)]
                ]
            pure (exprs0, stmts0)

      mapM_ (putStrLn . pretty) stmts
      mapM_ (putStrLn . prettyExpr) exprs

printProgram :: (Pretty rel, Pretty var) => Program rel var -> IO ()
printProgram prog = do
  putStrLn "Declarations:"
  mapM_ (putStrLn . pretty) (decls prog)
  putStrLn ""
  putStrLn "Types:"
  mapM_ (putStrLn . uncurry prettyType) (Map.toList (types prog))

prettyGraph :: (Pretty node, Pretty weight) => Graph node weight -> String
prettyGraph = unlines . map go . edges
  where
    go (source, target, weight) = pretty source ++ " -> " ++ pretty target ++ "; [" ++ pretty weight ++ "]"
