module Datalog
  ( main
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.ST (ST, runST, stToIO)
import Data.Foldable (foldl', foldlM)
import Data.Map.Strict (Map)
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
      putStrLn "Input program:"
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
