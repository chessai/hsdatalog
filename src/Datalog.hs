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
      mapM_ (putStrLn . pretty) (decls prog)
      print (types prog)
      --mapM_ (putStrLn . pretty) (renameProgram prog)
      --print $ computeProgramPrecedenceGraph prog
      --print $ enumerateWeightCycles $ computeProgramPrecedenceGraph prog
      --print $ parityStratifyCheck prog
