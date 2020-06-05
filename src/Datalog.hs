{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Datalog
  ( main
  ) where

import Control.Monad
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
import Datalog.RelAlgebra
import Datalog.Stratification
import Datalog.Syntax

import qualified Data.Map.Strict as Map
import qualified Datalog.Cudd as Cudd

-- TODO (on the next episode):
--   - interpreter
--       - finish inferBitWidths
--   - loop detection
--   - generate SSA
--   - constant propagation
--   - stratification
--   - typechecking

main :: IO ()
main = do
  prog <- doParse "test.datalog"

  putStrLn "Input program:"
  printProgram prog
  putStrLn ""

  let statement = programToStatement prog
  let tacs = statementToTACs statement

  putStrLn (pretty statement)
  --putStrLn (pretty (inferBitWidths tacs))

printProgram :: (Pretty rel, Pretty var) => Program rel var -> IO ()
printProgram prog = do
  putStrLn "Declarations:"
  mapM_ (putStrLn . pretty) (decls prog)
  putStrLn ""
  putStrLn "Types:"
  mapM_ (putStrLn . uncurry prettyType) (Map.toList (types prog))

doParse :: FilePath -> IO (Program Rel Name)
doParse progFile = do
  progCode <- readFile progFile
  either fail pure (parseProgram progFile progCode)
