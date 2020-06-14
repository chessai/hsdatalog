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
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Void (Void)
import Datalog.Cudd (CuddT, SatBit)
import Datalog.CycleEnumeration
import Datalog.Elaboration
import Datalog.Graph
import Datalog.Interpreter
import Datalog.Pretty
import Datalog.RelAlgebra
import Datalog.Stratification
import Datalog.Syntax

import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
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

  let relProgram@(RelProgram statement typs) = programToRelProgram prog
  -- let tacs = statementToTACs statement
  -- let b = [True, False, True, False, True, False, True, False]
  -- let c = replicate 2 (ConstantBitString b)
  let bdd = interpretProgram relProgram
              -- $ RelProgram
              -- (Assignment (TAC (ElaborationRel 0) (Const c)))
              -- (Map.singleton (ElaborationRel 0) (TypeRelation [TypeInt, TypeInt]))


  putStrLn (pretty relProgram)
  forM_ (Map.toList bdd) $ \(rel, sats) -> do
    forM_ sats $ \sat -> do
      putStrLn $ pretty rel ++ prettySatBit sat

printProgram :: (Pretty rel, Pretty var) => Program rel var -> IO ()
printProgram prog = do
  putStrLn "Declarations:"
  mapM_ (putStrLn . pretty) (decls prog)
  putStrLn ""
  putStrLn "Types:"
  mapM_ (putStrLn . uncurry prettyType) (Map.toList (types prog))

prettySatBit :: [SatBit] -> String
prettySatBit sbs = id
  $ concatMap (('[' :) . (++ "]") . concatMap pretty)
  $ Extra.chunksOf 8 sbs

doParse :: FilePath -> IO (Program Rel Name)
doParse progFile = do
  progCode <- readFile progFile
  either fail pure (parseProgram progFile progCode)
