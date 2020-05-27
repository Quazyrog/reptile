module Compilatron where
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Intermediate
import Parser
import ExpressionsParser as EP
import Debug.Trace
import Control.DeepSeq
import qualified Stdlib
import qualified Control.Monad.State.Strict as MS

initialState :: ProgramState
initialState = PS {
  stateMemory = Map.empty,
  stateGlobalFrame = Map.empty,
  stateMemoryCounter = 1,
  stateFunctionScope = Stdlib.stdlib }

compileExpr :: Expression -> FunctionBody
compileExpr (ConstInt v) = \_ -> do return (Just (VInt v))
compileExpr (ConstStr v) = \_ -> do return (Just (VStr v))
compileExpr (ConstLog v) = \_ -> do return (Just (VBool v))
compileExpr (EP.Var vname) = \frame -> do
  vdata <- getVar frame vname
  return (vdata `deepseq` (Just vdata))
compileExpr (Call fname args) = 
  let 
    compiledArgs = \frame -> do
      cargs <- mapM (\arg -> compileExpr arg frame) args
      return (map (Value . fromJust) cargs)
  in \frame -> do
    vargs <- compiledArgs frame
    functions <- args `deepseq` MS.gets stateFunctionScope
    let funRFI = Map.lookup fname functions
    if isJust funRFI then do
      let funRFI' = fromJust funRFI
      let fun = instantiateFunction funRFI' frame
      bytecode <- fun
      retval <- bytecode vargs 
      return retval
    else do
      error ("[BUG] Called undefined function " ++ fname)


compileInstr :: AST -> FunctionBody
compileInstr (DoAll is) = 
  let 
    instrs = map compileInstr is
  in composeInstr instrs
compileInstr (Compute expr) = \f -> compileExpr expr f
compileInstr a = \_ -> undefined


composeInstr :: [FunctionBody] -> FunctionBody
composeInstr [] = \f -> do return Nothing
composeInstr (e:es) = 
  let rest = composeInstr es in \f -> do
    r1 <- e f
    r2 <- r1 `deepseq` (rest f)
    if isJust r2 then do return r2 else do return r1
