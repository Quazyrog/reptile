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
  stateTopFrame = Map.empty,
  stateMemoryCounter = 1,
  stateFunctionScope = Stdlib.stdlib }

compileExpr :: Expression -> FunctionBody
compileExpr (ConstInt v) = do return (Just (VInt v))
compileExpr (ConstStr v) = do return (Just (VStr v))
compileExpr (ConstLog v) = do return (Just (VBool v))
compileExpr (EP.Var vname) = do
  frame <- MS.gets stateTopFrame
  vdata <- getVar frame vname
  return (vdata `deepseq` (Just vdata))
compileExpr (Call fname args) = 
  let 
    compiledArgs = do
      cargs <- mapM (\arg -> compileExpr arg) args
      return (map (Value . fromJust) cargs)
  in do
    frame <- MS.gets stateTopFrame
    vargs <- compiledArgs
    functions <- args `deepseq` MS.gets stateFunctionScope
    let funRFI = Map.lookup fname functions
    if isJust funRFI then do
      let funRFI' = fromJust funRFI
      let fun = instantiateFunction funRFI'
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
compileInstr (Compute expr) = compileExpr expr
compileInstr (Declare "int" names) = error "todo"
compileInstr a = undefined


composeInstr :: [FunctionBody] -> FunctionBody
composeInstr [] = do return Nothing
composeInstr (e:es) = 
  let rest = composeInstr es in do
    f <- MS.gets stateTopFrame
    r1 <- e
    r2 <- r1 `deepseq` rest
    if isJust r2 then do return r2 else do return r1
