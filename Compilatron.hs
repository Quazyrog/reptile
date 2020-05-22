module Compilatron where
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Intermediate
import Parser
import ExpressionsParser
import Debug.Trace
import Control.DeepSeq

initialState :: ProgramState
initialState = PS {
  stateMemory = Map.empty,
  stateGlobalFrame = Map.empty}

compileExpr :: Expression -> FunctionBody
compileExpr (ConstInt v) = \_ -> do return (Just (VInt v))
compileExpr (ConstStr v) = \_ -> do return (Just (VStr v))
compileExpr (ConstLog v) = \_ -> do return (Just (VBool v))


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
