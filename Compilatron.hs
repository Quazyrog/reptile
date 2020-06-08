{-# LANGUAGE ScopedTypeVariables #-}
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
compileExpr (Call fname args) = do
  frame <- MS.gets stateTopFrame
  functions <- args `deepseq` MS.gets stateFunctionScope
  let funRFI = Map.lookup fname functions
  if isJust funRFI then do
    let funRFI' = fromJust funRFI
    let fun = instantiateFunction funRFI'
    vargs <- compileArgs (fArgsTypes funRFI') args -- todo seq
    bytecode <- fun
    retval <- bytecode vargs 
    return retval
  else do
    error ("[BUG] Called undefined function " ++ (trace (show (Map.keys functions)) fname))


compileArgs :: [RFIArg] -> [Expression] -> InterpIO [Arg]
compileArgs [] [] = do return []
compileArgs ((_, PassVal, _):rfis) (arg:args) = do
  let carg = compileExpr arg
  (Just varg) <- carg
  rest <- compileArgs rfis args
  return ((Value varg) : rest)
compileArgs ((_, PassRef, _):rfis) ((EP.Var vname):args) = do
  f <- MS.gets stateTopFrame
  let (Just id) = Map.lookup vname f
  rest <- compileArgs rfis args
  return ((Reference id) : rest)

compileInstr :: AST -> FunctionBody
compileInstr (DoAll is) = 
  let 
    instrs = map compileInstr is
  in composeInstr instrs
compileInstr (Compute expr) = compileExpr expr
compileInstr (Declare "int" names) = putVars (VInt 0) names
compileInstr (Declare "str" names) = putVars (VStr "") names
compileInstr (Declare "bool" names) = putVars (VBool False) names
compileInstr a = undefined


putVar :: VData -> (Map.Map String Integer) -> String -> InterpIO (Map.Map String Integer)
putVar init f name = do 
      if Map.member name f then do
        error ("[BUG] Redeclaration of a variable " ++ name)
      else do
        ref <- putRef init
        return (Map.insert name ref f)

putVars :: VData -> [String] -> FunctionBody
putVars init names = do
    f <- MS.gets stateTopFrame
    f' <- MS.foldM (putVar init) f names
    MS.modify (\s -> s { stateTopFrame = f' })
    return Nothing

composeInstr :: [FunctionBody] -> FunctionBody
composeInstr [] = do return Nothing
composeInstr (e:es) = 
  let rest = composeInstr es in do
    f <- MS.gets stateTopFrame
    r1 <- e
    r2 <- r1 `deepseq` rest
    if isJust r2 then do return r2 else do return r1
