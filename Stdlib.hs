module Stdlib where
import Intermediate
import qualified Control.Monad.State.Strict as MS
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Data.Set as Set


stdlib :: Map.Map String RuntimeFunctionInfo
stdlib = Map.fromList intOps
  

wrapStdlib :: String -> [RFIArg] -> VType -> FunctionBody -> RuntimeFunctionInfo
wrapStdlib name args rettype body = RFI {
  fName = name ++ " " ++ (argRepr args),
  fBoundVariables = [],
  fFreeVariables = [],
  fLocalFunctions = Map.empty,
  fArgsTypes = args,
  fReturnType = rettype,
  fBody = body}

------------------------------------ INTEGER -----------------------------------
intOps = [
  ("=", wrapStdlib "=" 
    [("lhs", PassRef, IntegerType), ("rhs", PassVal, IntegerType)] 
    IntegerType intAssign),
  intOp "+" (+), intOp "-" (-), 
  intOp "*" (*), intOp "/" div, intOp "%" mod,
  intOp "**" (^)]

intAssign :: FunctionBody
intAssign = do
  frame <- MS.gets stateTopFrame
  v <- getVar frame "rhs"
  updateVar "lhs" (\_ -> v)
  return (Just v)

intOp :: String -> (Integer -> Integer -> Integer) -> (String, RuntimeFunctionInfo)
intOp name op = 
  let 
    args = [("lhs", PassVal, IntegerType), ("rhs", PassVal, IntegerType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VInt lhs) <- getVar frame "lhs"
      (VInt rhs) <- getVar frame "rhs"
      return (Just (VInt (op lhs rhs)))
  in
  (name, wrapStdlib name args IntegerType body)
