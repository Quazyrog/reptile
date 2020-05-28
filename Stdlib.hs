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
  intOp "+" intAdd]

intAssign :: FunctionBody
intAssign = do
  frame <- MS.gets stateTopFrame
  v <- getVar frame "rhs"
  updateVar "lhs" (\_ -> v)
  return (Just v)

intOp :: String -> FunctionBody -> (String, RuntimeFunctionInfo)
intOp name body = 
  let args = [("lhs", PassVal, IntegerType), ("rhs", PassVal, IntegerType)] in
  (name, wrapStdlib name args IntegerType body)

intAdd :: FunctionBody
intAdd = do
  frame <- MS.gets stateTopFrame
  (VInt a) <- getVar frame "lhs"
  (VInt b) <- getVar frame "rhs"
  return (Just (VInt (a + b)))
