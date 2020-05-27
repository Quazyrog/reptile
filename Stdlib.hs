module Stdlib where
import Intermediate
import qualified Control.Monad.State.Strict as MS
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Data.Set as Set


stdlib :: Map.Map String RuntimeFunctionInfo
stdlib = Map.fromList [intOp "+" intAdd]
  

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
