module Stdlib where
import Intermediate
import qualified Control.Monad.State.Strict as MS
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.IO

stdlib :: Map.Map String RuntimeFunctionInfo
stdlib = Map.fromList $ 
  (map mangle intOps) ++ 
  boolOps ++
  [concatStr, setStr] ++
  [println, assert, inputi, inputs]
  
mangle :: RuntimeFunctionInfo -> (String, RuntimeFunctionInfo)
mangle rfi = (fName rfi ++ (argsRepr (fArgsTypes rfi)), rfi)

wrapStdlib :: String -> [RFIArg] -> VType -> FunctionBody -> RuntimeFunctionInfo
wrapStdlib name args rettype body = RFI {
  fName = name,
  fBoundVariables = [],
  fFreeVariables = [],
  fLocalFunctions = Map.empty,
  fArgsTypes = args,
  fReturnType = rettype,
  fBody = body}

------------------------------------ INTEGER -----------------------------------
intOps = [
  wrapStdlib "=" 
    [("lhs", PassRef, IntegerType), ("rhs", PassVal, IntegerType)] 
    IntegerType intAssign,
  intCmpOp "<" (<), intCmpOp "<=" (<=), intCmpOp ">" (>), intCmpOp ">=" (>=),
  intCmpOp "==" (==), intCmpOp "!=" (/=),
  intOp "+" (+), intOp "-" (-), 
  intOp "*" (*), intOp "/" div, intOp "%" mod,
  intOp "**" (^), intNeg, intToStr]

intAssign :: FunctionBody
intAssign = do
  frame <- MS.gets stateTopFrame
  v <- getVar frame "rhs"
  updateVar "lhs" (\_ -> v)
  return (Just v)

intOp :: String -> (Integer -> Integer -> Integer) -> RuntimeFunctionInfo
intOp name op = 
  let 
    args = [("lhs", PassVal, IntegerType), ("rhs", PassVal, IntegerType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VInt lhs) <- getVar frame "lhs"
      (VInt rhs) <- getVar frame "rhs"
      return (Just (VInt (op lhs rhs)))
  in
  wrapStdlib name args IntegerType body

intCmpOp :: String -> (Integer -> Integer -> Bool) -> RuntimeFunctionInfo
intCmpOp name op = 
  let 
    args = [("lhs", PassVal, IntegerType), ("rhs", PassVal, IntegerType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VInt lhs) <- getVar frame "lhs"
      (VInt rhs) <- getVar frame "rhs"
      return (Just (VBool (op lhs rhs)))
  in
  wrapStdlib name args BoolType body

intNeg = 
  let 
    baseName = "-"
    returnType = IntegerType
    args = [("arg", PassVal, IntegerType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VInt arg) <- getVar frame "arg"
      return (Just (VInt (-arg)))
  in wrapStdlib baseName args returnType body

intToStr = 
  let 
    baseName = "str"
    returnType = StringType
    args = [("arg", PassVal, IntegerType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VInt arg) <- getVar frame "arg"
      return (Just (VStr (show arg)))
  in wrapStdlib baseName args returnType body

------------------------------------ BOOLEAN -----------------------------------
boolOps = [boolOp "&&" (&&), boolOp "||" (||), boolNeg, boolToStr]

boolOp :: String -> (Bool -> Bool -> Bool) -> (String, RuntimeFunctionInfo)
boolOp name op = 
  let 
    args = [("lhs", PassVal, BoolType), ("rhs", PassVal, BoolType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VBool lhs) <- getVar frame "lhs"
      (VBool rhs) <- getVar frame "rhs"
      return (Just (VBool (op lhs rhs)))
  in (name ++ (argsRepr args), wrapStdlib name args BoolType body)

boolNeg = 
  let 
    baseName = "!"
    returnType = BoolType
    args = [("arg", PassVal, BoolType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VBool arg) <- getVar frame "arg"
      return (Just (VBool (not arg)))
  in (baseName ++ (argsRepr args), wrapStdlib baseName args returnType body)

boolToStr = 
  let 
    baseName = "str"
    returnType = StringType
    args = [("arg", PassVal, BoolType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VBool arg) <- getVar frame "arg"
      return (Just (VStr (show arg)))
  in (baseName ++ (argsRepr args), wrapStdlib baseName args returnType body)

------------------------------------- STRING -----------------------------------
concatStr = 
  let 
    baseName = "+"
    returnType = StringType
    args = [("lhs", PassVal, StringType), ("rhs", PassVal, StringType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VStr lhs) <- getVar frame "lhs"
      (VStr rhs) <- getVar frame "rhs"
      return (Just (VStr (lhs ++ rhs)))
  in (baseName ++ (argsRepr args), wrapStdlib baseName args returnType body)

setStr = 
  let 
    baseName = "="
    returnType = StringType
    args = [("lhs", PassRef, StringType), ("rhs", PassVal, StringType)] 
    body = do
      frame <- MS.gets stateTopFrame
      v <- getVar frame "rhs"
      updateVar "lhs" (\_ -> v)
      return (Just v)
  in (baseName ++ (argsRepr args), wrapStdlib baseName args returnType body)


-------------------------------------- MISC ------------------------------------
println = 
  let 
    baseName = "println"
    returnType = IntegerType
    args = [("arg", PassVal, StringType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VStr s) <- getVar frame "arg"
      MS.liftIO (putStrLn s)
      return (Just (VInt 0))
  in (baseName ++ (argsRepr args), wrapStdlib baseName args returnType body)

assert = 
  let 
    baseName = "assert"
    returnType = IntegerType
    args = [("arg", PassVal, BoolType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VBool arg) <- getVar frame "arg"
      if arg then do return (Just (VInt 0)) 
      else do error "Assertion failed"
  in (baseName ++ (argsRepr args), wrapStdlib baseName args returnType body)

inputi = 
  let 
    baseName = "inputi"
    returnType = IntegerType
    args = [("prompt", PassVal, StringType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VStr prompt) <- getVar frame "prompt"
      MS.liftIO (putStr prompt)
      MS.liftIO (System.IO.hFlush System.IO.stdout)
      line <- MS.liftIO getLine
      return (Just (VInt (read line :: Integer)))
  in (baseName ++ (argsRepr args), wrapStdlib baseName args returnType body)

inputs = 
  let 
    baseName = "inputs"
    returnType = StringType
    args = [("prompt", PassVal, StringType)] 
    body = do
      frame <- MS.gets stateTopFrame
      (VStr prompt) <- getVar frame "prompt"
      MS.liftIO (putStr prompt)
      MS.liftIO (System.IO.hFlush System.IO.stdout)
      line <- MS.liftIO getLine
      return (Just (VStr line))
  in (baseName ++ (argsRepr args), wrapStdlib baseName args returnType body)

