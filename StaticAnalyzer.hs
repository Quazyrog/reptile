module StaticAnalyzer where
import Control.Monad.State.Strict as MS
import qualified Data.Map as Map
import qualified Data.List as List
import Parser
import ExpressionsParser as EParser
import Intermediate
import Stdlib (stdlib)
import Data.Maybe (isJust, fromJust)

data AnalyzerState = AnalyzerState {
  asVars :: Vars,
  asFuns :: Overloads,
  asReturn :: VType
}
initialAnalyzerState = AnalyzerState {
  asVars = Map.empty,
  asFuns = stdlibInfo,
  asReturn = IntegerType
}

data FunctionType = FTypeInfo {
  ftReturn :: VType,
  ftArgs :: [(ArgPassType, VType)]
}
type Overloads = Map.Map String [FunctionType]
type Vars = Map.Map String VType

stdlibInfo :: Overloads
-- |Type info for buildin functions and operators
stdlibInfo = 
  let 
    singleOverload rfi = FTypeInfo { 
      ftReturn = (fReturnType rfi),
      ftArgs = map (\(_, p, t) -> (p, t)) (fArgsTypes rfi)}
    stdfunToInfo (_, rfi) = ((fName rfi), [singleOverload rfi])
  in Map.fromListWith (++) (map stdfunToInfo (Map.toList stdlib))

transform' :: AST -> State AnalyzerState AST
-- |Statefuly transform given AST preforming name mangling and type checking
transform' (DoAll instr) = do
  instr' <- mapM transform' instr
  return (DoAll instr')
transform' instr@(Declare typename idents) = 
  let
    typeFromName "int" = IntegerType
    typeFromName "bool" = BoolType
    typeFromName "str" = StringType
    typeFromName s = error ("Unknown type '" ++ s ++ "'")
  in do
  mapM (injectVar (typeFromName typename)) idents
  return instr
transform' (Compute expr) = do 
  s <- MS.get
  let expr' = fst (checkType expr s)
  return (Compute expr')
transform' other = do return other

injectVar :: VType -> String -> State AnalyzerState ()
-- |Append to state information about variable type
injectVar t name = do
  s <- MS.get
  if isJust (Map.lookup name (asVars s)) then do 
    error ("Redeclaration of variable '" ++ name ++ "'")
  else do
    MS.put s { asVars = Map.insert name t (asVars s) }

checkType :: Expression -> AnalyzerState -> (Expression, VType)
-- |Preform type check on expression together with name mangling
checkType c@(ConstInt _) _ = (c, IntegerType)
checkType c@(ConstStr _) _ = (c, StringType)
checkType c@(ConstLog _) _ = (c, BoolType)
checkType v@(EParser.Var vn) state = 
  let typeMaybe = Map.lookup vn (asVars state) in
  if isJust typeMaybe then 
    (v, fromJust typeMaybe)
  else
    error ("Undeclared variable " ++ vn)
checkType (Call basename args) state = 
  let overloads = Map.lookup basename (asFuns state) in
  if isJust overloads then
    let 
      typedArgs = map (typeArg state) args
      transformedArgs = map (\(expr, _, _) -> expr) typedArgs
      argsTypeInfo = map (\(_, ref, t) -> (ref, t)) typedArgs
      funInfo = lookupOverload (fromJust overloads) argsTypeInfo
    in
    if isJust funInfo then
      let f = fromJust funInfo in
      (Call (mangleName basename f) transformedArgs, ftReturn f)
    else 
      error ("No suitable overload of '" ++ basename 
        ++ "' for " ++ (show argsTypeInfo))
  else
    error ("Unknown function '" ++ basename ++"'")

typeArg :: AnalyzerState -> Expression -> (Expression, Bool, VType)
-- |Mangle names in expression, check it's type and if it can be passed as reference.
typeArg state expr = 
  let 
    isLvalue (EParser.Var _) = True
    isLvalue _ = False
    (expr', exprType) = checkType expr state
  in (expr', isLvalue expr', exprType)

lookupOverload :: [FunctionType] -> [(Bool, VType)] -> Maybe FunctionType
-- | Find valid overloaded version for given arguments
lookupOverload ovs args = 
  let 
    matchArgs ft = 
      if length (ftArgs ft) /= length args then 
        False 
      else
        matchArgs' args (ftArgs ft)
    matchArgs' [] [] = True
    matchArgs' ((True, t0):margs) ((_, t1):cargs) = 
      (t0 == t1) && (matchArgs' margs cargs)
    matchArgs' ((False, t0):margs) ((PassVal, t1):cargs) = 
      (t0 == t1) && (matchArgs' margs cargs)
    matchArgs' ((False, t0):margs) ((PassRef, t1):cargs) = False
  in List.find matchArgs ovs

mangleName :: String -> FunctionType -> String
-- |Map pasename + type into mangled name
mangleName basename funt = basename ++ (argsRepr' (ftArgs funt))
  

-- |Preform name mangling and type check
transform :: AST -> AST
transform ast = MS.evalState (transform' ast) initialAnalyzerState
