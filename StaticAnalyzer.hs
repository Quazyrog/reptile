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
  asClosureState :: Maybe AnalyzerState,
  asFreeVariables :: String,
  asVars :: Vars,
  asFuns :: Overloads,
  asReturn :: VType
}
subscopeState parent = AnalyzerState {
  asClosureState = parent,
  asFreeVariables = [],
  asVars = Map.empty,
  asFuns = stdlibInfo,
  asReturn = IntegerType
}
initialAnalyzerState = subscopeState Nothing

findVariable' :: String -> AnalyzerState -> (VType,  AnalyzerState)
findVariable' name state = 
  let typeMaybe = Map.lookup name (asVars state) in
  if isJust typeMaybe then
    (fromJust typeMaybe, state)
  else if isJust (asClosureState state) then
    let 
      (vtype, closure') = findVariable' name (fromJust (asClosureState state))
      vars' = Map.insert name vtype (asVars state) 
    in (vtype, state { asVars = vars', asClosureState = Just closure' })
  else
    error ("Variable " ++ name ++ " not found in surrounding scopes")

findVariable :: String -> State AnalyzerState VType
findVariable name = do
  state <- MS.get
  let (vtype, state') = findVariable' name state
  MS.put state'
  return vtype

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
  s <- MS.get
  let ss = subscopeState (Just s)
  let (instr', ss') = MS.runState (mapM transform' instr) ss
  MS.put (fromJust (asClosureState ss'))
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
  (expr', _) <- checkType expr
  return (Compute expr')
transform' (Decide condi condiInstr) = do
  (condi', t) <- checkType condi
  if t == BoolType then do
    condiInstr' <- transform' condiInstr
    return (Decide condi' condiInstr')
  else do
    error "Condition in if statement has no boolean type"
transform' other = do return other

injectVar :: VType -> String -> State AnalyzerState ()
-- |Append to state information about variable type
injectVar t name = do
  s <- MS.get
  if isJust (Map.lookup name (asVars s)) then do 
    error ("Redeclaration of variable '" ++ name ++ "'")
  else do
    MS.put s { asVars = Map.insert name t (asVars s) }

checkType :: Expression -> State AnalyzerState (Expression, VType)
-- |Preform type check on expression together with name mangling
checkType c@(ConstInt _) = do return (c, IntegerType)
checkType c@(ConstStr _) = do return (c, StringType)
checkType c@(ConstLog _) = do return (c, BoolType)
checkType v@(EParser.Var vn) = do
  vtype <- findVariable vn
  return (v, vtype)
checkType (Call basename args) = do
  functions <- MS.gets asFuns
  let overloads = Map.lookup basename functions
  if isJust overloads then do
    typedArgs <- mapM typeArg args
    let transformedArgs = map (\(expr, _, _) -> expr) typedArgs
    let argsTypeInfo = map (\(_, ref, t) -> (ref, t)) typedArgs
    let funInfo = lookupOverload (fromJust overloads) argsTypeInfo
    if isJust funInfo then do
      let f = fromJust funInfo
      return (Call (mangleName basename f) transformedArgs, ftReturn f)
    else do
      error ("No suitable overload of '" ++ basename 
        ++ "' for " ++ (show argsTypeInfo))
  else do
    error ("Unknown function '" ++ basename ++"'")

typeArg :: Expression -> State AnalyzerState (Expression, Bool, VType)
-- |Mangle names in expression, check it's type and if it can be passed as reference.
typeArg expr = 
  let 
    isLvalue (EParser.Var _) = True
    isLvalue _ = False
  in do
    (expr', exprType) <- checkType expr
    return (expr', isLvalue expr', exprType)

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
