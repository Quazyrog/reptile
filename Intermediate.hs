module Intermediate where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust, isNothing)
import qualified Control.Monad.State.Strict as MS
import Control.DeepSeq
import Debug.Trace

type InterpIO = MS.StateT ProgramState IO
data ProgramState = PS {
  stateMemory :: Memory,
  stateTopFrame :: Frame,
  stateMemoryCounter :: Integer,
  stateFunctionScope :: Map.Map String RuntimeFunctionInfo
}

------------------------------------ MEMORY ------------------------------------
data VType = IntegerType | BoolType | StringType
data VData = VInt Integer | VBool Bool | VStr String deriving Show
instance NFData VData where
  rnf (VInt v) = v `seq` ()
  rnf (VBool v) = v `seq` ()
  rnf (VStr v) = v `seq` ()
data Variable = Var Int VData
type Memory = Map.Map Integer Variable
type Frame = Map.Map String Integer

find :: Memory -> Integer -> Variable
find memory id =
  let store = Map.lookup id memory in 
  if isJust store then 
    fromJust store
  else 
    error "[BUG] Dereference deleted variable"

getVar :: Frame -> String -> InterpIO VData
getVar frame vname = do
  s <- MS.get
  let ref = Map.lookup vname frame
  if isJust ref then do
    let (Var _ d) = find (stateMemory s) (fromJust ref)
    return d
  else do
    error "[BUG] No entry in frame"

updateMemory :: (Memory -> Memory) -> InterpIO ()
updateMemory modify = do
  state <- MS.get
  MS.put (state {stateMemory = modify (stateMemory state)})
  return ()

putRef :: VData -> InterpIO Integer
putRef var = do
  ps <- MS.get
  let ref = stateMemoryCounter ps
  let mem = (Var 1 var)
  MS.put ps { 
    stateMemory = (Map.insert ref mem (stateMemory ps)),
    stateMemoryCounter = ref + 1 }
  return ref

incRef :: Integer -> InterpIO ()
incRef id = do
    state <- MS.get
    let (Var refc val) = find (stateMemory state) id
    updateMemory (\m -> Map.insert id (Var (refc + 1) val) m)
    return ()
  
decRef :: Integer -> InterpIO ()
decRef id = do
  state <- MS.get
  let (Var refc val) = find (stateMemory state) id
  if refc > 1 then do
    updateMemory (\m -> Map.insert id (Var (refc - 1) val) m)
    return ()
  else do
    updateMemory (\m -> Map.delete id m)
    return ()
  
updateRef :: Integer -> (VData -> VData) -> InterpIO ()
updateRef id operation = do
  state <- MS.get
  let (Var refc val) = find (stateMemory state) id
  updateMemory (\m -> Map.insert id (Var (refc + 1) (operation val)) m)
  return ()

---------------------------------- FUNCTIONS -----------------------------------
data ArgPassType = PassRef | PassVal
data Arg = Value VData | Reference Integer
type RFIArg = (String, ArgPassType, VType)
type FunctionBody = ByteCode
data RuntimeFunctionInfo = RFI {
  fName :: String,
  fBoundVariables :: [String],
  fFreeVariables :: [String],
  fLocalFunctions :: Map.Map String RuntimeFunctionInfo,
  fArgsTypes :: [RFIArg],
  fReturnType :: VType,
  fBody :: FunctionBody
}

type ByteCode = InterpIO (Maybe VData)

updateFrame :: Frame -> [RFIArg] -> [Arg] -> InterpIO Frame
updateFrame f [] [] = do return f
updateFrame f ((name, PassRef, _):ais) ((Reference id):as) = do
  incRef id
  let f' = Map.insert name id f
  f'' <- updateFrame f' ais as
  return f''
updateFrame f ((name, PassVal, t):ais) ((Value val):as) = do
  id <- putRef val
  f' <- updateFrame f ((name, PassRef, t):ais) ((Reference id):as)
  return f'

shutdownFrame :: Frame -> InterpIO ()
shutdownFrame f = do
  mapM decRef (Map.elems f)
  return ()

applyArgs :: FunctionBody -> Frame -> [RFIArg] -> InterpIO ([Arg] -> ByteCode)
applyArgs body closure argInfo = 
  return (\args -> do
    f <- updateFrame closure argInfo args
    ps <- MS.get
    MS.put (f `deepseq` (ps { stateTopFrame = f }))
    retval <- body
    retval `deepseq` (shutdownFrame f)
    MS.modify (\s -> s { stateTopFrame = closure })
    return retval)

instantiateFunction :: RuntimeFunctionInfo -> InterpIO ([Arg] -> ByteCode)
instantiateFunction rfi = 
  let 
    xd f vn = do
      let id = Map.lookup vn f
      if isJust id then do 
        incRef (fromJust id)
        return (vn, fromJust id)
      else 
        error "[BUG] cannot dereference free variable"
  in do
  ps <- MS.get
  let closure = stateTopFrame ps
  freev <- mapM (xd closure) (fFreeVariables rfi)
  res <- applyArgs (fBody rfi) (Map.fromList freev) (fArgsTypes rfi)
  return res

argRepr :: [RFIArg] -> String
argRepr _ = "" -- FIXME temporarily for texting
argRepr [] = ""
argRepr ((_, pass, t):args) = 
  let 
    pStr PassRef = "&"
    pStr PassVal = "="
    tStr IntegerType = "i"
    tStr BoolType = "b"
    tStr StringType = "s"
  in "(" ++ (pStr pass) ++ (tStr t) ++ ")" ++ (argRepr args)
