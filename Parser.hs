{-# LANGUAGE FlexibleContexts #-}
module Parser where
import qualified Tokenizer as Tkz
import qualified Data.Set as Set
import ExpressionsParser (parseExpression, Expression, minLevel)
import Data.Maybe (isJust, isNothing, fromJust)
import Control.Monad.State.Strict (State, get, put, modify)
import Control.Exception (throw)
import Control.DeepSeq
import Data.List (intercalate)


type Classifier = String -> Bool

indentWidth = 2

typeNames :: Set.Set String
typeNames = Set.fromList ["int", "str", "bool", "auto"]

classifyIdentifier :: Maybe Tkz.Token -> Classifier -> Bool
classifyIdentifier Nothing _ = False
classifyIdentifier (Just (Tkz.Identifier ident)) pred = pred ident

isDeclaration :: Classifier
isDeclaration ident = Set.member ident typeNames

isIf :: Classifier
isIf ident = ident == "if"

isFun :: Classifier
isFun ident = ident == "def"

data Argument = 
  ByVal String String |
  ByRef String String |
  ByConstVal String String
  deriving Show
instance NFData Argument where
  rnf (ByVal n t) = n `deepseq` t `deepseq` ()
  rnf (ByRef n t) = n `deepseq` t `deepseq` ()
  rnf (ByConstVal n t) = n `deepseq` t `deepseq` ()

data AST = 
  DoAll [AST] |
  Decide Expression AST |
  Compute Expression |
  Declare String [String] |
  DeclareFun String [Argument] String AST
instance NFData AST where
  rnf (DoAll instrs) = instrs `deepseq` ()
  rnf (Decide expr condInstr) = expr `deepseq` condInstr `deepseq` ()
  rnf (Compute exp) = exp `deepseq` ()
  rnf (Declare t vs) = t `deepseq` vs `deepseq` ()
  rnf (DeclareFun vn args rt body) = 
    vn `deepseq` args `deepseq` rt `deepseq` body `deepseq` ()
instance Show AST where
  show ast = 
    let
      ind n = take (2 * n) (repeat ' ')
      dd n (Compute e) = (ind n) ++ "Compute " ++ (show e)
      dd n (Declare t vs) = (ind n) ++ "Declare " ++ t ++ " " ++ (show vs)
      dd n (Decide c i1) = 
        (ind n) ++ "Decide " ++ (show c) ++ "\n" ++ (dd n i1)
      dd n (DoAll []) = (ind (n + 1)) ++ "Noop"
      dd n (DoAll is) = (intercalate "\n" (map (dd (n + 1)) is))
      dd n (DeclareFun fn args t blk) =
        (ind n) ++ "Function " ++ fn ++ ": " ++ (show args) 
          ++ " -> " ++ t ++ "\n" ++ (dd n blk)
    in dd 0 ast

assureEOL :: State Tkz.Tokenizer ()
assureEOL = do
  tkz <- get
  eol <- Tkz.getEOL
  put tkz
  if isJust eol then return () else throw (Tkz.raiseFrom "End of line expected" tkz)

required :: State Tkz.Tokenizer (Maybe Tkz.Token) -> State Tkz.Tokenizer Tkz.Token
required getter = do
  tk <- getter
  if isJust tk then do
    return (fromJust tk)
  else do
    tkz <- get
    throw (Tkz.raiseFrom "Required token not found" tkz)

parseDeclaration :: State Tkz.Tokenizer AST
parseDeclaration = 
  let
    parseVarNames = do
      modify Tkz.skipWhitespace
      nameToken <- Tkz.getIdentifier
      if isJust nameToken then do
        let (Tkz.Identifier name) = fromJust nameToken
        modify Tkz.skipWhitespace
        tkz <- get
        let (continue, tkz') = Tkz.matchChar tkz ','
        if continue then do
          put tkz'
          moreNames <- parseVarNames
          return (name : moreNames)
        else do
          return [name]
      else do
        tkz <- get
        throw (Tkz.raiseFrom "Expected variable identifier" tkz)
  in do
  typeToken <- Tkz.getIdentifier
  if isJust typeToken then do
    let (Tkz.Identifier typeName) = fromJust typeToken
    names <- parseVarNames
    return (Declare typeName names)
  else do 
    tkz <- get
    throw (Tkz.raiseFrom "Expected type identifier" tkz)

parseComputation :: State Tkz.Tokenizer AST
parseComputation = do
  expr <- parseExpression minLevel
  return (Compute expr)

parseIf :: Int -> State Tkz.Tokenizer AST
parseIf depth = do
  Tkz.expect "if"
  modify Tkz.skipWhitespace
  expr <- parseExpression minLevel
  modify Tkz.skipWhitespace
  Tkz.expect ":"
  thenBlock <- parseBlock (depth + 1)
  return (Decide expr thenBlock)

parseFun :: Int -> State Tkz.Tokenizer AST
parseFun depth = 
  let 
    isMode (Just (Tkz.Identifier "val")) = True
    isMode (Just (Tkz.Identifier "const")) = True
    isMode (Just (Tkz.Identifier "ref")) = True
    isMode _ = False
    mkArg (Just (Tkz.Identifier "val")) (Tkz.Identifier n) (Tkz.Identifier t) =
       ByVal n t
    mkArg (Just (Tkz.Identifier "const")) (Tkz.Identifier n) (Tkz.Identifier t) =
      ByConstVal n t
    mkArg (Just (Tkz.Identifier "ref")) (Tkz.Identifier n) (Tkz.Identifier t) =
      ByRef n t
    parseArgList = do
      tkz <- get
      modify Tkz.skipWhitespace
      mode <- Tkz.getIdentifier
      if isMode mode then do
        modify Tkz.skipWhitespace
        argname <- required Tkz.getIdentifier
        modify Tkz.skipWhitespace
        Tkz.expect "as"
        modify Tkz.skipWhitespace
        argtype <- required Tkz.getIdentifier
        modify Tkz.skipWhitespace
        tkz' <- get
        let (next, tkz'') = Tkz.matchChar tkz' ','
        if next then do
          put tkz''
          more <- parseArgList
          return ((mkArg mode argname argtype) : more)
        else do
          return [mkArg mode argname argtype]
      else do
        put tkz
        return []
    mkFun (Tkz.Identifier n) (Tkz.Identifier t) args blk = 
      DeclareFun n args t blk
  in do
  Tkz.expect "def"
  modify Tkz.skipWhitespace
  fname <- required Tkz.getIdentifier
  modify Tkz.skipWhitespace
  required Tkz.getLPar
  args <- parseArgList
  modify Tkz.skipWhitespace
  required Tkz.getRPar
  modify Tkz.skipWhitespace
  Tkz.expect "as"
  modify Tkz.skipWhitespace
  tname <- required Tkz.getIdentifier
  modify Tkz.skipWhitespace
  Tkz.expect ":"
  blk <- parseBlock (depth + 1)
  return (mkFun fname tname args blk)
  
parseInstr :: Int -> State Tkz.Tokenizer AST
parseInstr depth = do
  tkz <- get
  identifierToken <- Tkz.getIdentifier
  let classify = classifyIdentifier identifierToken
  put tkz
  if classify isDeclaration then do
    instr <- parseDeclaration
    assureEOL
    return instr
  else if classify isIf then do
    instr <- parseIf depth
    assureEOL
    return instr
  else if classify isFun then do
    instr <- parseFun depth
    assureEOL
    return instr
  else do
    instr <- parseComputation 
    assureEOL
    return instr

parseBlock :: Int -> State Tkz.Tokenizer AST
parseBlock depth = 
  let 
    parseInstrs depth = do
      tkz <- get
      if Tkz.atEOF tkz then do
        return []
      else do
        wtf <- Tkz.getIndent
        let (Tkz.Indent w) = wtf
        if w `mod` indentWidth /= 0 || w `div` indentWidth > depth then do
          tkz <- get
          throw (Tkz.raiseFrom "Invalid indentation" tkz)
        else if w `div` indentWidth < depth then do
          put tkz
          return []
        else do
          instr <- parseInstr depth
          moreInstr <- parseInstrs depth
          return (instr : moreInstr)
  in do
  instrs <- parseInstrs depth
  return (DoAll instrs)
