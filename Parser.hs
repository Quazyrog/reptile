{-# LANGUAGE FlexibleContexts #-}
module Parser where
import qualified Tokenizer as Tkz
import qualified Data.Set as Set
import ExpressionsParser (parseExpression, Expression, minLevel)
import Data.Maybe (isJust, isNothing, fromJust)
import Control.Monad.State.Strict (State, get, put, modify)
import Control.Exception (throw)
import Debug.Trace (trace)
import Control.DeepSeq


type Classifier = String -> Bool

typeNames :: Set.Set String
typeNames = Set.fromList ["int", "str", "bool", "auto"]

classifyIdentifier :: Maybe Tkz.Token -> Classifier -> Bool
classifyIdentifier Nothing _ = False
classifyIdentifier (Just (Tkz.Identifier ident)) pred = pred ident

classifyDeclaration :: Classifier
classifyDeclaration ident = Set.member ident typeNames


data AST = 
  Compute Expression |
  Declare String [String]
instance NFData AST where
  rnf (Compute exp) = exp `deepseq` ()
instance Show AST where
  show ast = 
    let
      ind n = take (2 * n) (repeat ' ')
      dd n (Compute e) = (ind n) ++ "Compute " ++ (show e)
      dd n (Declare t vs) = "Declare " ++ t ++ " " ++ (show vs)
    in dd 0 ast

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

parseInstr :: State Tkz.Tokenizer AST
parseInstr = do
  tkz <- get
  identifierToken <- Tkz.getIdentifier
  put tkz
  if classifyIdentifier identifierToken classifyDeclaration then do
    instr <- parseDeclaration
    return instr
  else do
    instr <- parseComputation
    return instr

parseBlock :: Int -> State Tkz.Tokenizer [AST]
parseBlock depth = let indentWidth = 2 in do
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
      return []
    else do
      instr <- parseInstr
      eol <- Tkz.getEOL
      if isJust eol then do
        moreInstr <- parseBlock depth
        return (instr : moreInstr)
      else do 
        tkz <- get
        throw (Tkz.raiseFrom "Expected end of line" tkz)
