{-# LANGUAGE FlexibleContexts #-}
module Parser where
import qualified Tokenizer as Tkz
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromJust)
import Control.Monad.State.Strict (State, get, put, modify)
import Control.Exception (throw)
import Debug.Trace (trace)
import Control.DeepSeq


data Operator = OP String deriving Show 
instance Eq Operator where
  (OP a) == (OP b) = a == b
instance Ord Operator where
  compare (OP a) (OP b) = compare a b
instance NFData Operator where
  rnf (OP s) = s `seq` ()
data Associativity = ToRight | ToLeft | Unary deriving Show
data Level = 
  OperatorLevel { 
    levelPriotiry :: Int,
    levelAssociativity :: Associativity,
    levelOperators :: Set.Set Operator
  } | 
  LiteralLevel

operators :: Map.Map Int Level
operators = Map.fromList $ (999, LiteralLevel) : (
  map (\l -> (levelPriotiry l, l)) [
    OperatorLevel 0 ToRight (Set.fromList [OP "="]),
    OperatorLevel 1 ToRight (Set.fromList [OP "&&", OP "||"]),
    OperatorLevel 2 Unary   (Set.fromList [OP "!"]),
    OperatorLevel 3 ToRight (Set.fromList [OP "<", OP "<=", OP ">", OP ">=", OP "=="]),
    OperatorLevel 4 ToRight (Set.fromList [OP "+", OP "-"]),
    OperatorLevel 5 ToRight (Set.fromList [OP "*", OP "/", OP "%"]),
    OperatorLevel 6 Unary   (Set.fromList [OP "-"])])

nextLevel :: Level -> Level
nextLevel (OperatorLevel pri _ _) = 
  let (_, greater) = Map.split pri operators in
  snd (Map.findMin greater)

minLevel :: Level
minLevel = snd (Map.findMin operators)

data Expression = 
  UExpr Operator Expression |
  BExpr Operator Expression Expression |
  Var String |
  ConstInt Integer |
  ConstStr String |
  ConstLog Bool
  deriving Show
instance NFData Expression where
  rnf (UExpr op e) = op `seq` (e `deepseq` ())
  rnf (BExpr op e1 e2) = op `seq` (e1 `deepseq` (e2 `deepseq` ()))
  rnf (Var s) = s `seq` ()
  rnf (ConstInt i) = i `seq` ()
  rnf (ConstStr s) = s `seq` ()
  rnf (ConstLog b) = b `seq` ()

dump :: Expression -> String
dump e =
  let 
    ind n = take (2 * n) (repeat ' ')
    dd n (UExpr (OP o) e) = (ind n) ++ "UExpr " ++ o ++ "\n" ++ (dd (n+1) e)
    dd n (BExpr (OP o) e1 e2) = (ind n) ++ "BExpr " ++ o ++ "\n" ++ (dd (n+1) e1) ++ (dd (n+1) e2)
    dd n (Var s) = (ind n) ++ "Var " ++ (show s) ++ "\n"
    dd n (ConstInt i) = (ind n) ++ "Const " ++ (show i) ++ "\n"
  in dd 0 e

data AST = Compute Expression
instance NFData AST where
  rnf (Compute exp) = exp `deepseq` ()

--------------------------------------------------------------------------------
unwrapOP :: Maybe Tkz.Token -> Operator
unwrapOP (Just (Tkz.Operator opname)) = OP opname

parseArg :: State Tkz.Tokenizer Expression
parseArg = do
  modify Tkz.skipWhitespace
  liter <- Tkz.getLiteralI
  if isJust liter then do
    let (Just (Tkz.LiteralI val)) = liter
    return (ConstInt val)
  else do
    modify Tkz.skipWhitespace
    ident <- Tkz.getIdentifier
    if isNothing ident then do 
      tkz <- get
      throw (Tkz.raiseFrom "Expected literal or identifier" tkz)
    else do
      let (Just (Tkz.Identifier vname)) = ident
      return (Var vname)

parseExpression :: Level -> State Tkz.Tokenizer Expression

parseExpression LiteralLevel = do
  tkz <- get
  lpar <- Tkz.getLPar
  if isJust lpar then do
    arg <- parseExpression minLevel
    rpar <- Tkz.getRPar
    if isNothing rpar then do
      tkz <- get
      throw (Tkz.raiseFrom "Expected enclosing parenthesis" tkz)
    else do
      tkz' <- get
      return arg
  else do
    arg <- parseArg
    return arg

parseExpression lvl@(OperatorLevel _ Unary ops) = do
  tkz <- get
  modify Tkz.skipWhitespace
  tkop <- Tkz.getOperator
  if isJust tkop && Set.member (unwrapOP tkop) ops then do
    arg <- parseExpression (nextLevel lvl)
    return (UExpr (unwrapOP tkop) arg)
  else do 
    put tkz
    arg <- parseExpression (nextLevel lvl)
    return arg

parseExpression lvl@(OperatorLevel _ ToRight ops) = do
  arg1 <- parseExpression (nextLevel lvl)
  tkz <- get
  modify Tkz.skipWhitespace
  tkop <- Tkz.getOperator
  if isJust tkop && Set.member (unwrapOP tkop) ops then do
    modify Tkz.skipWhitespace
    arg2 <- parseExpression lvl
    return (BExpr (unwrapOP tkop) arg1 arg2)
  else do
    put tkz
    return arg1

parseExpression (OperatorLevel _ ToLeft _) = undefined