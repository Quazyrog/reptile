{-# LANGUAGE FlexibleContexts #-}
module ExpressionsParser where
import qualified Tokenizer as Tkz
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromJust)
import Control.Monad.State.Strict (State, get, put, modify)
import Control.Exception (throw)
import Debug.Trace (trace)
import Control.DeepSeq
import Data.List (intercalate)

type Operator = String
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
    OperatorLevel 0 ToRight (Set.fromList ["="]),
    OperatorLevel 1 ToRight (Set.fromList ["&&", "||"]),
    OperatorLevel 2 Unary   (Set.fromList ["!"]),
    OperatorLevel 3 ToRight (Set.fromList ["<", "<=", ">", ">=", "=="]),
    OperatorLevel 4 ToLeft (Set.fromList ["+", "-"]), -- string concatenation
    OperatorLevel 5 ToRight (Set.fromList ["*", "/", "%"]),
    OperatorLevel 6 Unary   (Set.fromList ["-"]),
    OperatorLevel 7 ToRight  (Set.fromList ["**"])])

nextLevel :: Level -> Level
nextLevel (OperatorLevel pri _ _) = 
  let (_, greater) = Map.split pri operators in
  snd (Map.findMin greater)

minLevel :: Level
minLevel = snd (Map.findMin operators)

data Expression = 
  Call String [Expression] |
  Var String |
  ConstInt Integer |
  ConstStr String |
  ConstLog Bool
instance NFData Expression where
  rnf (Call fun args) = fun `seq` args `seq` ()
  rnf (Var s) = s `seq` ()
  rnf (ConstInt i) = i `seq` ()
  rnf (ConstStr s) = s `seq` ()
  rnf (ConstLog b) = b `seq` ()
instance Show Expression where
  show (Call fn es) = fn ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
  show (Var s) = "$" ++ s
  show (ConstInt i) = show i
  show (ConstStr s) = show s
  show (ConstLog b) = show b

dump :: Expression -> String
dump e =
  let 
    ind n = take (2 * n) (repeat ' ')
    dd n (Call fun args) = (ind n) ++ "Call " ++ fun ++ "\n" 
        ++ foldr (++) "" (map (dd (n+1)) args)
    dd n (Var s) = (ind n) ++ "Var " ++ (show s) ++ "\n"
    dd n (ConstInt i) = (ind n) ++ "Const " ++ (show i) ++ "\n"
    dd n (ConstStr s) = (ind n) ++ "Const " ++ (show s) ++ "\n"
    dd n (ConstLog b) = (ind n) ++ "Const " ++ (show b) ++ "\n"
  in dd 0 e

unwrapOP :: Maybe Tkz.Token -> Operator
unwrapOP (Just (Tkz.Operator opname)) = opname

parseArgList :: State Tkz.Tokenizer [Expression]
parseArgList = do
  modify Tkz.skipWhitespace
  s <- get
  rpar <- Tkz.getRPar
  if isJust rpar then do
    put s
    return []
  else do
    expr <- parseExpression minLevel
    modify Tkz.skipWhitespace
    tkz <- get
    let (doNext, tkz') = Tkz.matchChar tkz ','
    if doNext then do
      put tkz'
      rem <- parseArgList
      return (expr : rem)
    else do
      return [expr]

parseArg :: State Tkz.Tokenizer Expression
parseArg = do
  modify Tkz.skipWhitespace
  literi <- Tkz.getLiteralI
  liters <- Tkz.getLiteralS
  literb <- Tkz.getLiteralB
  if isJust literi then do
    let (Just (Tkz.LiteralI val)) = literi
    return (ConstInt val)
  else if isJust liters then do
    let (Just (Tkz.LiteralS val)) = liters
    return (ConstStr val)
  else if isJust literb then do
    let (Just (Tkz.LiteralB val)) = literb
    return (ConstLog val)
  else do
    ident <- Tkz.getIdentifier
    if isNothing ident then do 
      tkz <- get
      throw (Tkz.raiseFrom "Expected literal or identifier" tkz)
    else do
      lpar <- Tkz.getLPar
      if isNothing lpar then do
        let (Just (Tkz.Identifier vname)) = ident
        return (Var vname)
      else do
        args <- parseArgList
        rpar <- Tkz.getRPar
        if isNothing rpar then do
          tkz <- get
          throw (Tkz.raiseFrom "Expected enclosing parenthesis" tkz)
        else do
          let (Tkz.Identifier fn) = fromJust ident
          return (trace (show (Call fn args)) (Call fn args))

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
    return (Call (unwrapOP tkop) [arg])
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
    return (Call (unwrapOP tkop) [arg1, arg2])
  else do
    put tkz
    return arg1

parseExpression lvl@(OperatorLevel _ ToLeft ops) = 
  let 
    parsemore lhs = do
      s <- get
      modify Tkz.skipWhitespace
      tkop <- Tkz.getOperator
      if isJust tkop && Set.member (unwrapOP tkop) ops then do
        rhs <- parseExpression (nextLevel lvl)
        let lhs' = Call (unwrapOP tkop) [lhs, rhs]
        out <- parsemore lhs'
        return out
      else do
        put s
        return lhs
  in do
  modify Tkz.skipWhitespace
  lhs <- parseExpression (nextLevel lvl)
  out <- parsemore lhs
  return out