{-# LANGUAGE FlexibleContexts #-}
module Tokenizer where
import System.IO
import Data.Char
import Control.Exception
import Control.Monad.State.Strict (State, get, put)


data ParserException = ParseError String Int Int String
instance Show ParserException where
  show (ParseError file line column message) =
    file ++ ":" ++ (show line) ++ ":" ++ (show column) ++ ":  " ++ message
instance Exception ParserException
raiseFrom :: String -> Tokenizer -> ParserException
raiseFrom msg (Tokenizer _ fn l c) = ParseError fn l c msg


data Token = 
  Indent Int |
  EOL |
  LPar | 
  RPar | 
  Identifier String |
  Operator String |
  LiteralI Integer |
  LiteralB Bool |
  LiteralS String

instance Show Token where
  show (Indent n) = "Indent{" ++ (show n) ++ "}"
  show EOL = "EOL"
  show LPar = "LPar"
  show RPar = "RPar"
  show (Identifier s) = "Identifier{" ++ s ++ "}"
  show (Operator s) = "Operator{" ++ s ++ "}"
  show (LiteralI v) = "LiteralI{" ++ (show v) ++ "}"
  show (LiteralB v) = "LiteralB{" ++ (show v) ++ "}"
  show (LiteralS v) = "LiteralS{" ++ (show v) ++ "}"

data Tokenizer = Tokenizer {
  source :: String,
  fileName :: String,
  line :: Int,
  column :: Int
}

instance Show Tokenizer where
  show (Tokenizer _ fn l c) = 
    "Tokenizer{" ++ fn ++ ":" ++ (show l) ++ ":" ++ (show c) ++ "}"


tokenizer :: String -> String -> Tokenizer
tokenizer src fn = Tokenizer src fn 1 1

atEOF :: Tokenizer -> Bool
atEOF (Tokenizer src _ _ _) = 
  let 
    isBlank "" = True
    isBlank (c:cs) = if Data.Char.isSpace c then isBlank cs else False
  in isBlank src

hasNextChar :: Tokenizer -> Bool
hasNextChar (Tokenizer [] _ _ _) = False
hasNextChar (Tokenizer _ _ _ _) = True

nextChar :: Tokenizer -> (Char, Tokenizer)
nextChar (Tokenizer ('\n':cs) fn l c) = ('\n', Tokenizer cs fn (l + 1) 1)
nextChar (Tokenizer (ch:cs) fn l c) = (ch, Tokenizer cs fn l (c + 1))
nextChar (Tokenizer [] fn l c) = throw (ParseError fn l c "Unexpected EOF")

skipWhitespace :: Tokenizer -> Tokenizer
skipWhitespace tkz = snd (readToken tkz (rcFromPred (==' ')))

matchChar :: Tokenizer -> Char -> (Bool, Tokenizer)
matchChar tkz c = 
  let (c', tkz') = nextChar tkz in
  if c == c' then (True, tkz') else (False, tkz)

matchString :: Tokenizer -> String -> (Bool, Tokenizer)
matchString tkz expected = 
  let
    cont (e:es) c = if e == c then RCNext (cont es) else RCEnd ""
    cont [] _ = RCEnd "+"
    (s, tkz') = readToken tkz (RCNext (cont expected))
  in if s /= "" then (True, tkz') else (False, tkz)


data ReadingCont = RCNext (Char -> ReadingCont) | RCEnd String

rcUnwrap :: ReadingCont -> String
rcUnwrap (RCEnd acc) = acc
rcUnwrap _ = undefined

rcExtend :: Char -> ReadingCont -> ReadingCont
rcExtend c (RCEnd s) = RCEnd (c : s)
rcExtend c (RCNext f) = RCNext (\x -> rcExtend c (f x))

rcFromPred :: (Char -> Bool) -> ReadingCont
rcFromPred pred = 
  let cont c = if pred c then rcExtend c (RCNext cont) else RCEnd "" in
  RCNext cont

readToken :: Tokenizer -> ReadingCont -> (String, Tokenizer)
readToken tokenizer start = 
  let
    discards (RCEnd _) = True
    discards _ = False
    read tkz (RCEnd ret) = (ret, tkz)
    read tkz (RCNext nx) = 
      let 
        (c, tkz') = nextChar tkz 
        cont = nx c
        rd = read tkz' cont
      in if discards cont then (rcUnwrap cont , tkz) else rd
  in read tokenizer start


getIndent :: State Tokenizer Token
getIndent =
  let
    cont _ '\n' = RCNext (cont "")
    cont acc ' ' = RCNext (cont (' ':acc))
    cont acc _ = RCEnd acc
  in do 
  tkz <- get
  let (indent, tkz') = readToken tkz (RCNext (cont ""))
  put tkz'
  return (Indent (length indent))

getEOL :: State Tokenizer (Maybe Token)
getEOL = do
  tkz <- get
  let (m, tkz') = matchChar (skipWhitespace tkz) '\n'
  if m then do 
    put tkz'
    return (Just EOL)
  else do
    return Nothing

getLPar :: State Tokenizer (Maybe Token)
getLPar = do
  tkz <- get
  let (m, tkz') = matchChar (skipWhitespace tkz) '('
  if m then do 
    put tkz'
    return (Just LPar)
  else do
    return Nothing

getRPar :: State Tokenizer (Maybe Token)
getRPar = do
  tkz <- get
  let (m, tkz') = matchChar (skipWhitespace tkz) ')'
  if m then do 
    put tkz'
    return (Just RPar)
  else do
    return Nothing

getIdentifier :: State Tokenizer (Maybe Token)
getIdentifier = do
  tkz <- get
  let (token, tkz') = readToken tkz (rcFromPred isIdentifierChar)
  if token /= "" then do
    put tkz'
    return (Just (Identifier token))
  else do
    return Nothing

getOperator :: State Tokenizer (Maybe Token)
getOperator =
  let
    rdInit '`' = RCNext rdQuoted
    rdInit c = 
      if isOperatorChar c then 
        rcExtend c (rcFromPred isOperatorChar) 
      else 
        RCEnd ""
    rdQuoted '`' = RCNext (\_ -> RCEnd "")
    rdQuoted c = rcExtend c (RCNext rdQuoted)
  in do 
  tkz <- get
  let (op, tkz') = readToken tkz (RCNext rdInit)
  if op /= "" then do
    put tkz'
    return (Just (Operator op))
  else do
    return Nothing

getLiteralI :: State Tokenizer (Maybe Token)
getLiteralI = 
  let isDigit c = isNumber c && isAscii c in do 
  tkz <- get
  let (token, tkz') = readToken tkz (rcFromPred isDigit)
  if token /= "" then do
    put tkz'
    return (Just (LiteralI (read token :: Integer)))
  else do
    return Nothing

getLiteralB :: State Tokenizer (Maybe Token)
getLiteralB = do
  tkz <- get
  let (matchTrue, trueTkz) = matchString tkz "true"
  let (matchFalse, falseTkz) = matchString tkz "false"
  if matchTrue then do
    put trueTkz
    return (Just (LiteralB True))
  else if matchFalse then do
    put falseTkz
    return (Just (LiteralB False))
  else do
    return Nothing

-- FIXME escape seq
getLiteralS :: State Tokenizer (Maybe Token)
getLiteralS =
  let
    contIni '"' = rcExtend '+' (RCNext cont)
    contIni _ = RCEnd ""
    cont '"' = RCNext (\_ -> RCEnd "")
    cont c = rcExtend c (RCNext cont)
  in do 
  tkz <- get
  let (rd, tkz') = readToken tkz (RCNext contIni)
  if rd /= "" then do
    put tkz'
    return (Just (LiteralS (tail rd)))
  else
    return Nothing

expect :: String -> State Tokenizer ()
expect s = do
  tkz <- get
  let (matched, tkz') = matchString tkz s
  if matched then do
    put tkz'
    return ()
  else do
    throw (raiseFrom ("Expected '" ++ s ++ "'") tkz)

isIdentifierChar :: Char -> Bool
isIdentifierChar c = (Data.Char.isAlphaNum c && Data.Char.isAscii c) || c == '_'

isOperatorChar :: Char -> Bool
isOperatorChar c = elem c "|!@%^&*<>=-+/"
