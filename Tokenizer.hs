module Tokenizer where
import System.IO
import Data.Char (isAlphaNum, isNumber, isAscii)

-- TODO invalid token
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

hasNextChar :: Tokenizer -> Bool
hasNextChar (Tokenizer [] _ _ _) = False
hasNextChar (Tokenizer _ _ _ _) = True

nextChar :: Tokenizer -> (Char, Tokenizer)
nextChar (Tokenizer ('\n':cs) fn l c) = ('\n', Tokenizer cs fn (l + 1) 1)
nextChar (Tokenizer (ch:cs) fn l c) = (ch, Tokenizer cs fn l (c + 1))
nextChar (Tokenizer [] _ _ _) = error "EOF"


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


getIndent :: Tokenizer -> (Token, Tokenizer)
getIndent tkz = 
  let
    cont '\n' = RCNext cont
    cont ' ' = rcExtend ' ' (RCNext cont)
    cont _ = RCEnd ""
    (indent, tkz') = readToken tkz (RCNext cont)
  in (Indent $ length indent, tkz')

getEOL :: Tokenizer -> (Token, Tokenizer)
getEOL = undefined

getLPar :: Tokenizer -> (Token, Tokenizer)
getLPar = undefined

getRPar  :: Tokenizer -> (Token, Tokenizer)
getRPar = undefined

getIdentifier :: Tokenizer -> (Token, Tokenizer)
getIdentifier tkz = 
  let
    (token, tkz') = readToken tkz (rcFromPred isIdentifierChar)
  in (Identifier token, tkz')

getOperator :: Tokenizer -> (Token, Tokenizer)
getOperator tkz = 
  let 
    (fstc, tkz') = nextChar tkz 
    (op, tkz'') = 
      if fstc == '`' then 
        let (rd, tk) = readToken tkz' (rcFromPred (\c -> c /= '`')) in
        (rd, snd (nextChar tk))
      else readToken tkz' (rcFromPred isIdentifierChar)
  in (Operator op, tkz'')
  

getLiteralI :: Tokenizer -> (Token, Tokenizer)
getLiteralI tkz = 
  let
    isDigit c = isNumber c && isAscii c
    (token, tkz') = readToken tkz (rcFromPred isDigit)
  in (LiteralI (read token :: Integer), tkz')

getLiteralB :: Tokenizer -> (Token, Tokenizer)
getLiteralB tkz = 
  let
    (trueStr, trueTkz) = getExactly tkz "true"
    (falseStr, falseTkz) = getExactly tkz "true"
  in 
  if trueStr == "true" then (LiteralB True, trueTkz) 
  else (LiteralB False, falseTkz)

-- FIXME exceptions; escape seq
getLiteralS :: Tokenizer -> (Token, Tokenizer)
getLiteralS tkz = 
  let
    contIni '"' = rcExtend '+' (RCNext cont)
    contIni _ = RCEnd ""
    cont '"' = RCEnd ""
    cont c = rcExtend c (RCNext cont)
    (rd, tkz') = readToken tkz (RCNext contIni)
  in (LiteralS rd, tkz')

getExactly :: Tokenizer -> String -> (String, Tokenizer)
getExactly tkz expected = 
  let
    cont (e:es) c = if e == c then RCNext (cont es) else RCEnd ""
    cont [] _ = RCEnd "+"
    (rd, tkz') = readToken tkz (RCNext (cont expected))
  in if rd == "" then ("", tkz) else (rd, tkz')


isIdentifierChar :: Char -> Bool
isIdentifierChar c = (Data.Char.isAlphaNum c && Data.Char.isAscii c) || c == '_'

isOperatorChar :: Char -> Bool
isOperatorChar c = elem c "!@%^&*<>=:"

