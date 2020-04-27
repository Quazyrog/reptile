module Tokenizer where
import System.IO
import Data.Char (isAlphaNum)

data Token = 
  Indent Int |
  EOL |
  LPar | 
  RPar | 
  Keyword String |
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
      in if discards cont then (rcUnwrap cont, tkz) else rd
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

getKeyword :: Tokenizer -> (Token, Tokenizer)
getKeyword = undefined

getIdentifier :: Tokenizer -> (Token, Tokenizer)
getIdentifier tkz = 
  let
    cont c = if isIdentifierChar c then rcExtend c (RCNext cont) else RCEnd ""
    (token, tkz') = readToken tkz (RCNext cont)
  in (Identifier token, tkz')

getOperator :: Tokenizer -> (Token, Tokenizer)
getOperator = undefined

getLiteralI :: Tokenizer -> (Token, Tokenizer)
getLiteralI = undefined

getLiteralB :: Tokenizer -> (Token, Tokenizer)
getLiteralB = undefined

getLiteralS :: Tokenizer -> (Token, Tokenizer)
getLiteralS = undefined


isIdentifierChar :: Char -> Bool
isIdentifierChar c = Data.Char.isAlphaNum c || c == '_'
