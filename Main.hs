import qualified Tokenizer
import qualified System.IO as IO

fileName = "examples/indent.txt"

main = do
  file <- IO.openFile fileName IO.ReadMode
  src <- IO.hGetContents file
  let tkz0 = Tokenizer.tokenizer src fileName
  let (tk1, tkz1) = Tokenizer.getIndent tkz0
  putStrLn (show (tk1, tkz1))
  let (tk2, tkz2) = Tokenizer.getIdentifier tkz1
  putStrLn (show (tk2, tkz2))
  let (tk3, tkz3) = Tokenizer.getIndent tkz2
  putStrLn (show (tk3, tkz3))
  let (tk4, tkz4) = Tokenizer.getIdentifier tkz3
  putStrLn (show (tk4, tkz4))
  IO.hClose file
