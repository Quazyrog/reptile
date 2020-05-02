import qualified Tokenizer
import qualified System.IO as IO

fileName = "examples/indent.txt"

main = do
  file <- IO.openFile fileName IO.ReadMode
  src <- IO.hGetContents file
  tkz <- return $ Tokenizer.tokenizer src fileName
  (tk, tkz) <- return $ Tokenizer.getIndent tkz
  putStrLn (show (tk, tkz))
  (tk, tkz) <- return $ Tokenizer.getLiteralS tkz
  putStrLn (show (tk, tkz))
  (tk, tkz) <- return $ Tokenizer.getIndent tkz
  putStrLn (show (tk, tkz))
  (tk, tkz) <- return $ Tokenizer.getOperator tkz
  putStrLn (show (tk, tkz))
  IO.hClose file
