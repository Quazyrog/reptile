import qualified Tokenizer
import qualified System.IO as IO
import Tokenizer as Tkz
import Parser
import qualified Data.Map as Map
import Control.Monad.State.Strict as State
import Control.DeepSeq

parseExpr "q\n" = do return ()
parseExpr line = do
  let tkz = Tkz.tokenizer line "(Unknown)"
  let out = State.evalState (parseExpression minLevel) tkz
  print line
  putStrLn (dump out)
  l <- getLine
  parseExpr (l ++ "\n")

main = do
  l <- getLine
  parseExpr (l ++ "\n")
