import qualified Tokenizer
import qualified System.IO as IO
import Tokenizer as Tkz
import Parser
import qualified Data.Map as Map
import Control.Monad.State.Strict as State
import Control.DeepSeq
import System.Environment (getArgs)
import Data.List (intercalate)

main = do 
  args <- getArgs
  handle <- IO.openFile (args !! 0) IO.ReadMode  
  contents <- IO.hGetContents handle  
  let tkz = Tokenizer.tokenizer contents (args !! 0)
  let blks = State.evalState (parseBlock 0) tkz
  putStrLn (intercalate "\n" (map show blks))
  IO.hClose handle  
