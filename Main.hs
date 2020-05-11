import qualified Tokenizer
import qualified System.IO as IO
import qualified Data.Map as Map
import Control.Monad.State.Strict as State
import Control.DeepSeq
import System.Environment (getArgs)
import Data.List (intercalate)
import Tokenizer as Tkz
import Parser
import qualified Intermediate

main = do 
  args <- getArgs
  handle <- IO.openFile (args !! 0) IO.ReadMode  
  contents <- IO.hGetContents handle  
  let tkz = Tokenizer.tokenizer contents (args !! 0)
  let blk = State.evalState (parseBlock 0) tkz
  putStrLn (show blk)
  IO.hClose handle  
