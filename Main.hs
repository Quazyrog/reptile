import qualified Tokenizer
import qualified System.IO as IO
import qualified Data.Map as Map
import Control.Monad.State.Strict as State
import Control.DeepSeq
import System.Environment (getArgs)
import Data.List (intercalate)
import Tokenizer as Tkz
import Parser
import qualified Compilatron 
import qualified StaticAnalyzer

main = do 
  args <- getArgs
  handle <- IO.openFile (args !! 0) IO.ReadMode  
  contents <- IO.hGetContents handle  
  let tkz = Tokenizer.tokenizer contents (args !! 0)
  let ast = State.evalState (parseBlock 0) tkz
  let ast' = StaticAnalyzer.transform ast
  -- putStrLn (show ast')
  let program = Compilatron.compileInstr ast'
  result <- ast' `deepseq` (State.evalStateT program Compilatron.initialState)
  -- putStrLn ("\nResult: " ++ (show result))
  IO.hClose handle  
