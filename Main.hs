import qualified System.IO as IO
import qualified System.Exit
import qualified Data.Map as Map
import Control.Monad.State.Strict as MS
import Control.DeepSeq
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Tokenizer
import Parser
import Intermediate
import Compilatron 
import StaticAnalyzer

compileAll :: ProgramInfo -> Map.Map String RuntimeFunctionInfo
compileAll program = 
  let 
    meta = piFunctions program
    code = piFunctionsSources program
    targets = Map.keys meta
    compile target = 
      let 
        rfi = fromJust (Map.lookup target meta)
        source = fromJust (Map.lookup target code)
      in rfi { fBody = compileInstr source }
  in Map.fromList (map (\t -> (t, compile t)) targets)

main = do 
  args <- getArgs
  handle <- IO.openFile (args !! 0) IO.ReadMode  
  contents <- IO.hGetContents handle  
  let tkz = tokenizer contents (args !! 0)
  let ast = MS.evalState (parseBlock 0) tkz
  let ast' = StaticAnalyzer.transform ast
  -- putStrLn (show ast')
  let program = ast' `deepseq` compileAll ast'
  -- All errors should be found by (fully implemented) static analyzer, 
  -- so no need to deepseq the program
  let main = Map.lookup "main" program
  if isJust main then do
    result <- (MS.evalStateT (fBody (fromJust main)) Compilatron.initialState)
    IO.hClose handle
    putStrLn ("\n\n=====\nProgram finished; return value = " ++ (show result))
    System.Exit.exitWith System.Exit.ExitSuccess 
  else do
    putStrLn "Error: no main() function defined"
    IO.hClose handle
    System.Exit.exitWith (System.Exit.ExitFailure 11)
  -- putStrLn ("\nResult: " ++ (show result))
