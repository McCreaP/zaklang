import System.IO ( stdin, hGetContents )
import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs, getProgName )
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map

import LexZaklang
import ParZaklang
import SkelZaklang
import PrintZaklang
import AbsZaklang

import ErrM

import TypeChecker.Show
import TypeChecker.Monad
import TypeChecker.TypeChecker

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"] -> usage
        ["--help"] -> usage
        [] -> hGetContents stdin >>= run pProgram
        [file] -> readFile file >>= run pProgram
        _ -> usage

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ unlines
        ["usage: ./" ++ progName ++ "[file]"
        ,"file      file with a source code"
        , "If file is not specifier, parse stdin"
        ]
    exitFailure

run parser program = let tokens = myLexer program in case parser tokens of
    Bad s -> do
        putStrLn $ "Parse failed: " ++ s
        exitFailure
    Ok tree -> do
        (value, typeEnv) <- runStateT (runErrorT $ inferProg tree) (TypeEnv 0 Map.empty Map.empty)
        case value of
            Left errMsg -> error $ "Error: " ++ errMsg
            Right _ -> runStateT (runErrorT $ printDeclTypes) typeEnv >> return ()

printDeclTypes :: TCM ()
printDeclTypes = gets types >>= mapM_ printType . Map.toList >>
    gets ctors >>= mapM_ printType . Map.toList
  where
    printType (k, a) = do
        scheme <- prune a
        s <- showTCM scheme
        liftIO $ putStrLn $ show k ++ " :: " ++ s
