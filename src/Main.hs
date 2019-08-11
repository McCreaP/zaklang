module Main where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as Map
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexZaklang
import ParZaklang
import AbsZaklang

import ErrM

import TypeChecker.TypeChecker ( inferProg )
import TypeChecker.Monad ( TypeEnv(..) )
import Interpreter.Interpreter ( denoteProgram )

type ParseFun a = [Token] -> Err a

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

run :: ParseFun Program -> String -> IO ()
run parser program = let tokens = myLexer program in case parser tokens of
    Bad s -> error $ "Parse failed: " ++ s
    Ok tree -> do
        (value, _) <- runStateT (runErrorT $ inferProg tree) (TypeEnv 0 Map.empty Map.empty)
        case value of
            Left errMsg -> error $ "Type error: " ++ errMsg
            Right _ -> runProgram tree

runProgram tree =
    let (value, _) = runIdentity $ runStateT (runErrorT $ denoteProgram tree) Map.empty in
        case value of
            Left errMsg -> error $ "Runtime Error: " ++ errMsg
            Right value -> putStrLn $ show value
