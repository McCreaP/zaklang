module Interpreter.Util where

import Control.Monad.State

import AbsZaklang

import Interpreter.Data
import Interpreter.Monad

local :: IM Value -> IM Value
local computation = do
    currentEnv <- get
    result <- computation
    put currentEnv
    return result

isNamedFunction :: Ident -> Bool
isNamedFunction (Ident name) = not $ null name
