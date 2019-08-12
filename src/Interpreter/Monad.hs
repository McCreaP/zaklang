module Interpreter.Monad where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map

import AbsZaklang

import Interpreter.Data

type IM a = ErrorT String (State Env) a
