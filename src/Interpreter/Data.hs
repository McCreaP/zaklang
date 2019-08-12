module Interpreter.Data where

import qualified Data.Map as Map

import AbsZaklang

data Value
    = VInt Int
    | VBool Bool
    | VClosure [Parameters] [[Value]] Exp Ident Env
    | VCtor Ident
    | VCons Ident [Value]
    deriving (Eq, Show)

type Env = Map.Map Ident Value
