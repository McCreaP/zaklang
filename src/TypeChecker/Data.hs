module TypeChecker.Data where

import Data.IORef (IORef)
import qualified Data.Map as Map

import AbsZaklang

data TyVar = TV {name :: String, typeRef :: IORef (Maybe TypeInfer)} deriving (Eq)

data TypeInfer = TyVar TyVar | TyCons String [TypeInfer] deriving (Eq, Show)
data Scheme a = Forall [TyVar] a deriving (Show)

data TyCtor = TC {ident :: Ident, constructedType :: TypeInfer, args :: [TypeInfer]}

instance Show TyVar where
    show = name

instance Ord TyVar where
    compare v v' = compare (name v) (name v')
