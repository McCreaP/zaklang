module TypeChecker.Monad where

import Control.Monad.Error
import Control.Monad.State
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map

import AbsZaklang

import TypeChecker.Data

data TypeEnv = TypeEnv {typeVarCount :: Int,
                        types :: Map.Map Ident (Scheme TypeInfer),
                        ctors :: Map.Map Ident (Scheme TyCtor)}

type TCM a = ErrorT String (StateT TypeEnv IO) a

readTypeRef :: TyVar -> TCM (Maybe TypeInfer)
readTypeRef v = liftIO $ readIORef (typeRef v)
