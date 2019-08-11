{-# LANGUAGE LambdaCase #-}
module TypeChecker.FreeTypeVariables where

import Control.Monad.State
import qualified Data.Set as Set

import TypeChecker.Data
import TypeChecker.Monad

class Ftv a where
    ftv :: a -> TCM (Set.Set TyVar)

instance Ftv TyVar where
    ftv v = readTypeRef v >>= \case
        Just t -> ftv t
        Nothing -> return $ Set.singleton v

instance Ftv TypeInfer where
    ftv (TyVar v) = ftv v
    ftv (TyCons _ types) = ftv types

instance Ftv TyCtor where
    ftv (TC _ constructedType args) = liftM2 Set.union (ftv args) (ftv constructedType)

instance Ftv a => Ftv (Scheme a) where
    ftv (Forall tvs t) = ftv t >>= return . flip Set.difference (Set.fromList tvs) -- ftv tvs

instance Ftv a => Ftv [a] where
    ftv types = union $ mapM ftv types
        where union xs = xs >>= return . foldr Set.union Set.empty
