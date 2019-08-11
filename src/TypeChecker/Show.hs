{-# LANGUAGE LambdaCase #-}
module TypeChecker.Show where

import TypeChecker.Data
import TypeChecker.Monad

import AbsZaklang

class ShowTCM a where
    showTCM :: a -> TCM String

instance ShowTCM TyVar where
    showTCM v = readTypeRef v >>= \case
        Just t -> showTCM t
        Nothing -> return $ name v

instance ShowTCM TypeInfer where
    showTCM (TyVar v) = showTCM v
    showTCM (TyCons "=>" [a, b]) = do
        s1 <- showTCM a
        s2 <- showTCM b
        return $ "(" ++ s1 ++ " => " ++ s2 ++ ")"
    showTCM (TyCons "*" [x]) = showTCM x
    showTCM (TyCons "*" types) = do
        xs <- mapM showTCM types
        return $ "(" ++ showStrings xs ++ ")"
    showTCM (TyCons name []) = return name
    showTCM (TyCons name types) = do
        xs <- mapM showTCM types
        return $ name ++ " [" ++ showStrings xs ++ "]"

instance ShowTCM TyCtor where
    showTCM (TC (Ident name) t args) = do
        s <- mapM showTCM args
        s' <- showTCM t
        return $ "(" ++ showStrings s ++ ") => " ++ s'

showStrings [] = ""
showStrings (x:xs) = x ++ showRemainingStrings xs
  where
    showRemainingStrings [] = ""
    showRemainingStrings (x:xs) = ", " ++ x ++ showRemainingStrings xs

instance (ShowTCM a) => ShowTCM (Scheme a) where
    showTCM (Forall tvs t) = do
        boundedVars <- mapM showTCM tvs
        s <- showTCM t
        return $ "Forall " ++ showStrings boundedVars ++ ". " ++ s
