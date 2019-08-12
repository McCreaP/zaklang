{-# LANGUAGE LambdaCase #-}
module Interpreter.Interpreter where
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map

import PrintZaklang
import AbsZaklang

import Interpreter.Data
import Interpreter.Monad
import Interpreter.Util

denoteProgram :: Program -> IM Int
denoteProgram (Prog declarations) = do
    mapM_ denoteDeclWrapper declarations
    value <- denoteExp $ EApp (Ident "main") [Args []]
    case value of
        VInt x -> return x
        _ -> throwError $ "Program should be evaluated to int, instead " ++ show value

denoteDeclWrapper decl = denoteDecl decl `catchError` handler
    where handler s = throwError $ "In declaration: " ++ printTree decl ++ "\n" ++ s

denoteDecl :: Decl -> IM ()
denoteDecl (DVal ident exp) = denoteExpWrapper exp >>= modify . Map.insert ident
denoteDecl (DFun ident paramsHead paramsTail exp) =
    denoteAbs (paramsHead:paramsTail) exp ident >>= modify . Map.insert ident
denoteDecl (DType _ constructors) = mapM_ denoteCtor constructors
    where denoteCtor (Ctor ident _) = modify $ Map.insert ident (VCtor ident)

denoteExpWrapper exp = denoteExp exp `catchError` handler
    where handler s = throwError $ "In expression: " ++ printTree exp ++ "\n" ++ s

denoteExp :: Exp -> IM Value
denoteExp ETrue = return $ VBool True
denoteExp EFalse = return $ VBool False
denoteExp (EOr exp exp') = denoteBoolBinOp exp exp' (||)
denoteExp (EAnd exp exp') = denoteBoolBinOp exp exp' (&&)
denoteExp (ENot exp) = do
    VBool b <- denoteExp exp
    return $ VBool (not b)
denoteExp (ELt exp exp') = denoteIntCmpOp exp exp' (<)
denoteExp (EEq exp exp') = denoteIntCmpOp exp exp' (==)
denoteExp (EAdd exp exp') = denoteIntBinOp exp exp' (+)
denoteExp (ESub exp exp') = denoteIntBinOp exp exp' (-)
denoteExp (EMul exp exp') = denoteIntBinOp exp exp' (*)
denoteExp (EDiv exp exp') = denoteIntBinOp exp exp' div
denoteExp (EPow exp exp') = denoteIntBinOp exp exp' (^)
denoteExp (EInt (TInt s)) = return $ VInt (read s)

denoteExp (EIfte condition exp exp') = do
    VBool b <- denoteExp condition
    if b then denoteExp exp else denoteExp exp'

denoteExp (EBlock declarations exp) = local $ mapM_ denoteDecl declarations >> denoteExp exp
denoteExp (EAbs paramsHead paramsTail exp) = denoteAbs (paramsHead:paramsTail) exp (Ident "")
denoteExp (EApp ident args) = gets (Map.lookup ident) >>= \case
    Just value -> denoteApp args value
    Nothing -> throwError $ show ident ++ " not in the scope"

denoteExp (EMatch exp cases) = do
    value <- denoteExp exp
    (Case exp exp') <- value `matchWrapper` cases
    local $ bindCase value exp >> denoteExp exp'
      where
        bindCase :: Value -> Exp -> IM ()
        bindCase value@(VCons _ values) (EApp ident [Args exps]) =
            gets (Map.lookup ident) >>= \case
                Just (VCtor _) -> zipWithM_ bindCase values exps
                _ -> modify $ Map.insert ident value
        bindCase value (EApp ident []) = modify $ Map.insert ident value
        bindCase _ _ = return ()

matchWrapper value cases = match value cases `catchError` handler
    where handler s = throwError $ "In pattern matching: " ++ printTree cases ++ "\n" ++ s

match :: Value -> [Case] -> IM Case
match value [] = throwError $ "Could not match value: " ++ show value ++
    ". Non-exhaustive patterns in matching"
match value (c@(Case exp _):cs) = value `matchWith` exp >>= \case
    True -> return c
    False -> match value cs

matchWith :: Value -> Exp -> IM Bool
matchWith (VCons ident values) (EApp ident' [Args exps]) =
    gets (Map.lookup ident') >>= \case
        Just (VCtor _)
            | ident == ident' -> do
                matches <- zipWithM matchWith values exps
                return $ all id matches
            | otherwise -> return False
        _ -> return True
matchWith value (EApp ident []) =
    gets (Map.lookup ident) >>= \case
        Just (VCtor _) -> return False
        _ -> return True
matchWith value exp = denoteExp exp >>= return . (value==)

denoteAbs :: [Parameters] -> Exp -> Ident -> IM Value
denoteAbs params exp ident =
    get >>= return . VClosure params [] exp ident

denoteApp :: [Arguments] -> Value -> IM Value
denoteApp args functionValue = do
    values <- evalArgs args
    case functionValue of
        VClosure params bindedValues exp ident env
            | length params == (length args) + (length bindedValues) -> local $ do
                put env
                if isNamedFunction ident
                    then modify $ Map.insert ident (VClosure params [] exp ident env)
                    else return ()
                callFunction params (bindedValues ++ values) exp
            | length params > (length args) + (length bindedValues) ->
                return $ VClosure params (bindedValues ++ values) exp ident env
            | length params < (length args) + (length bindedValues) ->
                denoteApp (take (length params) args) functionValue >>=
                denoteApp (drop (length params) args)
        VCtor ident -> return $ VCons ident (concat values)
        value -> return value
      where
        evalArgs args = mapM evalArg args
            where evalArg (Args exps) = mapM denoteExp exps
        bindParams (Params idents) values = zipWithM_ bindParam idents values
            where bindParam ident value = modify $ Map.insert ident value
        callFunction params values exp =
            zipWithM_ bindParams params values >> denoteExp exp

denoteBoolBinOp :: Exp -> Exp -> (Bool -> Bool -> Bool) -> IM Value
denoteBoolBinOp exp exp' op = do
    VBool b1 <- denoteExp exp
    VBool b2 <- denoteExp exp'
    return $ VBool (b1 `op` b2)

denoteIntBinOp :: Exp -> Exp -> (Int -> Int -> Int) -> IM Value
denoteIntBinOp exp exp' op = VInt <$> denoteBinOp exp exp' op

denoteIntCmpOp :: Exp -> Exp -> (Int -> Int -> Bool) -> IM Value
denoteIntCmpOp exp exp' op = VBool <$> denoteBinOp exp exp' op

denoteBinOp exp exp' op = do
    VInt n1 <- denoteExp exp
    VInt n2 <- denoteExp exp'
    return $ n1 `op` n2
