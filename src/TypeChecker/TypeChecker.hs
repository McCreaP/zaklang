{-# LANGUAGE LambdaCase #-}
module TypeChecker.TypeChecker where
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative
import qualified Data.Set as Set
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map

import AbsZaklang
import PrintZaklang

import TypeChecker.Data
import TypeChecker.FreeTypeVariables
import TypeChecker.Show
import TypeChecker.Monad


tyBool = TyCons "_bool" []
tyInt = TyCons "_int" []
tyUnit = TyCons "_unit" []
tyFun a b = TyCons "=>" [a, b]
tyProd types = TyCons "*" types

unify :: TypeInfer -> TypeInfer -> TCM ()
unify (TyVar v) t = bind v t
unify t (TyVar v) = bind v t
unify t@(TyCons name args) t'@(TyCons name' args')
    | name == name' && length args == length args' = zipWithM_ unify args args'
    | otherwise = do
        s <- showTCM t
        s' <- showTCM t'
        throwError $ "Type: " ++ s ++ " does not match with type: " ++ s'

bind :: TyVar -> TypeInfer -> TCM ()
bind v@(TV name ref) t = liftM2 (==) (normalize $ TyVar v) (normalize t) >>= \case
    True -> return ()
    False -> readTypeRef v >>= \case
        Just t' -> unify t t'
        Nothing -> checkOccurences v t >>= \case
            True -> do
                normalizedType <- (normalize $ TyVar v)
                normalizedType' <- (normalize t)
                throwError $ "Cannot unify: " ++ show normalizedType ++ " and: " ++ show normalizedType' ++
                    ". Cyclic type dependency"
            False -> liftIO $ writeIORef ref (Just t)

checkOccurences v t = ftv t >>= return . Set.member v

normalize :: TypeInfer -> TCM TypeInfer
normalize t@(TyVar v) = readTypeRef v >>= \case
    Nothing -> return t
    Just t' -> normalize t'
normalize t = return t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: TCM TypeInfer
fresh = do
    ref <- liftIO $ newIORef Nothing
    env <- get
    put env{typeVarCount = typeVarCount env + 1}
    return $ TyVar $ TV (letters !! typeVarCount env) ref

ftvEnv :: TCM (Set.Set TyVar)
ftvEnv = gets types >>= ftv . Map.elems

--generalize :: TypeInfer -> TCM (Scheme a)
generalize t = do
    freeTypeVar <- ftv t
    freeEnvTypeVar <- ftvEnv
    let tvs = Set.toList $ freeTypeVar `Set.difference` freeEnvTypeVar
    return $ Forall tvs t

insertType :: Ident -> (Scheme TypeInfer) -> TCM ()
insertType ident scheme = do
    env <- get
    put env{types = Map.insert ident scheme (types env),
            ctors = Map.delete ident (ctors env)}

insertCtor :: Ident -> Scheme TyCtor -> TCM ()
insertCtor ident ctorScheme@(Forall tvs ctor) = do
    env <- get
    put env{ctors = Map.insert ident ctorScheme (ctors env)}
    env' <- get
    case (constructedType ctor) of
        TyCons constructedTypeName _
          | ident == Ident constructedTypeName -> return ()
          | otherwise -> put env'{types = Map.delete ident (types env)}
        _ -> put env'{types = Map.delete ident (types env)}

local computation = do
    currentEnv <- get
    result <- computation
    envAfterComputation <- get
    put currentEnv{typeVarCount = typeVarCount envAfterComputation}
    return result

inferProg :: Program -> TCM ()
inferProg (Prog declarations) = mapM_ inferDeclWrapper declarations

inferDeclWrapper decl = inferDecl decl `catchError` handler
    where handler s = throwError $ "In declaration: " ++ printTree decl ++ "\n" ++ s

inferDecl :: Decl -> TCM ()
inferDecl (DFun ident paramsHead paramsTail exp) =
    inferFuncType >>= generalize >>= insertType ident
      where
        inferFuncType = local $ do
            recFuncType <- fresh
            insertType ident (Forall [] recFuncType)
            funcType <- inferExpWrapper (EAbs paramsHead paramsTail exp)
            unify funcType recFuncType
            return funcType

inferDecl (DVal ident exp) = inferExpWrapper exp >>= generalize >>= insertType ident
inferDecl (DType definedType ctors) = do
    (constructedType, polyArgsMap) <- inferType definedType
    mapM_ (inferCtor constructedType polyArgsMap) ctors
      where
        inferCtor constructedType polyArgsMap (Ctor ident types) =
            mapM (flip inferTypeInCtor polyArgsMap) types >>=
            generalize . TC ident constructedType >>=
            insertCtor ident

inferType :: Type -> TCM (TypeInfer, Map.Map Ident TypeInfer)
inferType definedType = case definedType of
    Type ident@(Ident name) -> do
        let t = TyCons name []
        insertType ident (Forall [] t)
        return (t, Map.empty)
    TypeCtor ident@(Ident name) types -> do
        idents <- typesToIdents types
        freshVars <- mapM (const fresh) idents
        let t = TyCons name freshVars
        let polyArgsMap = Map.fromList (zip idents freshVars)
        scheme <- generalize t
        insertType ident scheme
        return (t, polyArgsMap)
      where
        typesToIdents :: [Type] -> TCM [Ident]
        typesToIdents [] = return []
        typesToIdents (t:types) = case t of
            Type ident -> typesToIdents types >>= return . (ident:)
            _ -> throwError $ "Wrong polymorphic argument: " ++ show t ++
                ". Only identifiers are allowed."

inferTypeInCtor :: Type -> Map.Map Ident TypeInfer -> TCM TypeInfer
inferTypeInCtor (Type (Ident "int")) _ = return tyInt
inferTypeInCtor (Type (Ident "bool")) _ = return tyBool
inferTypeInCtor (Type (Ident "unit")) _ = return $ tyProd []
inferTypeInCtor (Type ident) polyArgsMap = case Map.lookup ident polyArgsMap of
    Just v -> return v
    Nothing -> gets (Map.lookup ident . types) >>= \case
        Just (Forall [] t) -> return t
        _ -> throwError $ "Unknown type: " ++ show ident

inferTypeInCtor typeCtor@(TypeCtor ident polyTypes) polyArgsMap =
    gets (Map.lookup ident . types) >>= \case
        Just scheme -> freshInstance scheme >>= \case
            t@(TyCons ident' polyTypes') -> mapM (flip inferTypeInCtor polyArgsMap) polyTypes >>=
                zipWithM unify polyTypes' >> return t
        Nothing -> throwError $ "Unknown type: " ++ show typeCtor

inferExpWrapper exp = inferExp exp `catchError` handler
    where handler s = throwError $ "In expression: " ++ printTree exp ++ "\n" ++ s

inferExp :: Exp -> TCM TypeInfer
inferExp (EIfte condition exp exp') = do
    conditionType <- inferExp condition
    unify tyBool conditionType
    t <- inferExp exp
    t' <- inferExp exp'
    unify t t'
    return t

inferExp (EOr exp exp') = inferBoolBinOp exp exp'
inferExp (EAnd exp exp') = inferBoolBinOp exp exp'
inferExp (ENot exp) = inferExp exp >>= unify tyBool >> return tyBool

inferExp (ELt exp exp') = inferIntCmpOp exp exp'
inferExp (EEq exp exp') = inferIntCmpOp exp exp'

inferExp (EAdd exp exp') = inferIntBinOp exp exp'
inferExp (ESub exp exp') = inferIntBinOp exp exp'
inferExp (EMul exp exp') = inferIntBinOp exp exp'
inferExp (EDiv exp exp') = inferIntBinOp exp exp'
inferExp (EPow exp exp') = inferIntBinOp exp exp'
inferExp (EInt _) = return tyInt
inferExp (ETrue) = return tyBool
inferExp (EFalse) = return tyBool

inferExp (EApp ident args) = gets (Map.lookup ident . ctors) >>= \case
    Just scheme -> prune scheme >>= freshConstructor >>= inferCtor args
    Nothing -> gets (Map.lookup ident . types) >>= \case
        Just scheme -> prune scheme >>= freshInstance >>= inferApp args
        Nothing -> throwError $ "Name: " ++ show ident ++ " not in the scope"

inferExp (EAbs paramsHead@(Params params) paramsTail exp) = do
    freshVariables <- mapM (const fresh) params
    local $ do
        zipWithM_ bindParamType params freshVariables
        returnType <- if null paramsTail
            then inferExp exp
            else inferExp (EAbs (head paramsTail) (tail paramsTail) exp)
        return $ tyFun (tyProd freshVariables) returnType
      where
        bindParamType :: Ident -> TypeInfer -> TCM ()
        bindParamType ident t = insertType ident (Forall [] t)

inferExp (EBlock declarations exp) = local $ do
    mapM_ inferDecl declarations
    blockType <- inferExp exp
    return blockType

inferExp (EMatch exp cases) = do
    patternType <- inferExp exp
    resultType <- fresh
    mapM_ (inferCase patternType resultType) cases
    return resultType

inferCase patternType resultType (Case exp exp') = local $ do
    t <- inferPattern exp
    t' <- inferExp exp'
    unify patternType t
    unify resultType t'

inferPattern :: Exp -> TCM TypeInfer
inferPattern (EApp ident []) = do
        freshVar <- fresh
        insertType ident (Forall [] freshVar)
        return freshVar

inferPattern (EApp ident [Args exps]) = gets (Map.lookup ident . ctors) >>= \case
    Just scheme@(Forall tvs ctor)
        | length (args ctor) == length exps -> do
            freshCtor <- freshConstructor scheme
            zipWithM_ matchPattern (args freshCtor) exps
            return $ constructedType freshCtor
        | otherwise -> throwError $ "Wrong number of arguments in pattern: " ++ show ident ++
            ", expected: " ++ show (length $ args ctor) ++
            ", got: " ++ show (length exps)
    Nothing -> throwError $ show ident ++ " is not a constructor"

inferPattern exp = inferExp exp

matchPattern :: TypeInfer -> Exp -> TCM ()
matchPattern t (EApp ident []) = do
        freshVar <- fresh
        unify t freshVar
        insertType ident (Forall [] freshVar)

matchPattern t (EApp ident [Args exps]) = gets (Map.lookup ident . ctors) >>= \case
    Just scheme@(Forall tvs ctor)
        | length (args ctor) == length exps -> do
            freshCtor <- freshConstructor scheme
            unify t (constructedType freshCtor)
            zipWithM_ matchPattern (args freshCtor) exps
        | otherwise -> throwError $ "Wrong number of arguments in pattern: " ++ show ident ++
            ", expected: " ++ show (length $ args ctor) ++
            ", got: " ++ show (length exps)
    Nothing -> throwError $ show ident ++ " is not a constructor"

matchPattern t exp = inferExp exp >>= unify t

--prune :: Scheme TypeInfer -> TCM (Scheme TypeInfer)
prune (Forall tvs ts) = do
    freeTypeVars <- ftv tvs
    return $ Forall (Set.toList freeTypeVars) ts

freshInstance :: Scheme TypeInfer -> TCM TypeInfer
freshInstance (Forall tvs ts) = do
    freshVars <- mapM (const fresh) tvs
    substitute (zip tvs freshVars) ts

freshConstructor :: Scheme TyCtor -> TCM TyCtor
freshConstructor (Forall tvs (TC ident constructedType args)) = do
    freshVars <- mapM (const fresh) tvs
    let bindings = zip tvs freshVars
    freshConstructedType <- substitute bindings constructedType
    freshArgs <- mapM (substitute bindings) args
    return $ TC ident freshConstructedType freshArgs

substitute :: [(TyVar, TypeInfer)] -> TypeInfer -> TCM TypeInfer
substitute bindings t@(TyVar v) = readTypeRef v >>= \case
    Just t' -> substitute bindings t'
    Nothing -> case Map.lookup v (Map.fromList bindings) of
        Nothing -> return t
        Just t' -> return t'
substitute bindings (TyCons name types) = TyCons name <$> (mapM (substitute bindings) types)

inferApp :: [Arguments] -> TypeInfer -> TCM TypeInfer
inferApp [] functionType = return functionType
inferApp ((Args exps):argsTail) functionType = do
    argTypes <- mapM inferExp exps
    returnType <- fresh
    unify (tyFun (tyProd argTypes) returnType) functionType
    inferApp argsTail returnType

inferCtor :: [Arguments] -> TyCtor -> TCM TypeInfer
inferCtor [Args exps] ctor
    | length exps == length (args ctor) = do
        argTypes <- mapM inferExp exps
        zipWithM_ unify argTypes (args ctor)
        return $ constructedType ctor
    | otherwise = throwError $ "Wrong number of arguments in constructor: " ++ show (ident ctor) ++
        ", expected: " ++ show (length $ args ctor) ++
        ", got: " ++ show (length exps)
inferCtor args ctor = throwError $ "Constructor: " ++ show (ident ctor) ++
    "supports exactly one application"

inferBoolBinOp :: Exp -> Exp -> TCM TypeInfer
inferBoolBinOp exp exp' = do
    t <- inferExp exp
    unify tyBool t
    t' <- inferExp exp'
    unify tyBool t'
    return tyBool

inferIntCmpOp :: Exp -> Exp -> TCM TypeInfer
inferIntCmpOp exp exp' = inferInt exp exp' >> return tyBool

inferIntBinOp :: Exp -> Exp -> TCM TypeInfer
inferIntBinOp exp exp' = inferInt exp exp' >> return tyInt

inferInt exp exp' = do
    t <- inferExp exp
    unify tyInt t
    t' <- inferExp exp'
    unify tyInt t'
