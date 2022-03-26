{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Data.List
import Data.Char
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe

infixl 4 :@
infixr 3 :->

type Symb = String

-- Терм
data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

-- Тип
data Type = TVar Symb
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)

freeVars :: Expr -> [Symb]
freeVars (Var a) = [a]
freeVars (a :@ b) = (freeVars a) `union` (freeVars b)
freeVars (Lam a e) = (freeVars e) \\ [a]

freeTVars :: Type -> [Symb]
freeTVars (TVar a) = [a]
freeTVars (a :-> b) = (freeTVars a) `union` (freeTVars b)

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) s t = Env $ (s, t) : env

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env []) = []
freeTVarsEnv (Env env) =
  (freeTVars $ snd $ head env) `union` (freeTVarsEnv $ Env $ tail env)

appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env []) v = throwError ("There is no variable \"" ++ v ++ "\" in the enviroment.")
appEnv (Env (x:xs)) v = if fst x == v
                        then return $ snd x
                        else appEnv (Env xs) v

appSubsTy :: SubsTy -> Type -> Type
appSubsTy subs (a :-> b) = ((appSubsTy subs a) :-> (appSubsTy subs b))
appSubsTy (SubsTy []) t = t
appSubsTy (SubsTy (s:subs)) (TVar t) = if fst s == t
                                       then snd s
                                       else appSubsTy (SubsTy subs) (TVar t)

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv subs (Env env) = Env $ map (\x -> (fst x, appSubsTy subs (snd x))) env

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy (SubsTy s1) (SubsTy s2) =
    SubsTy $ fmap (\s -> (s, appSubsTy (SubsTy s1) $ appSubsTy (SubsTy s2) (TVar s))) subs
      where subs = fmap fst s1 `union` fmap fst s2

instance Semigroup SubsTy where
  (<>) = mappend

instance Monoid SubsTy where
  mempty = SubsTy []
  mappend  = composeSubsTy

canUnify :: Type -> Type -> Bool
canUnify (TVar t1) (TVar t2) = (t1 /= t2)
canUnify (TVar s) (t1 :-> t2) = (canUnify (TVar s) t1) && (canUnify (TVar s) t2)
canUnify (t1 :-> t2) (TVar s) = canUnify (TVar s) (t1 :-> t2)
canUnify (t1 :-> t2) (t3 :-> t4) = (canUnify t1 t3) && (canUnify t2 t4)

unify :: MonadError String m => Type -> Type -> m SubsTy
unify t1 t2
        | t1 == t2  = return $ SubsTy []
        | otherwise = step t1 t2
          where
            step (TVar s1) t2 = if s1 `elem` (freeTVars t2)
                                then throwError ("Can't unify (" ++ (show t1) ++ ") with (" ++ (show t2) ++ ")!")
                                else return $ SubsTy $ [(s1, t2)]
            step (t1' :-> t1'') (TVar s2) = unify t2 t1
            step (t1' :-> t1'') (t2' :-> t2'') = do
                u2 <- unify t1'' t2''
                u1 <- unify (appSubsTy u2 t1') (appSubsTy u2 t2')
                return $ composeSubsTy u1 u2

getType :: MonadError String m => Symb -> StateT [Symb] m Symb
getType s = do
    vars <- get
    let newT = fromJust $ find (\t -> not (t `elem` vars)) (generate s)
    put $ newT : vars
    return newT

generate s = map (\x -> s ++ show x) [0..]

equations :: MonadError String m => Env -> Expr -> Type -> m [(Type,Type)]
equations env e t = 
    evalStateT (equations' env e t) (freeTVars t `union` freeTVarsEnv env)
 
equations' :: MonadError String m => Env -> Expr -> Type -> StateT [Symb] m [(Type,Type)]
equations' env (Var x) t = do
    t' <- appEnv env x
    return [(t', t)]
equations' env (e :@ e') t = do
    a <- getType "a"
    eq1 <- equations' env e $ TVar a :-> t
    eq2 <- equations' env e' $ TVar a
    return $ eq1 `union` eq2
equations' env (Lam s e) t = do
    a <- getType "a"
    b <- getType "b"
    eq1 <- equations' (extendEnv env s (TVar a)) e $ TVar b
    let eq2 = [(TVar a :-> TVar b, t)]
    return $ eq1 `union` eq2

principlePair :: MonadError String m => Expr -> m (Env,Type)
principlePair e = do
    let types = map TVar (generate "c")
    let env = Env $ zip (freeVars e) types
    let t = TVar "d"
    eqs <- equations env e t
    sub <- unify (go $ map fst eqs) (go $ map snd eqs)
    return $ (appSubsEnv sub env, appSubsTy sub t)

go :: [Type] -> Type
go [t] = t
go (t:ts) = t :-> (go ts)
go [] = undefined
