{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Trees where 

import GHC.Types (Constraint)
import Data.Void
import Data.Kind

data Exp = Lit Integer 
         | Var Var
         | Ann Exp Typ
         | Abs Var Exp
         | App Exp Exp

type Var = String 

data Typ = Int
         | Fun Typ Typ
         deriving Eq

data Val = VInt Int | VString String deriving Show

type family XXLit x
type family XXVar x
type family XXAnn x
type family XXAbs x
type family XXApp x
type family XXExp x

data XExp x = XLit (XXLit x) Integer 
            | XVar (XXVar x) Var
            | XAnn (XXAnn x) (XExp x) Typ
            | XAbs (XXAbs x) Var (XExp x)
            | XApp (XXApp x) (XExp x) (XExp x)
            | XExp (XXExp x)

-- UD - undecorated

type instance XXLit UD = Void
type instance XXVar UD = Void
type instance XXAnn UD = Void
type instance XXAbs UD = Void
type instance XXApp UD = Void
type instance XXExp UD = Void

data UD

type UDExp = XExp UD

void :: Void
void = error "void"

incLit :: Exp -> Exp
incLit (Lit i) = Lit (i + 1)
incLit e = e

-- incLitX :: UDExp -> UDExp
-- incLitX (XLit _ i) = XLit void (i + 1)
-- incLitX e = e

pattern UDLit :: Integer -> UDExp
pattern UDLit i <- XLit _ i where UDLit i = XLit void i

incLitX :: UDExp -> UDExp
incLitX (UDLit i) = UDLit (i + 1)
incLitX e = e

-- Field Ext

-- data XExp x = XLit (XXLit x) Integer 
--             | XVar (XXVar x) Var
--             | XAnn (XXAnn x) (XExp x) Typ
--             | XAbs (XXAbs x) Var (XExp x)
--             | XApp (XXApp x) (XExp x) (XExp x)
--             | XExp (XXExp x)

type TCExp = XExp TC

data TC

type instance XXLit TC = Void
type instance XXVar TC = Void
type instance XXAnn TC = Void
type instance XXAbs TC = Void
type instance XXApp TC = Typ
type instance XXExp TC = Void

pattern TCLit :: Integer -> TCExp
pattern TCLit i <- XLit _ i where TCLit i = XLit void i

pattern TCVar :: Var -> TCExp
pattern TCVar v <- XVar _ v

pattern TCAnn :: TCExp -> Typ -> TCExp
pattern TCAnn m a <- XAnn _ m a

pattern TCAbs :: Var -> TCExp -> TCExp
pattern TCAbs v e <- XAbs _ v e

pattern TCApp :: Typ -> TCExp -> TCExp -> TCExp
pattern TCApp t e1 e2 = XApp t e1 e2

check :: TCExp -> [(Var, Typ)] -> Typ -> Bool
check (TCLit _)     _   Int       = True
check (TCVar x)     env c         = maybe False (== c) (lookup x env)
check (TCAnn m a)   env c         = (a == c) || check m env c
check (TCAbs x n)   env (Fun a b) = check n ((x, a) : env) b
check (TCApp a l m) env c         = check l env (Fun a c) || check m env a
check _             _   _         = False

-- Constructor Ext

-- (?) Can there be multiple constructor extensions for a single type discriminator

type PEExp = XExp PE
data PE

type instance XXLit PE = Void
type instance XXVar PE = Void
type instance XXAnn PE = Void
type instance XXAbs PE = Void
type instance XXApp PE = Void
type instance XXExp PE = Val

pattern PEVal :: Val -> PEExp
pattern PEVal v = XExp v

-- Generic Functions

printT :: Typ -> String 
printT Int = "Int"
printT (Fun a b) = "(" ++ printT a ++ ") -> " ++ printT b

printE :: (XXExp e -> String) -> XExp e -> String
printE _ (XLit _ i) = show i
printE _ (XVar _ x) = x
printE p (XAnn _ m a) = "(" ++ printE p m ++ ")::(" ++ printT a ++ ")"
printE p (XAbs _ x n) = "\\" ++ x ++ "." ++ printE p n
printE p (XApp _ l m) = "(" ++ printE p l ++ ") (" ++ printE p m ++ ")"
printE p (XExp e) = p e

printUDE :: UDExp -> String
printUDE = printE absurd

printTCE :: TCExp -> String
printTCE = printE absurd

printPEE :: PEExp -> String
printPEE = printE p
  where 
    p v = "{{" ++ show v ++ "}}"

type XForall (a :: Type -> Constraint) e = 
  ( a (XXLit e)
  , a (XXVar e)
  , a (XXAnn e)
  , a (XXAbs e)
  , a (XXApp e)
  , a (XXExp e)
  )

instance XForall Show e => Show (XExp e) where 
  -- show :: XForall Show e => XExp e -> String
  show = undefined
  
-- 'Modifying' Constructor

type SAExp = XExp SA
data SA

type instance XXLit SA = Void
type instance XXVar SA = Void
type instance XXAnn SA = Void
type instance XXAbs SA = Void
type instance XXApp SA = Void
type instance XXExp SA = (SAExp, [SAExp])

-- the provided SAApp doesn't actually use XApp, so it's effectively 'overridden'
pattern SAApp :: SAExp -> [SAExp] -> SAExp
pattern SAApp l ms = XExp (l, ms)

---------------------
-- Type Parameter Ext
---------------------

type family XXALit x a
type family XXAVar x a
type family XXAAnn x a
type family XXAAbs x a
type family XXAApp x a
type family XXAExp x a

data XAExp x a = XALit (XXALit x a) Integer 
               | XAVar (XXAVar x a) Var
               | XAAnn (XXAAnn x a) (XAExp x a) Typ
               | XAAbs (XXAAbs x a) Var         (XAExp x a)
               | XAApp (XXAApp x a) (XAExp x a) (XAExp x a)
               | XAExp (XXAExp x a)

type LEExp a = XAExp LE a
data LE

type instance XXALit LE a = Void
type instance XXAVar LE a = Void
type instance XXAAnn LE a = Void
type instance XXAAbs LE a = Void
type instance XXAApp LE a = Void
type instance XXAExp LE a = (a, LEExp a, LEExp a)

pattern LELet :: a -> LEExp a -> LEExp a -> LEExp a
pattern LELet x m n = XAExp (x, m, n)

-------------------------
-- Existentials and GADTs
-------------------------

data ExpGADT a where 
  Con :: c -> ExpGADT c
  AppGADT :: (a -> String) -> ExpGADT (a -> b) -> ExpGADT a -> ExpGADT b
  AddGADT :: ExpGADT (Int -> Int -> Int)
  AndGADT :: ExpGADT (Bool -> Bool -> Bool)

data XGExp x a where 
  XGCon :: XXGCon x c   -> c                -> XGExp x c
  XGApp :: XXGApp x a b -> XGExp x (a -> b) -> XGExp x a -> XGExp x b
  XGAdd :: XXGAdd x     -> XGExp x (Int -> Int -> Int)
  XGAnd :: XXGAnd x     -> XGExp x (Bool -> Bool -> Bool) 
  XGExp :: XXGExp x a   -> XGExp x a

type family XXGCon x c
type family XXGApp x a b
type family XXGAdd x
type family XXGAnd x
type family XXGExp x a

