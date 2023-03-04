{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Trees where 

import GHC.Types (Constraint)
import Data.Void

data Exp = Lit Integer 
         | Var Var
         | Ann Exp Typ
         | Abs Var Exp
         | App Exp Exp

type Var = String 

data Typ = Int
         | Fun Typ Typ

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


type TCExp = XExp TC

data TC

type instance XXLit TC = Void
type instance XXVar TC = Void
type instance XXAnn TC = Void
type instance XXAbs TC = Void
type instance XXApp TC = Typ
type instance XXExp TC = Void

pattern TCApp :: Typ -> TCExp -> TCExp -> TCExp
pattern TCApp a l m = XApp a l m
