{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Trees where 

import GHC.Types (Constraint)

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
            | XAnn (XXAnn x) Exp Typ
            | XAbs (XXAbs x) Var Exp
            | XApp (XXApp x) Exp Exp
            | XExp (XXExp x)
