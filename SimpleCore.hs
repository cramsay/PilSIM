module SimpleCore where

type Name = String

-- We distinguish between "reference" variables and unboxed primitive variables directly.
-- PilGRIM gets this from GHC's type info.

type RefVar = Name
type PrimVar = Name
type Var = Name

type FName = (Name, Int)
type CName = Name
type CafName = Name

data Expr = Simple SExpr
           | Let Binding Expr
           | LetS Binding Expr
           | Case SExpr [Alt]
           | If Cmp Expr Expr
           -- | Fix
           -- | Try
           -- | Throw
  deriving Show


data SExpr = SVar Var
           | Int Int
           | CAp CName [Var]
           | FAp FName [Var]
           | VAp RefVar [Var]
           | CafAp CafName [Var] -- Why args here? For function-valued CAFs?
           | POp PrimOp [PrimVar]
           | Proj CName Int RefVar
  deriving Show

data Binding = Binding Name SExpr
  deriving Show

data Alt = Alt CName [Var] Expr
  deriving Show

data PrimOp = Plus
            | Sub
            | LEq
            -- No exhaustive list given
  deriving Show

data Cmp = CmpInt Expr Expr
  deriving Show

data TExpr = Fun   FName [Var] Expr
           | Caf CafName       Expr
  deriving Show

arity :: FName -> Int
arity = snd

name :: FName -> Name
name = fst
