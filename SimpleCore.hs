module SimpleCore where

type Name = String

-- We distinguish between "reference" variables and unboxed primitive variables directly.
-- PilGRIM gets this from GHC's type info.

newtype RefVar = RefVar Name
  deriving Show
newtype PrimVar = PrimVar Name
  deriving Show
type Var = Either RefVar PrimVar

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


data SExpr = SVar Var
           | Int Int
           | CAp CName [Var]
           | FAp FName [Var]
           | VAp RefVar [Var]
           | CafAp CafName [Var] -- Why args here? For function-valued CAFs?
           | POp PrimOp [PrimVar]
           | Proj CName Int RefVar

data Binding = Binding Name SExpr

data Alt = Alt CName [Var] Expr

data PrimOp = Plus
            | Sub
            | LEq
            -- No exhaustive list given

data Cmp = CmpInt Expr Expr

data TExpr = Fun   FName [Var] Expr
           | Caf CafName       Expr

arity :: FName -> Int
arity = snd

name :: FName -> Name
name = fst
