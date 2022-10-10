module SimpleCore where

type Name = String

-- Type synonyms for different kinds of name
type Var = Name
type FName = Name
type CName = Name
type CafName = Name

data Expr = Simple SExpr
           | Let Binding Expr
           | LetS Binding Expr
           | Case SExpr [Alt]
           | If PrimCmp Expr Expr
           | Fix FName [Var]
           | Try FName [Var] Var
           | Throw Var
  deriving Show


data SExpr = SVar Var
           | Int Int
           | CAp CName [Var]
           | FAp FName [Var]
           | VAp Var   [Var]
           | CafAp CafName [Var]
           | POp PrimOp [Var]
           | Proj CName Int Var
  deriving Show

data Binding = Binding Name SExpr
  deriving Show

data Alt = Alt CName [Var] Expr
  deriving Show

data PrimOp = Plus
            | Sub
            | LEq
            -- TODO No exhaustive list given in paper...
  deriving Show

data PrimCmp = IntEQ Var Var
             | IntLT Var Var
             | IntGT Var Var
             | IntLTE Var Var
  deriving Show

data TExpr = Fun FName   [Var] Expr
           | Caf CafName       Expr
  deriving Show

fname :: TExpr -> Name
fname (Fun n _ _) = n
fname (Caf n   _) = n

arity :: TExpr -> Int
arity (Fun _ args _) = length args
arity (Caf _      _) = 0
