{-# LANGUAGE GADTs #-}
module Translate where

import InstrSet
import SimpleCore

import Data.Maybe (catMaybes, fromMaybe)

type PartialBlock a = (a -> Block) -> Block

cont :: Show a => Instr a -> PartialBlock a
cont = (:>)

iseq :: (Show a, Show b) => Instr a -> (a -> Instr b) -> PartialBlock b
iseq a f = \rest -> a :> \h -> f h :> rest

type ArityMap = [(FName, Int)]

getArity :: ArityMap -> Var -> Int
getArity m f = fromMaybe (error $ "Looked up a function missing from context: " ++ f)
                         (lookup f m)

toArityMap :: [TExpr] -> ArityMap
toArityMap = map (\f -> (fname f, arity f))

translate :: [TExpr] -> [(Name, Func)]
translate prog = map (topTrans (toArityMap prog)) prog

topTrans :: ArityMap -> TExpr -> (Name, Func)
topTrans am (Fun f args body) = (f, IFun args $ tTrans am body)
topTrans am (Caf g      body) = (g, ICaf      $ tTrans am body)

tTrans :: ArityMap -> Expr -> Block
tTrans am (Simple (CAp con args))
  = Terminate $ Return (CNode con args)
tTrans am (Simple (FAp f args))
  | length args < arity = Terminate $ Return (PartialF f (arity - length args) args)
  | otherwise                   = Terminate $ uncurry Jump $ eTrans am (FAp f args)
  where arity = getArity am f
tTrans am (Simple s)
  = Terminate $ uncurry Jump $ eTrans am s
tTrans am (Let  (Binding x s) e)
  = vTrans am x s (\v -> tTrans am e)
tTrans am (LetS (Binding x s) e)
  = sTrans am x s (\v -> tTrans am e)
tTrans am (Case s alts) -- TODO implement unambiguous scrutinee optimisation
  = Terminate $ uncurry ICase (eTrans am s) (map (altTrans am) alts)
tTrans am (If cmp x y)
  = Terminate $ IIf cmp (tTrans am x) (tTrans am y)
tTrans am (Fix f args)
  = Terminate $ Jump (IFix f args) NOp
tTrans am (Try f args h)
  = Terminate $ Jump (TLF f args) (ICatch h)
tTrans am (Throw x)
  = Terminate $ IThrow x

altTrans :: ArityMap -> Alt -> IAlt
altTrans am (Alt cn args e) = IAlt (CNode cn args) (tTrans am e)

-- Translate a lazy subexpression -- TODO What do we do with the x?
vTrans :: ArityMap -> Var -> SExpr -> PartialBlock Var
vTrans am x (CafAp n args)
  | length args == 0 = cont $ PushCaf n
  | otherwise        = PushCaf n `iseq` \h ->
                       Store (FNode "ap" (h:args))
vTrans am x (FAp n args)
  | arity >  length args = cont $ Store (PartialF n (arity - length args) args)
  | arity == length args = cont $ Store (FNode n args)
  | otherwise            = Store (FNode n (take arity args)) `iseq` \h ->
                           Store (FNode h (h : drop arity args))
  where arity = getArity am n
vTrans am x (VAp f args) = cont $ Store (FNode "ap" (f:args))
vTrans am x (Proj n field var)  -- TODO How do we interpret the "FSel_n" notation? Is that the name of a predefined function?
  = cont $ Store (FNode n' [var])
  where n' = "sel_" ++ n
vTrans am x e = undefined

-- Translate a strict subexpression
sTrans :: ArityMap -> Var -> SExpr -> PartialBlock Var
sTrans am x (Int n) = cont $ Constant n
sTrans am x (POp n [a,b]) = cont $ IPrimOp n a b
sTrans am x (CAp n args) = cont $ Store (CNode n args)
sTrans am x (VAp n args) = cont $ uncurry Force (eTrans am $ VAp n args)
sTrans am x (Proj n field e) = cont $ uncurry Force (eTrans am $ Proj n field e)
sTrans am x (FAp f args)
  | arity <= length args = cont $ uncurry Force (eTrans am $ FAp f args)
  | otherwise            = cont $ Store (PartialF f (arity - length args) args)
  where arity = getArity am f
sTrans am _ x = cont $ uncurry Force (eTrans am x)

-- Translate evaluation expressions
eTrans :: ArityMap -> SExpr -> (Call, Cont)
eTrans am (SVar x) = (Eval x, NOp)
eTrans am (Proj _ field x) = (Eval x, Select field)
eTrans am (CafAp n args)
  | length args == 0 = (EvalCaf n, NOp)
  | otherwise        = (EvalCaf n, Apply args)
eTrans am (FAp f args)
  | getArity am f == length args = (TLF f args             , NOp)
  | otherwise                    = (TLF f (take arity args), Apply (drop arity args))
  where arity = getArity am f
eTrans am (VAp n args) = (Eval n, Apply args)
