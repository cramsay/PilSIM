module Trans where

import InstrSet
import SimpleCore

import Data.Maybe (catMaybes)

{-
tTrans :: Expr -> Block
tTrans (Simple (CAp con args)) = Block [] (Return (CTag con) (map argTrans args))
tTrans (Simple (FAp f args))
  | length args < arity f = Block [] (Return (PartialFTag f (arity f - length args)) (map argTrans args))
  | otherwise             = let (e',  cont) = eTrans (FAp f args)
                            in Block [] (uncurry Jump (eTrans (FAp f args)))
tTrans (Simple s) = Block [] (uncurry Jump (eTrans s))
tTrans (Let  (Binding x s) e) = consBlock (Store x $ vTrans s) (tTrans e)
tTrans (LetS (Binding x s) e) = consBlock (Store x $ sTrans s) (tTrans e)
tTrans (Case s alts) = case (catMaybes $ map matchAlt alts) of
                             -- [(con, args, e)]  -> ICall (CTag con) args e NOp
                             -- TODO implement unambiguous scrutinee optimisations
                             []                -> uncurry ICase (eTrans s) (map altTrans alts)
tTrans (If cmp x y) = IIf cmp (tTrans x) (tTrans y)

vTrans :: SExpr -> [Instr]
vTrans (CafAp n args)
  | length args == 0 = [PushCaf n n] -- TODO are we sure?
  | otherwise        = [PushCaf n' n, Store n' (FTag (n', -99)) (map argTrans args)]
      where n' = n ++ "'"
vTrans (FAp   n args)
  | arity n >  length args = [Store "TODO" (PartialFTag n (arity n - length args)) (map argTrans args)]
  | arity n == length args = [Store "TODO" (FTag n) (map argTrans args)]
  | otherwise              = (Store (fst h) (FTag n) (map argTrans $ take (arity n) args))
                             : vTrans (FAp h (drop (arity n) args))
     where h = let (f, arity) = n
               in (f++"'", arity)
vTrans (VAp   n args)  = [Store "TODO" (FTag (n,(-99))) (map argTrans args)]
vTrans (Proj  n field var) = [Store "TODO" (CTag $ "sel_" ++ n ++ show field) [argTrans var]] -- Is Store meant to take a continuation? Would make more sense... need to use Select here

sTrans :: SExpr -> Instr
sTrans (Int n) = Constant n
sTrans (POp pn [x,y]) = IPrimOp "TODO" pn x y
sTrans (CAp cn args) = Store "TODO" (CTag cn) (map argTrans args)
sTrans (VAp vn args) = uncurry (Force "TODO") (eTrans $ VAp vn args)
sTrans (Proj cn field vn) = uncurry (Force "TODO") (eTrans $ Proj cn field vn)
sTrans (FAp f args)
  | arity f <= length args = uncurry (Force "TODO") (eTrans $ FAp f args)
  | otherwise               = Store "TODO" (PartialFTag f (arity f - length args)) (map argTrans args)
sTrans x = uncurry (Force "TODO") (eTrans x)

matchAlt :: Alt -> Maybe ()
matchAlt _ = Nothing

altTrans :: Alt -> IAlt
altTrans (Alt cn args e) = IAlt (CTag cn) (map argTrans args) (tTrans e)
-}


-- Translate a strict subexpression
sTrans :: SExpr -> Instr a
sTrans (Int n) = Constant n
sTrans (POp n [x,y]) = IPrimOp n x y
{-
sTrans (CAp cn args) = Store "TODO" (CTag cn) (map argTrans args)
sTrans (VAp vn args) = uncurry (Force "TODO") (eTrans $ VAp vn args)
sTrans (Proj cn field vn) = uncurry (Force "TODO") (eTrans $ Proj cn field vn)
sTrans (FAp f args)
  | arity f <= length args = uncurry (Force "TODO") (eTrans $ FAp f args)
  | otherwise               = Store "TODO" (PartialFTag f (arity f - length args)) (map argTrans args)
sTrans x = uncurry (Force "TODO") (eTrans x)
-}

-- Translate evaluation expressions
eTrans :: SExpr -> (Call, Cont)
eTrans (SVar (Left x)) = (Eval x, NOp)
eTrans (SVar (Right x)) = error $ "Cannot evaluate primitive ref: " ++ show x
eTrans (Proj _ field x) = (Eval x, Select field)
eTrans (CafAp n args)
  | length args == 0 = (EvalCaf n, NOp)
  | otherwise        = (EvalCaf n, Apply args)
eTrans (FAp f args)
  | arity f == length args = (TLF f args, NOp)
  | otherwise              = (TLF f (take (arity f) args), Apply (drop (arity f) args))
eTrans (VAp n args)    = (Eval n, Apply args)
