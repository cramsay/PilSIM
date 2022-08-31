{-# LANGUAGE GADTs #-}
module Translate where

import InstrSet
import SimpleCore
import Pretty

import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace

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
  = Terminate $ Return (Node (CTag con) args)
tTrans am (Simple (FAp f args))
  | length args < arity = Terminate $ Return (Node (PTag f (arity - length args)) args)
  | otherwise                   = Terminate $ uncurry Jump $ eTrans am (FAp f args)
  where arity = getArity am f
tTrans am (Simple s)
  = Terminate $ uncurry Jump $ eTrans am s
tTrans am (Let  (Binding x s) e)
  = vTrans am s (subst x (tTrans am e))
tTrans am (LetS (Binding x s) e)
  = sTrans am s (subst x (tTrans am e))
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
altTrans am (Alt cn args e) = IAlt (Node (CTag cn) args) (tTrans am e)

-- Translate a lazy subexpression
vTrans :: ArityMap -> SExpr -> PartialBlock Var
vTrans am (CafAp n args)
  | length args == 0 = cont $ PushCaf n
  | otherwise        = PushCaf n `iseq` \h ->
                       Store (Node (FTag "ap") (h:args))
vTrans am (FAp n args)
  | arity >  length args = cont $ Store (Node (PTag n (arity - length args)) args)
  | arity == length args = cont $ Store (Node (FTag n) args)
  | otherwise            = Store (Node (FTag n) (take arity args)) `iseq` \h ->
                           Store (Node (FTag h) (h : drop arity args))
  where arity = getArity am n
vTrans am (VAp f args) = cont $ Store (Node (FTag "ap") (f:args))
vTrans am (Proj n field var)  -- TODO How do we interpret the "FSel_n" notation? Is that the name of a predefined function?
  = cont $ Store (Node (FTag n') [var])
  where n' = "sel_" ++ n
vTrans am e = undefined

-- Translate a strict subexpression
sTrans :: ArityMap -> SExpr -> PartialBlock Var
sTrans am (Int n) = cont $ Constant n
sTrans am (POp n [a,b]) = cont $ IPrimOp n a b
sTrans am (CAp n args) = cont $ Store (Node (CTag n) args)
sTrans am (VAp n args) = cont $ uncurry Force (eTrans am $ VAp n args)
sTrans am (Proj n field e) = cont $ uncurry Force (eTrans am $ Proj n field e)
sTrans am (FAp f args)
  | arity <= length args = cont $ uncurry Force (eTrans am $ FAp f args)
  | otherwise            = cont $ Store (Node (PTag f (arity - length args)) args)
  where arity = getArity am f
sTrans am x = cont $ uncurry Force (eTrans am x)

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

-- Subst a name in a block for a new name
subst :: Var -> Block -> (Var -> Block)
subst xold (Terminate t) xnew
  = Terminate $ substTerm xold t xnew
subst xold (i :> rest) xnew
  = substInstr xold i xnew :> \h -> subst xold (rest h) xnew

substTerm :: Var -> Terminator -> (Var -> Terminator)
substTerm xold (Return n) xnew = Return $ substNode xold n xnew
substTerm xold (Jump call cont) xnew = Jump (substCall xold call xnew)
                                            (substCont xold cont xnew)
substTerm xold (ICase call cont alts) xnew
  = ICase (substCall xold call xnew) (substCont xold cont xnew)
          (map (\a -> substAlt xold a xnew) alts)
substTerm xold (IIf cmp x y) xnew = IIf (substCmp xold cmp xnew)
                                      (subst xold x xnew)
                                      (subst xold y xnew)
substTerm xold (IThrow x) xnew
  = IThrow (substVar xold x xnew)

substInstr :: Var -> Instr a -> Var -> Instr a
substInstr xold (Store n) xnew = Store $ substNode xold n xnew
substInstr xold (PushCaf n) xnew = PushCaf n
substInstr xold (IPrimOp op a b) xnew = IPrimOp op (substVar xold a xnew)
                                                   (substVar xold b xnew)
substInstr xold (Constant n) xnew = Constant n
substInstr xold (ICall call cont) xnew = ICall (substCall xold call xnew)
                                               (substCont xold cont xnew)
substInstr xold (Force call cont) xnew = Force (substCall xold call xnew)
                                               (substCont xold cont xnew)

substNode :: Var -> Node -> Var -> Node
substNode xold (Node tag args) xnew = Node tag (map (\v -> substVar xold v xnew) args)

substCmp :: Var -> PrimCmp -> Var -> PrimCmp
substCmp xold (IntEQ x y) xnew = IntEQ   (substVar xold x xnew)
                                         (substVar xold y xnew)
substCmp xold (IntLT x y) xnew = IntLT   (substVar xold x xnew)
                                         (substVar xold y xnew)
substCmp xold (IntGT x y) xnew = IntGT   (substVar xold x xnew)
                                         (substVar xold y xnew)
substCmp xold (IntLTE x y) xnew = IntLTE (substVar xold x xnew)
                                         (substVar xold y xnew)
substAlt :: Var -> IAlt -> Var -> IAlt
substAlt xold (IAlt (Node tag args) block) xnew
 -- | xold `elem` args = IAlt (Node tag args) block -- Don't recurse if we'd be shadowing
 -- | otherwise = IAlt (substNode xold (Node tag args) xnew)
 --                    (subst xold block xnew)
  = IAlt (substNode xold (Node tag args) xnew)
         (subst xold block xnew)

substCall :: Var -> Call -> Var -> Call
substCall xold (Eval n) xnew = Eval (substVar xold n xnew)
substCall xold (EvalCaf n) xnew = EvalCaf n
substCall xold (TLF  n args) xnew = TLF n (map (\v -> substVar xold v xnew) args)
substCall xold (IFix n args) xnew = IFix (substVar xold n xnew) (map (\v -> substVar xold v xnew) args)

substCont :: Var -> Cont -> Var -> Cont
substCont xold NOp xnew = NOp
substCont xold (Apply args) xnew = Apply $ map (\v -> substVar xold v xnew) args
substCont xold (Select n) xnew = Select n
substCont xold (ICatch n) xnew = ICatch $ substVar xold n xnew

substVar :: Var -> Var -> Var -> Var
substVar old x new
  | old == x = new
  | otherwise = x
