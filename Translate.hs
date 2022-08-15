{-# LANGUAGE GADTs #-}
module Translate where

import InstrSet
import SimpleCore

import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad.State

-- Helpers for our translation state
-- Used for unique naming and looking up arities of global function

data TransState = TransState
  { nextI :: Int -- Generated name counter
  , arities :: [(FName, Int)]
  }

initTrans :: [TExpr] -> TransState
initTrans fns = TransState
                  0
                  (map (\f->(fname f, arity f)) fns)

alloc :: String -> State TransState Name
alloc base
  = do s <- get
       let n = base ++ "_" ++ show (nextI s)
       put $ s { nextI = nextI s + 1 }
       return n

noteArity :: Var -> Int -> State TransState ()
noteArity f arity
  = do s <- get
       put $ s { arities = (f,arity) : arities s }

arityLookup :: Var -> State TransState Int
arityLookup f = do s <- get
                   pure $ fromMaybe (error $ "Looked up a function missing from context: " ++ f)
                                    (lookup f (arities s))

allocInstr :: Instr a -> State TransState a
allocInstr (Store node) = alloc "h"
allocInstr (PushCaf n) = alloc "caf"
allocInstr (IPrimOp op x y) = alloc "prim"
allocInstr (Constant n) = alloc "n"
allocInstr (ICall call cont)
  = do c  <- alloc "c"
       h1 <- alloc "arg" -- TODO we don't know ahead of time how many (used) args our node will have.
       h2 <- alloc "arg" -- for now, just return a hard coded maximum
       h3 <- alloc "arg" -- for now, just return a hard coded maximum
       h4 <- alloc "arg" -- for now, just return a hard coded maximum
       return (CNode c [h1,h2,h3,h4])
allocInstr (Force call cont) = alloc "h"

clausify :: Instr a -> State TransState (a, Instr a)
clausify rhs = do lhs <- allocInstr rhs
                  pure (lhs, rhs)

translate :: [TExpr] -> [(Name, Func)]
translate prog = evalState (traverse topTrans prog) (initTrans prog)

topTrans :: TExpr -> State TransState (Name, Func)
topTrans (Fun f args body) = do blk <- tTrans body
                                return (f, IFun args blk)
topTrans (Caf g      body) = do blk <- tTrans body
                                return (g, ICaf blk)

tTrans :: Expr -> State TransState Block
tTrans (Simple (CAp con args)) = pure $ Terminate $ Return (CNode con args)
tTrans (Simple (FAp f args))
  = do arity <- arityLookup f
       case length args < arity of
         True  -> pure $ Terminate $ Return (PartialF f (arity - length args) args)
         False -> do (call, cont) <- eTrans (FAp f args)
                     pure $ Terminate $ Jump call cont
tTrans (Simple s) = do (call, cont) <- eTrans s
                       pure $ Terminate  $ Jump call cont
tTrans (Let  (Binding x s) e) = do s' <- vTrans s x
                                   e' <- tTrans e
                                   pure $ s' e'
tTrans (LetS (Binding x s) e) = do e' <- tTrans e
                                   s' <- sTrans s
                                   pure $ (x, s') :> e'
tTrans (Case s alts) = case (catMaybes $ map matchAlt alts) of
                             -- [(con, args, e)]  -> Terminate $ ICall (CTag con) args e NOp
                             -- TODO implement unambiguous scrutinee optimisations
                             []                -> do alts' <- traverse altTrans alts
                                                     (call, cont) <- eTrans s
                                                     pure $ Terminate $ ICase call cont alts'
tTrans (If cmp x y) = do x' <- tTrans x
                         y' <- tTrans y
                         pure $ Terminate $ IIf cmp x' y'
tTrans (Fix f args) = pure $ Terminate $ Jump (IFix f args) NOp
tTrans (Try f args h) = pure $ Terminate $ Jump (TLF f args) (ICatch h)
tTrans (Throw x) = pure $ Terminate $ IThrow x

matchAlt :: Alt -> Maybe ()
matchAlt _ = Nothing

altTrans :: Alt -> State TransState IAlt
altTrans (Alt cn args e) = do e' <- tTrans e
                              pure $ IAlt (CNode cn args) e'

-- Translate a lazy subexpression
vTrans :: SExpr -> Name -> State TransState (Block -> Block)
vTrans (CafAp n args) lhs
  | length args == 0 = pure $ \rest -> (lhs, PushCaf n) :> rest
  | otherwise        = do (h, i) <- clausify (PushCaf n)
                          let i' = Store (FNode h (h:args))
                          pure $ \rest -> (h,i) :> (lhs, i') :> rest
     -- TODO Really don't know how to interpret the F_AP. Don't think I need names in tags?
     -- Should _all_ names have an associated arity?
vTrans (FAp n args) lhs
  = do arity <- arityLookup n
       case compare arity (length args) of
         GT -> pure $ \rest -> (lhs, Store (PartialF n (arity - length args) args)) :> rest
         EQ -> pure $ \rest -> (lhs, Store (FNode n args)) :> rest
         LT -> do (h,i) <- clausify (Store (FNode n (take arity args)))
                  let i' = Store (FNode h (h : drop arity args))
                  pure $ \rest -> (h,i) :> (lhs, i') :> rest
vTrans (VAp n args) lhs = pure $ \rest -> (lhs, Store (FNode n args)) :> rest
vTrans (Proj n field var) lhs = pure $ \rest -> (lhs, Store (FNode ("sel_" ++ n ++ show field) [var])) :> rest

-- Translate a strict subexpression
sTrans :: SExpr -> State TransState (Instr Var)
sTrans (Int n) = pure $ Constant n
sTrans (POp n [x,y]) = pure $ IPrimOp n x y
sTrans (CAp n args) = pure $ Store (CNode n args)
sTrans (VAp n args) = do (call, cont) <- eTrans $ VAp n args
                         pure $ Force call cont
sTrans (Proj n field x) = do (call, cont) <- eTrans $ Proj n field x
                             pure  $ Force call cont
sTrans (FAp f args)
  = do arity <- arityLookup f
       case arity <= length args of
         True -> do (call, cont) <- eTrans $ FAp f args
                    pure $ Force call cont
         False -> pure $ Store (PartialF f (arity - length args) args)
sTrans x = do (call, cont) <- eTrans x
              pure $ Force call cont

-- Translate evaluation expressions
eTrans :: SExpr -> State TransState (Call, Cont)
eTrans (SVar x) = pure (Eval x, NOp)
eTrans (Proj _ field x) = pure (Eval x, Select field)
eTrans (CafAp n args)
  | length args == 0 = pure (EvalCaf n, NOp)
  | otherwise        = pure (EvalCaf n, Apply args)
eTrans (FAp f args)
  = do arity <- arityLookup f
       case arity == length args of
         True  -> pure (TLF f args, NOp)
         False -> pure (TLF f (take arity args), Apply (drop arity args))
eTrans (VAp n args) = pure (Eval n, Apply args)
