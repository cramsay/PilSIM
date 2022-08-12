{-# LANGUAGE GADTs #-}
module Translate where

import InstrSet
import SimpleCore

import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad.State

data TransState = TransState
  { nextI :: Int -- Generated name counter
  , arities :: [(FName, Int)]
  , env     :: [(Name,  Val)]
  }

initTrans :: [TExpr] -> TransState
initTrans fns = TransState
                  0
                  (map (\f->(fname f, arity f)) fns)
                  []

alloc :: String -> State TransState Name
alloc base
  = do s <- get
       let n = base ++ "_" ++ show (nextI s)
       put $ s { nextI = nextI s + 1
               , env   = (n, Ref n) : env s}
       return n

newPrim :: String -> State TransState Name
newPrim base
  = do s <- get
       let n = base ++ "_" ++ show (nextI s)
       put $ s { nextI = nextI s + 1
               , env   = (n, Prim n) : env s}
       return n

cleanEnv :: TExpr -> State TransState ()
cleanEnv (Fun _ args _) = do s <- get
                             let env' = zip args ()
                             put $ s { env = env' }
cleanEnv (Caf _      _) = do s <- get
                             pure $ s { env = [] }

envLookup :: Var -> State TransState Val
envLookup x = do s <- get
                 pure $ fromMaybe (error $ "Looked up a name missing from environment: " ++ x)
                                  (lookup x (env s))

envLookups :: [Var] -> State TransState [Val]
envLookups = traverse envLookup

arityLookup :: Var -> State TransState Int
arityLookup f = do s <- get
                   pure $ fromMaybe (error $ "Looked up a function missing from context: " ++ f)
                                    (lookup f (arities s))

{-
allocInstr :: Instr a -> State TransState a
allocInstr (Store tag args) = alloc "h"
allocInstr (PushCaf n) = alloc "caf"
allocInstr (IPrimOp op x y) = alloc "prim"
allocInstr (Constant n) = alloc "n"
allocInstr (ICall call cont)
  = do c  <- alloc "c"
       h1 <- alloc "arg" -- TODO we don't know ahead of time how many (used) args our node will have.
       h2 <- alloc "arg" -- for now, just return a hard coded maximum
       h3 <- alloc "arg" -- for now, just return a hard coded maximum
       h4 <- alloc "arg" -- for now, just return a hard coded maximum
       return (CTag c, [h1,h2,h3,h4])
allocInstr (Force call cont) = alloc "h"

clausify :: Instr a -> State TransState (a, Instr a)
clausify rhs = do lhs <- allocInstr rhs
                  pure (lhs, rhs)

translate :: [TExpr] -> [(Name, Func)]
translate prog = evalState (traverse topTrans prog) initTrans

topTrans :: TExpr -> State TransState (Name, Func)
topTrans (Fun f args body) = do blk <- tTrans body
                                return (name f, IFun args blk)
topTrans (Caf g      body) = do blk <- tTrans body
                                return (g     , ICaf blk)

tTrans :: Expr -> State TransState Block
tTrans (Simple (CAp con args)) = pure $ Terminate $ Return (CTag con) args
tTrans (Simple (FAp f args))
  | length args < arity f = pure $ Terminate $ Return (PartialFTag f (arity f - length args)) args
  | otherwise             = pure $ Terminate $ uncurry Jump (eTrans (FAp f args))
tTrans (Simple s) = pure $ Terminate  $ uncurry Jump (eTrans s)
tTrans (Let  (Binding x s) e) = do s' <- vTrans s x
                                   e' <- tTrans e
                                   pure $ s' e'
tTrans (LetS (Binding x s) e) = do e' <- tTrans e
                                   pure $ (x, sTrans s) :> e'
tTrans (Case s alts) = case (catMaybes $ map matchAlt alts) of
                             -- [(con, args, e)]  -> Terminate $ ICall (CTag con) args e NOp
                             -- TODO implement unambiguous scrutinee optimisations
                             []                -> do alts' <- traverse altTrans alts
                                                     pure $ Terminate $ uncurry ICase (eTrans s) alts'
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
                              pure $ IAlt (CTag cn) args e'

-- Translate a lazy subexpression
vTrans :: SExpr -> Name -> State TransState (Block -> Block)
vTrans (CafAp n args) lhs
  | length args == 0 = pure $ \rest -> (lhs, PushCaf n) :> rest
  | otherwise        = do (h, i) <- clausify (PushCaf n)
                          let i' = Store (FTag (h, length args)) (h:args)
                          pure $ \rest -> (h,i) :> (lhs, i') :> rest
     -- TODO Really don't know how to interpret the F_AP. Don't think I need names in tags?
     -- Should _all_ names have an associated arity?
vTrans (FAp n args) lhs
  | arity n >  length args = pure $ \rest -> (lhs, Store (PartialFTag n (arity n - length args)) args) :> rest
  | arity n == length args = pure $ \rest -> (lhs, Store (FTag n) args) :> rest
  | otherwise              = do (h,i) <- clausify (Store (FTag n) (take (arity n) args))
                                let i' = Store (FTag (h, length args - arity n)) (h : drop (arity n) args)
                                pure $ \rest -> (h,i) :> (lhs, i') :> rest
vTrans (VAp n args) lhs = pure $ \rest -> (lhs, Store (FTag (n,length args)) args) :> rest
vTrans (Proj n field var) lhs = pure $ \rest -> (lhs, Store (FTag ("sel_" ++ n ++ show field,1)) [var]) :> rest

-}

-- Translate a strict subexpression
sTrans :: SExpr -> State TransState (Instr Val)
sTrans (Int n) = pure $ Constant n
sTrans (POp n [x,y]) = pure $ IPrimOp n x y
sTrans (CAp n args) = Store (CNode n args)
sTrans (VAp n args) = uncurry Force (eTrans $ VAp n args)
sTrans (Proj n field x) = uncurry Force (eTrans $ Proj n field x)
sTrans (FAp f args)
  | arity f <= length args = uncurry Force (eTrans $ FAp f args)
  | otherwise               = Store (PartialFTag f (arity f - length args)) args
sTrans x = uncurry Force (eTrans x)

-- Translate evaluation expressions
eTrans :: SExpr -> State TransState (Call, Cont)
eTrans (SVar x) = pure (Eval x, NOp)
eTrans (Proj _ field x) = pure (Eval x, Select field)
eTrans (CafAp n args)
  | length args == 0 = pure (EvalCaf n, NOp)
  | otherwise        = do vals <- envLookups args
                          pure (EvalCaf n, Apply vals)
eTrans (FAp f args)
  = do arity <- arityLookup f
       vals <- envLookups args
       case arity == length args of
         True  -> pure (TLF f vals, NOp)
         False -> pure (TLF f (take arity vals), Apply (drop arity vals))
eTrans (VAp n args) = do vals <- envLookups args
                         pure (Eval n, Apply vals)
