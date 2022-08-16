{-# LANGUAGE GADTs #-}
module Simulate where

import InstrSet
import SimpleCore

import qualified Data.Map.Lazy as M
import Control.Monad.State
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------
-- Template instantiation

type SimState = (Heap,Stack,Locals,Stats)
type Sim = State SimState

type StackFrame = ([Node'], [Cont], Return)
type Stack      = [StackFrame]
type Locals     = [Atom]

type Arity = Int

data Atom = Ref Name
          | Pri Int
  deriving Show

data Node' = CNode' Name [Atom]
          | FNode' Name Arity [Atom]
          | PNode' Name Arity Int [Atom]
  deriving Show

data Return = RNTo Block
            | RRTo Block
            | RCase [IAlt]
            | RMain

type Heap = M.Map Name Node'
data Stats = Stats { scCount :: Int
                   , primCount :: Int
                   , heapAllocs :: Int
                   , heapReads :: Int
                   , maxStkDepth :: Int }
  deriving Show

hInitial :: Heap
hInitial = M.empty

hAlloc :: Name -> Node' -> Sim ()
hAlloc n node h = let addr = 1 + M.size h
                      n' = "dh_" ++ show addr
                  in (M.insert n' n h, n')

hLookup :: Heap -> Name -> Node'
hLookup h addr = fromMaybe (error "Accessed uninitisalised address on heap") $
                 M.lookup addr h

statInitial :: Stats
statInitial = Stats 0 0 0 0 0

iSim :: Block -> Sim ()
iSim ((r, Store node     ) :> i) = undefined
iSim ((r, PushCaf n      ) :> i) = undefined
iSim ((r, IPrimOp op x y ) :> i) = undefined
iSim ((r, Constant n     ) :> i) = undefined
iSim ((r, ICall call cont) :> i) = undefined
iSim ((r, Force call cont) :> i) = undefined

iSim (Terminate (Return node)          ) = undefined
iSim (Terminate (Jump call cont)       ) = undefined
iSim (Terminate (ICase call cont alts) ) = undefined
iSim (Terminate (IIf cmp xblock yblock)) = undefined
iSim (Terminate (IThrow x)             ) = undefined

cSim :: Call -> Cont -> Return -> Sim ()
cSim (Eval x     ) e r = undefined
cSim (EvalCaf n  ) e r = undefined
cSim (TLF f args ) e r = undefined
cSim (IFix f args) e r = undefined

rSim :: Node -> Sim ()
rSim n = undefined

eSim :: Sim ()
eSim = undefined -- Need to inspect return stage, continuations, etc.

wSim :: Var -> Sim ()
wSim x = undefined
