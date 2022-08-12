module Simulate where

import InstrSet
import SimpleCore

import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------
-- Template instantiation

type State = (Heap,Stack,Locals,Stats)

type StackFrame = ([Node], [Cont], Return)
type Stack      = [StackFrame]
type Locals     = [Val]

type Arity = Int

data Val  = Ref Name
          | Pri Int

data Node = CNode Name [Name]
          | FNode Name Arity [Name]
          | PNode Name Arity Int [Name]
  deriving Show

data Return = RNTo Block
            | RRTo Block
            | RCase [IAlt]
            | RMain

type Heap = M.Map Name Node
data Stats = Stats { scCount :: Int
                   , primCount :: Int
                   , heapAllocs :: Int
                   , heapReads :: Int
                   , maxStkDepth :: Int }
  deriving Show

hInitial :: Heap
hInitial = M.empty

hAlloc :: Node -> Heap -> (Heap, Name)
hAlloc n h = let addr = fromMaybe 0 . fmap ((+1) . fst) $ M. h
             in (M.insert addr n h, addr)

hLookup :: Heap -> Name -> Node
hLookup h addr = fromMaybe (error "Accessed uninitisalised address on heap") $
                 M.lookup addr h

statInitial :: Stats
statInitial = Stats 0 0 0 0 0
