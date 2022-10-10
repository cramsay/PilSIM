{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module InstrSet where

import SimpleCore

data Tag = CTag CName
         | FTag FName
         | PTag FName Int
data Node = Node Tag [Var]

data Cont
  = NOp
  | Apply [Var]
  | Select Int
  | ICatch Var

data Func
  = IFun [Var] Block
  | ICaf       Block

data Block where
  Terminate :: Terminator -> Block
  (:>)      :: forall a. Show a =>
               Instr a -> (a -> Block) -> Block

infixr 6 :>

data Instr ret where
  Store    :: Node                 -> Instr Var
  PushCaf  :: CafName              -> Instr Var
  IPrimOp  :: PrimOp -> Var -> Var -> Instr Var
  Constant :: Int                  -> Instr Var
  ICall    :: Call -> Cont         -> Instr Node
  Force    :: Call -> Cont         -> Instr Var

data Terminator
  = Return Node
  | Jump Call Cont
  | ICase Call Cont [IAlt]
  | IIf PrimCmp Block Block
  | IThrow Var

data IAlt = IAlt Node Block

data Call
  = Eval    Var
  | EvalCaf CafName
  | TLF     FName [Var]
  | IFix    FName [Var]
