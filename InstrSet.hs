{-# LANGUAGE GADTs #-}

module InstrSet where

import SimpleCore

data Tag       -- Do we need to carry names here?
  = CTag CName
  | FTag FName
  | PartialFTag FName Int

data Cont
  = NOp
  | Apply [Var]
  | Select Int
  -- | Catch RefVar

data Func
  = IFun [Var] Block
  | ICaf        Block

data Block where
  BlockNil  :: Terminator -> Block
  BlockCons :: Instr a -> (a -> Block) -> Block

data Instr ret where
  Store    :: Tag -> [Var]                 -> Instr RefVar
  PushCaf  :: CafName                      -> Instr RefVar
  IPrimOp  :: PrimOp -> PrimVar -> PrimVar -> Instr PrimVar
  Constant :: Int                          -> Instr PrimVar
  ICall    :: Call -> Cont                 -> Instr (Tag, [Var])
  Force    :: Call -> Cont                 -> Instr RefVar

data Terminator
  = Return Tag [Var]
  | Jump Call Cont
  | ICase Call Cont [IAlt]
  | IIf Cmp Block Block
  -- | Throw RefVar

data IAlt = IAlt Tag [Var] Block

data Call
  = Eval RefVar
  | EvalCaf CafName
  | TLF     FName [Var]
  -- | Fix     FName [Var]
