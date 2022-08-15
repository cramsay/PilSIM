{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module InstrSet where

import SimpleCore

data Node
  = CNode    CName     [Var]
  | FNode    FName     [Var]
  | PartialF FName Int [Var]

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
               (a, Instr a) -> Block -> Block

infixr 6 :>

data Instr ret where
  Store    :: Node                 -> Instr Var -- Var
  PushCaf  :: CafName              -> Instr Var -- Var
  IPrimOp  :: PrimOp -> Var -> Var -> Instr Var -- Prim
  Constant :: Int                  -> Instr Var -- Var
  ICall    :: Call -> Cont         -> Instr Node
  Force    :: Call -> Cont         -> Instr Var -- Var

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

deriving instance Show Node
deriving instance Show Cont
deriving instance Show Func
deriving instance Show Terminator
deriving instance Show IAlt
deriving instance Show Call
deriving instance Show (Instr a)

instance Show Block where
  show = unlines . go
    where go (Terminate t) = [show t]
          go ((lhs,rhs) :> rest) = (show lhs ++ " <- " ++ show rhs) : go rest
