{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module InstrSet where

import SimpleCore

data Val = Ref  Var
         | Prim Int

data Node
  = CNode    CName     [Val]
  | FNode    FName     [Val]
  | PartialF FName Int [Val]

data Cont
  = NOp
  | Apply [Val]
  | Select Int
  | ICatch Var

data Func
  = IFun [Val] Block
  | ICaf       Block

data Block where
  Terminate :: Terminator -> Block
  (:>)      :: forall a. Show a =>
               (a, Instr a) -> Block -> Block

infixr 6 :>

data Instr ret where
  Store    :: Node                 -> Instr Val -- Var
  PushCaf  :: CafName              -> Instr Val -- Var
  IPrimOp  :: PrimOp -> Int -> Int -> Instr Val -- Prim
  Constant :: Int                  -> Instr Val -- Var
  ICall    :: Call -> Cont         -> Instr Node
  Force    :: Call -> Cont         -> Instr Val -- Var

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
  | TLF     FName [Val]
  | IFix    FName [Val]

deriving instance Show Node
deriving instance Show Val
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
