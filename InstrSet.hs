{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module InstrSet where

import SimpleCore

import Control.Monad.State

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
  Terminate :: Terminator -> Block
  (:>)      :: forall a. Show a =>
               (a, Instr a) -> Block -> Block

infixr 6 :>

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

-- We get very iffy from here down!

deriving instance Show Tag
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
