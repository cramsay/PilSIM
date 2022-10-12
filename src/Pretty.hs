{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pretty where

import InstrSet
import SimpleCore

import Control.Monad.State

type Printer a = State Int a
type Indent = String

initPrinter = 0

pretty :: (Name, Func) -> String
pretty (n,f) = n ++ " " ++ evalState (showFunc f) initPrinter

nextName :: Name -> Printer Name
nextName base = do n <- get
                   put (n+1)
                   pure $ base ++ "_$" ++ show n

consperse :: String -> [String] -> String
consperse sep [] = ""
consperse sep [a]  = a
consperse sep (a:as) = a ++ sep ++ consperse sep as

padWith :: String -> Int -> String -> String
padWith pad n acc
  | n <= length acc = acc
  | otherwise = padWith pad n (acc ++ pad)

showTerminator :: Indent -> Terminator -> Printer String
showTerminator indent (Return n) = pure $ indent ++ "Return " ++ show n
showTerminator indent (Jump call cont) = pure $ indent ++ "Jump (" ++ show call ++ ") (" ++ show cont ++ ")"
showTerminator indent (ICase call cont alts)
  = do altStrs <- mapM (showIAlt (indent ++ "  ")) alts
       pure $ indent ++ "case (" ++ show call ++ ") (" ++ show cont ++ ") of \n" ++ unlines altStrs
showTerminator indent (IIf cmp x y)
  = do x' <- showBlock (indent ++ "  ") x
       y' <- showBlock (indent ++ "  ") y
       pure $ indent ++ "If " ++ show cmp ++ "\n" ++ indent ++ "Then\n" ++ x' ++
                      "\n" ++ indent ++ "Else\n" ++ y'
showTerminator indent (IThrow x) = pure $ indent ++ "throw " ++ x

showFunc :: Func -> Printer String
showFunc (IFun args body)
  = do rest <- showBlock "  " body
       pure $ "\\" ++ consperse ", " args ++ " -> \n" ++ rest
showFunc (ICaf body)
  = do rest <- showBlock "  " body
       pure $ " -> \n" ++ rest

showIAlt :: Indent -> IAlt -> Printer String
showIAlt indent (IAlt n b)
  = do rest <- showBlock ("  " ++ indent) b
       pure $ indent ++ show n ++ " -> \n" ++ rest

evalInstr :: Instr a -> Printer a
evalInstr (Store node) = nextName "h"
evalInstr (PushCaf g) = nextName "g"
evalInstr (IPrimOp op x y) = nextName ("p" ++ show op)
evalInstr (Constant n) = nextName "i"
evalInstr (Force call cont) = nextName "h"
evalInstr (ICall call cont)
  = do t <- nextName "tag"
       args <- mapM nextName (replicate 4 "arg")
       pure $ Node (CTag t) args
       -- ^ FIXME We don't know # arguments or tag type until we eval...

showBlock :: Indent -> Block -> Printer String
showBlock indent (Terminate t) = showTerminator indent t
showBlock indent (i :> rest)
  = do a <- evalInstr i
       rest <- showBlock indent (rest a)
       pure $ indent ++ show a ++ " <- " ++ show i ++ ";\n" ++ rest

instance Show Node where
  show (Node tag args) = show tag ++ " " ++ consperse " " args

instance Show Tag where
  show (CTag n) = "C_" ++ n
  show (FTag n) = "F_" ++ n
  show (PTag n i) = "P_" ++ n ++ "^" ++ show i

instance Show Call where
  show (Eval n) = "Eval " ++ n
  show (EvalCaf n) = "EvalCaf " ++ n
  show (TLF n args) = "TLF " ++ n ++ " " ++ consperse " " args
  show (IFix n args) = "IFix " ++ n ++ " " ++ consperse " " args

deriving instance Show (Instr a)

instance Show Cont where
  show (NOp) = "()"
  show (Apply args) = "Apply " ++ consperse " " args
  show (Select n) = "Select " ++ show n
  show (ICatch n) = "Catch " ++ n

instance Show Terminator where
  show (Return n  ) = "Return " ++ show n
  show (Jump ca co) = "Jump " ++ show ca ++ " " ++ show co
  show (ICase ca co alts) = "Case " ++ show ca ++ " " ++ show co ++ "..."
  show (IIf cmp t f) = "If " ++ show cmp ++ "..."
  show (IThrow x   ) = "Throw " ++ show x
