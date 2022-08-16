module Pretty where

import InstrSet

import Control.Monad.State

type Printer a = State Int a

nextName :: Name -> Printer Name
nextName base = do n <- get
                   put (n+1)
                   pure $ base ++ "_$" ++ show n

showTerminator :: Terminator -> Printer String

showFunc :: Func -> Printer String

showIAlt :: IAlt -> Printer String

showBlock :: Block -> Printer String
showBlock (Terminate t)
