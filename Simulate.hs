{-# LANGUAGE GADTs #-}
module Simulate where

import InstrSet
import SimpleCore
import Pretty

import qualified Data.Map.Lazy as M
import Control.Monad.State
import Data.Maybe
import Data.List

data EvalMode = EvalI Block
              | EvalE
              | EvalW Atom

type Program = [(Name, Func)]
data SimState = SimState { heap :: Heap
                         , stack :: Stack
                         , locals :: Locals
                         , code :: Program
                         , stats :: Stats }
type Sim = State SimState

type StackFrame = ([(Name,Node')], [Cont'], Return)
type Stack      = [StackFrame]
type Locals     = [(Name,Atom)]

type Arity = Int

data Atom = Ref Name
          | Pri Int
  deriving Show

data Node' = Node' Tag [Atom]
  deriving Show

convNode :: Node -> Node'
convNode (Node tag args) = Node' tag (map Ref args)

unconvNode :: Node' -> Node
unconvNode (Node' tag args) = Node tag (map toName args)
  where toName (Ref n) = n
        toName (Pri i) = "p"++show i

data Cont' = NOp'
           | Apply' [Atom]
           | Select' Int
           | ICatch' Atom
           | Update' Atom

data Return = RNTo (Node  -> Block)
            | RRTo (Name  -> Block)
            | RCase [IAlt]
            | RNext
            | RMain

type Heap = M.Map Name Node'
data Stats = Stats { scCount :: Int
                   , primCount :: Int
                   , heapAllocs :: Int
                   , heapReads :: Int
                   , maxStkDepth :: Int
                   , nameI :: Int}
  deriving Show

newName :: String -> Sim Name
newName base
  = do s <- get
       let st = stats s
           n = base ++ show (1 + (nameI st))
       put $ s { stats = st {nameI = 1 + nameI st} }
       pure n


hInitial :: Heap
hInitial = M.empty

hAlloc :: Name -> Node' -> Sim Name
hAlloc n node
  = do n' <- newName n
       s <- get
       put $ s { heap = M.insert n' node (heap s) }
       pure n'

hLookup :: Name -> Sim Node'
hLookup addr
  = do h <- heap <$> get
       pure $ fromMaybe (error "Accessed uninitisalised address on heap")
                        (M.lookup addr h)

hUpdate :: Name -> Node' -> Sim ()
hUpdate n node
  = do s <- get
       put $ s { heap = M.insert n node (heap s) }

qPush :: Atom -> Sim Name
qPush a =
  do n <- newName "q"
     s <- get
     put $ s { locals = (n,a) : locals s }
     return n

qRead :: Name -> Sim Atom
qRead n =
  do s <- get
     case lookup n (locals s) of
       Just a  -> pure a
       Nothing -> error $ "Local variable not found in queue: " ++ show n

qEmpty :: Sim ()
qEmpty = do s <- get
            put $ s { locals = [] }

sCompress :: Node' -> Sim ()
sCompress n
  = do s <- get
       let ((_,topConts,topRets) : rest) = stack s
       put $ s { stack = ([("ret", n)],topConts,topRets) : rest }
       -- ^ Should  be OK to make up the "ret" name here since caller will grab it implicitly from the top of the stack (?)

sPush :: StackFrame -> Sim ()
sPush frame
  = do s <- get
       put $ s { stack = frame : stack s }

sPop :: Sim StackFrame
sPop
  = do s <- get
       let (top : rest) = stack s
       put $ s { stack = rest }
       return top

statInitial :: Stats
statInitial = Stats 0 0 0 0 0 0

simInitial :: Program -> SimState
simInitial prog = SimState hInitial [frame] [] prog statInitial  -- TODO Add CAF nodes to heap
  where frame = ([("main", Node' (FTag "main") [])]
                ,[]
                ,RMain)

cycle :: EvalMode -> Sim ()
cycle (EvalI code) = iSim code
cycle EvalE = eSim
cycle (EvalW x) = wSim x

iSim :: Block -> Sim ()
iSim (Store node :> is)
  = do y <- hAlloc "h" (convNode node)
       iSim (is y)

iSim (PushCaf g :> is)
  = do g' <- qPush (Ref g)
       iSim (is g')

iSim (IPrimOp op x y :> is)
  = do x' <- qRead x
       y' <- qRead y
       z  <- qPush (doOp op x' y')
       iSim (is z)
  where doOp Plus (Pri a) (Pri b) = Pri (a+b)
        doOp Sub  (Pri a) (Pri b) = Pri (a-b)
        doOp LEq  (Pri a) (Pri b) = Pri $ if (a <= b) then 1 else 0

iSim (Constant n :> is)
  = do c <- qPush (Pri n)
       iSim (is c)

iSim (ICall call cont :> is)
  = cSim call cont (RNTo is)
iSim (Force call cont :> is)
  = cSim call cont (RRTo is)

iSim (Terminate (Return (Node tag args)))
  = do vals <- traverse qRead args
       rSim (Node' tag vals)

iSim (Terminate (Jump call cont))
  = do cSim call cont RNext

iSim (Terminate (ICase call cont alts))
  = cSim call cont (RCase alts)

iSim (Terminate (IIf cmp xblock yblock))
  = do cond <- doCmp cmp
       if cond
          then iSim xblock
          else iSim yblock
  where doCmp (IntEQ  a b) = go (==) a b
        doCmp (IntLT  a b) = go (<)  a b
        doCmp (IntGT  a b) = go (>)  a b
        doCmp (IntLTE a b) = go (<=) a b
        go f a b = do a' <- qRead a
                      b' <- qRead b
                      pure $ f a b

iSim (Terminate (IThrow x))
  = do val <- qRead x
       qEmpty
       wSim val

cLookup :: Name -> Sim Block
cLookup n
  = do s <- get
       case lookup n (code s) of
         Just (IFun _ is) -> pure is
         _                -> error $ "Failed to find code for function: " ++ n

constructCont :: Cont -> Sim Cont'
constructCont NOp = pure $ NOp'
constructCont (Select n) = pure $ Select' n
constructCont (ICatch n) = ICatch' <$> (qRead n)
constructCont (Apply ns) = Apply' <$> (mapM qRead ns)

cSim :: Call -> Cont -> Return -> Sim ()
cSim (Eval x) e r
  = do e' <- constructCont e
       x' <- qRead x
       n  <- hLookup x -- TODO Paper says to use x'?
       sPush ( [("eval", n)]
             , [e']
             , r )
       qEmpty
       qPush x' --FIXME we need this to still be called, x, right?
       eSim

cSim (EvalCaf g) e r
  = do e' <- constructCont e
       n  <- hLookup g
       sPush ( [("evalcaf", n)]
             , [e']
             , r )
       qEmpty
       qPush (Ref g)
       eSim

cSim (TLF f args) e r
  = do e' <- constructCont e
       vals <- mapM qRead args
       sPush ( [(f, Node' (FTag f) vals)]
             , [e']
             , r )
       is <- cLookup f
       iSim is

cSim (IFix f args) e r
  = do e' <- constructCont e
       y <- hAlloc "tmp" (Node' (CTag "tmp") [])
       vals <- mapM qRead args
       sPush ( [(f, Node' (FTag f) vals)]
             , [Update' (Ref y), e']
             , r )
       is <- cLookup f
       qPush (Ref y)
       iSim is

rSim :: Node' -> Sim ()
rSim n = do sCompress n
            qEmpty
            eSim

eSim :: Sim ()
eSim = do s <- get
          go (heap s) (head $ stack s) (locals s)
  where
    go h ( [(_, Node' (FTag f) vals)] , c, r ) [(_,u)]
      = do _ <- sPop
           sPush ( [(f, Node' (FTag f) vals)]
                 , Update' u : c
                 , r )
           qEmpty
           is <- cLookup f
           iSim is

    go h ( [(_, Node' (FTag f) vals)] , c, r ) []
      = do _ <- sPop
           sPush ( [(f, Node' (FTag f) vals)]
                 , c
                 , r )
           is <- cLookup f
           iSim is

    go h ( [(name, n)] , Update' (Ref u) : c, r ) []
      = do hUpdate u n
           _ <- sPop
           sPush ( [ (name, n) ]
                 , c
                 , r )
           eSim

    go h ( [(name, n)] , ICatch' _ : c, r ) []
      = do _ <- sPop
           sPush ( [ (name, n) ]
                 , c
                 , r )
           eSim

    go h ( [(_, Node' (CTag cn) vals)] , Select' i : c, r ) []
      = do let (Ref xi) = vals !! i
           n <- hLookup xi
           _ <- sPop
           sPush ( [ ("?", n) ]
                 , c
                 , r )

    go h ( [(sn, Node' (PTag fn m) vals)] , Apply' as : c, r ) []
      | m >  length as = do _ <- sPop
                            sPush ( [(sn, Node' (PTag fn (m-length as)) (vals ++ as))]
                                  , c
                                  , r )
                            eSim
      | m == length as = do _ <- sPop
                            sPush ( [(sn, Node' (FTag fn) (vals ++ as))]
                                  , c
                                  , r )
                            eSim
      | otherwise      = do let rem = length as - m
                            _ <- sPop
                            sPush ( [(sn, Node' (FTag fn) (vals ++ take rem as))]
                                  , Apply' (drop rem as) : c
                                  , r )
                            eSim

    go h ( [n] , [] , RNext ) []
      = do _ <- sPop
           (nodes, conts, ret) <- sPop
           sPush ( [n]
                 , conts
                 , ret )
           eSim

    go h ( [(name,n)], [] , RNTo retCode ) []
      = do _ <- sPop
           (nodes, conts, ret) <- sPop
           sPush ( (name, n) : nodes
                 , conts
                 , ret )
           iSim (retCode $ unconvNode n)

    go h ( [(name,n)], [] , RRTo retCode ) []
      = do _ <- sPop
           y <- hAlloc "h" n
           qPush (Ref y)
           iSim (retCode y)

    go h ( [(n, Node' (CTag cn) vals)] , [] , RCase alts ) []
      = do _ <- sPop
           (nodes, conts, ret) <- sPop
           sPush ( (n, Node' (CTag cn) vals) : nodes
                 , conts
                 , ret )
           let is = selectAlt cn alts
           iSim is
       where selectAlt cn ( IAlt (Node (CTag cn') _) code : rest)
               | cn == cn' = code
               | otherwise = selectAlt cn rest
    -- ^ TODO do I have to resolve project field names? These are like ARGs

    go h ( [(name, n)] , [] , RMain ) []
      = pure () -- End of program!

wSim :: Atom -> Sim ()
wSim x = do (nodes, conts, ret) <- sPop
            go nodes conts ret
  where go nodes (ICatch' (Ref h) : conts) ret
          = do n <- hLookup h
               sPush ( [("_",n)]
                     , Apply' [x] : conts
                     , ret )
               qPush (Ref h)
               eSim
        go nodes (c : conts) ret
          = do sPush ( nodes, conts, ret )
               wSim x
        go nodes [] RMain
          = pure () -- Exit with exception
        go nodes [] ret
          = wSim x

{-

I'm not sure if node stack entries should even have a name?

Need to work out if we should look for locals in queue or as atoms from node
Maybe annotate names in InstrSet?

-}
