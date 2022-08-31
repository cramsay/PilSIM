{-# LANGUAGE GADTs #-}
module Simulate where

import InstrSet
import SimpleCore
import Pretty

import qualified Data.Map.Lazy as M
import Control.Monad.State
import Data.Maybe
import Data.List
import Debug.Trace

data EvalMode = EvalI Block
              | EvalE
              | EvalW Atom

type Program = [(Name, Func)]
data SimState = SimState { heap :: Heap
                         , stack :: Stack
                         , locals :: Locals
                         , env :: Env
                         , code :: Program
                         , stats :: Stats }

instance Show SimState where
  show s = concat  [ "HEAP   ~> " ++ showLines (M.toList $ heap s)
                   , "Stack  ~> " ++ showLines (stack s)
                   , "Locals ~> " ++ showLines (locals s)
                   , "Env    ~> " ++ showLines (env s)]
    where indent =   "          "
          showLines :: Show a => [a] -> String
          showLines [] = unlines [""]
          showLines (a:as) = unlines $ show a : map (\a -> indent ++ show a) as

data NameType = Local
              | Arg Name Int -- Node name + index to arg
  deriving (Eq, Show)

type Sim = State SimState

type StackFrame = ([(Name,Node')], [Cont'], Return)
type Stack      = [StackFrame]
type Locals     = [(Name,Atom)]
type Env        = [(Name, NameType)]

type Arity = Int

data Atom = Ref Name
          | Pri Int
  deriving Show

data Node' = Node' Tag [Atom]
  deriving Show

unconvNode :: Node' -> Node
unconvNode (Node' tag args) = Node tag (map toName args)
  where toName (Ref n) = n
        toName (Pri i) = "p"++show i

data Cont' = Apply' [Atom]
           | Select' Int
           | ICatch' Atom
           | Update' Atom
  deriving Show

data Return = RNTo (Node  -> Block)
            | RRTo (Name  -> Block)
            | RCase [IAlt]
            | RNext
            | RMain

instance Show Return where
  show RNext = "RNext"
  show RMain = "RMain"
  show (RCase _) = "RCase [alts]"
  show (RNTo _) = "RNTo [code]"
  show (RRTo _) = "RRTo [code]"

type Heap = M.Map Name Node'
data Stats = Stats { scCount :: Int
                   , primCount :: Int
                   , heapAllocs :: Int
                   , heapReads :: Int
                   , maxStkDepth :: Int
                   , cycles :: Int
                   , nameI :: Int}
  deriving Show

newName :: String -> Sim Name
newName base
  = do s <- get
       let st = stats s
           n = base ++ show (1 + (nameI st))
       put $ s { stats = st {nameI = 1 + nameI st} }
       pure n

envPush :: Name -> NameType -> Sim ()
envPush n nt = do s <- get
                  put $ s { env = (n, nt) : env s }

envEmpty :: (NameType -> Bool) -> Sim ()
envEmpty f = do s <- get
                put $ s { env = filter (not . f . snd) (env s) }

envTranslate :: Name -> Sim NameType
envTranslate n =
  do s <- get
     case lookup n (env s) of
       Just a -> pure a
       Nothing -> error $ "Name not found in env: " ++ show n

envRead :: Name -> Sim Atom
envRead n =
  do nt <- envTranslate n
     case nt of
       Local -> qRead n
       (Arg node field) -> sReadA node field

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
       pure $ fromMaybe (error $ "Accessed uninitisalised address on heap: " ++ show addr)
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
     envPush n Local
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
            envEmpty (==Local)

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

stepAccounting :: Sim ()
stepAccounting
  = do -- Update cycle count
       s <- get
       let sts = stats s
           sts' = sts {cycles = 1 + cycles sts}
       put $ s {stats = sts'}
       -- Prune env entries that reference the stack
       let stkNames = concat $ map (\(ns,_,_)->map fst ns) (stack s)
       envEmpty (argNotIn stkNames)
  where argNotIn ns (Arg n _) = not $ n `elem` ns
        argNotIn ns _         = False

statsGetCycles :: Sim Int
statsGetCycles
  = (cycles . stats) <$> get

sReadA :: Name -> Int -> Sim Atom
sReadA node field
  = do nodes <- (concat . map (\(ns,_,_) -> ns) . stack) <$> get
       -- I assume we can only inspect the top stack frame, right?
       case lookup node nodes of
         Just (Node' _ vals) -> pure (vals !! field)
         Nothing             -> error $ "Couldn't find node in top stack frame: " ++ show node

statInitial :: Stats
statInitial = Stats 0 0 0 0 0 (-1) 0

simInitial :: Program -> SimState
simInitial prog = SimState hInitial [frame] [] [] prog statInitial  -- TODO Add CAF nodes to heap
  where frame = ([("main", Node' (FTag "main") [])]
                ,[]
                ,RMain)

modeInitial :: Program -> EvalMode
modeInitial prog = let Just (IFun _ code) = lookup "main" prog
                   in EvalI code

traceState :: String -> Sim () -> Sim ()
traceState header m
  = do s <- get
       trace (header ++ show s) m

sim prog = evalState (step $ modeInitial prog) (simInitial prog)

step :: EvalMode -> Sim ()
step m = do stepAccounting
            c <- statsGetCycles
            dispatch c m
  where
    dispatch c (EvalI (Terminate t))
      = traceState (banner c "Instr [Terminator]") $ iSim (Terminate t)
    dispatch c (EvalI (i :> rest))
      = traceState (banner c ("Instr " ++ show i)) $ iSim (i :> rest)
    dispatch c EvalE
      = traceState (banner c "Stack evaluation") $ eSim
    dispatch c (EvalW x)
      = traceState (banner c "Exception unwind") $ wSim x
    banner c s = unlines [ padWith "-" 80 ""
                         , "-- " ++ padWith "-" 77 ("Cycle " ++ show c ++ " ")
                         , "-- " ++ padWith "-" 77 ("Mode: " ++ s ++ " ")
                         , " "
                         ]

iSim :: Block -> Sim ()
iSim (Store (Node tag args) :> is)
  = do vals <- traverse envRead args
       y <- hAlloc "h" (Node' tag vals)
       y' <- qPush (Ref y)
       step $ EvalI $ (is y')

iSim (PushCaf g :> is)
  = do g' <- qPush (Ref g)
       step $ EvalI $ (is g')

iSim (IPrimOp op x y :> is)
  = do x' <- envRead x
       y' <- envRead y
       z  <- qPush (doOp op x' y')
       step $ EvalI $ (is z)
  where doOp Plus (Pri a) (Pri b) = Pri (a+b)
        doOp Sub  (Pri a) (Pri b) = Pri (a-b)
        doOp LEq  (Pri a) (Pri b) = Pri $ if (a <= b) then 1 else 0
        doOp op a b = error $ "Non-exhaustive patterns in doOp: " ++ show a ++ " " ++ show op ++ " " ++ show b

iSim (Constant n :> is)
  = do c <- qPush (Pri n)
       step $ EvalI $ (is c)

iSim (ICall call cont :> is)
  = cSim call cont (RNTo is)
iSim (Force call cont :> is)
  = cSim call cont (RRTo is)

iSim (Terminate (Return (Node tag args)))
  = do vals <- traverse envRead args
       rSim (Node' tag vals)

iSim (Terminate (Jump call cont))
  = do cSim call cont RNext

iSim (Terminate (ICase call cont alts))
  = cSim call cont (RCase alts)

iSim (Terminate (IIf cmp xblock yblock))
  = do cond <- doCmp cmp
       if cond
          then step $ EvalI $ xblock
          else step $ EvalI $ yblock
  where doCmp (IntEQ  a b) = go (==) a b
        doCmp (IntLT  a b) = go (<)  a b
        doCmp (IntGT  a b) = go (>)  a b
        doCmp (IntLTE a b) = go (<=) a b
        go f a b = do a' <- envRead a
                      b' <- envRead b
                      case a' of
                        Pri a'' ->
                          case b' of
                            Pri b'' ->
                              pure $ f a'' b''
                            _ -> error $ "Cannot compare non-primitives: " ++ show a' ++ " and " ++ show b'
                        _ -> error $ "Cannot compare non-primitives: " ++ show a' ++ " and " ++ show b'

iSim (Terminate (IThrow x))
  = do val <- envRead x
       qEmpty
       step $ EvalW val

cLookup :: Name -> Sim Block
cLookup n
  = do s <- get
       case lookup n (code s) of
         Just (IFun _ is) -> pure is
         _                -> error $ "Failed to find code for function: " ++ n

cLookupArgs :: Name -> Sim [Name]
cLookupArgs n
  = do s <- get
       case lookup n (code s) of
         Just (IFun args _) -> pure args
         _                  -> error $ "Failed to find code for function: " ++ n

constructCont :: Cont -> Sim [Cont']
constructCont NOp = pure $ []
constructCont (Select n) = pure $ [Select' n]
constructCont (ICatch n) = do n' <- envRead n
                              pure $ [ICatch' n']
constructCont (Apply ns) = do ns' <- traverse envRead ns
                              pure $ [Apply' ns']

cSim :: Call -> Cont -> Return -> Sim ()
cSim (Eval x) e r
  = do e' <- constructCont e
       x' <- envRead x
       case x' of
         (Pri n) -> error $ "Tried to Eval a primitive: Eval " ++ show x
         (Ref p) -> do n  <- hLookup p
                       sPush ( [("eval", n)]
                             , e'
                             , r )
                       qEmpty
                       --qPush (Ref p) --FIXME we need this to still be called, x, right?
                       step EvalE

cSim (EvalCaf g) e r
  = do e' <- constructCont e
       n  <- hLookup g
       sPush ( [("evalcaf", n)]
             , e'
             , r )
       qEmpty
       qPush (Ref g)
       step EvalE

cSim (TLF f args) e r
  = do e' <- constructCont e
       vals <- mapM envRead args
       nodeName <- newName "fn"
       sPush ( [(nodeName, Node' (FTag f) vals)]
             , e'
             , r )
       argNames <- cLookupArgs f
       _ <- traverse (\(n,i) -> envPush n (Arg nodeName i)) (zip argNames [0..])
       is <- cLookup f
       step $ EvalI is

cSim (IFix f args) e r
  = do e' <- constructCont e
       y <- hAlloc "tmp" (Node' (CTag "tmp") [])
       vals <- mapM envRead args
       nodeName <- newName "fn"
       sPush ( [(nodeName, Node' (FTag f) vals)]
             , Update' (Ref y) : e'
             , r )
       argNames <- cLookupArgs f
       _ <- traverse (\(n,i) -> envPush n (Arg nodeName i)) (zip argNames [0..])
       is <- cLookup f
       qPush (Ref y)
       step $ EvalI is

rSim :: Node' -> Sim ()
rSim n = do sCompress n
            qEmpty
            step EvalE

eSim :: Sim ()
eSim = do s <- get
          go (heap s) (head $ stack s) (locals s)
  where
    go h ( [(_, Node' (FTag f) vals)] , c, r ) [(_,u)]
      = do _ <- sPop
           nodeName <- newName "fn"
           argNames <- cLookupArgs f
           _ <- traverse (\(n,i) -> envPush n (Arg nodeName i)) (zip argNames [0..])
           sPush ( [(nodeName, Node' (FTag f) vals)]
                 , Update' u : c
                 , r )
           qEmpty
           is <- cLookup f
           step $ EvalI is

    go h ( [(_, Node' (FTag f) vals)] , c, r ) []
      = do _ <- sPop
           nodeName <- newName "fn"
           argNames <- cLookupArgs f
           _ <- traverse (\(n,i) -> envPush n (Arg nodeName i)) (zip argNames [0..])
           sPush ( [(nodeName, Node' (FTag f) vals)]
                 , c
                 , r )
           is <- cLookup f
           step $ EvalI is

    go h ( [(name, n)] , Update' (Ref u) : c, r ) []
      = do hUpdate u n
           _ <- sPop
           sPush ( [ (name, n) ]
                 , c
                 , r )
           step EvalE

    go h ( [(name, n)] , ICatch' _ : c, r ) []
      = do _ <- sPop
           sPush ( [ (name, n) ]
                 , c
                 , r )
           step EvalE

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
                            step EvalE
      | m == length as = do _ <- sPop
                            sPush ( [(sn, Node' (FTag fn) (vals ++ as))]
                                  , c
                                  , r )
                            step EvalE
      | otherwise      = do let rem = length as - m
                            _ <- sPop
                            sPush ( [(sn, Node' (FTag fn) (vals ++ take rem as))]
                                  , Apply' (drop rem as) : c
                                  , r )
                            step EvalE

    go h ( [n] , [] , RNext ) []
      = do _ <- sPop
           (nodes, conts, ret) <- sPop
           sPush ( [n]
                 , conts
                 , ret )
           step EvalE

    go h ( [(name,n)], [] , RNTo retCode ) []
      = do _ <- sPop
           (nodes, conts, ret) <- sPop
           sPush ( (name, n) : nodes
                 , conts
                 , ret )
           step $ EvalI $ (retCode $ unconvNode n)

    go h ( [(name,n)], [] , RRTo retCode ) []
      = do _ <- sPop
           y <- hAlloc "h" n
           qPush (Ref y)
           step $ EvalI $ (retCode y)

    go h ( [(n, Node' (CTag cn) vals)] , [] , RCase alts ) []
      = do _ <- sPop
           (nodes, conts, ret) <- sPop
           nodeName <- newName "scr"
           sPush ( (nodeName, Node' (CTag cn) vals) : nodes
                 , conts
                 , ret )
           is <- selectAlt nodeName cn alts
           step $ EvalI is
       where selectAlt nodeName cn ( IAlt (Node (CTag cn') args) code : rest )
               | cn == cn' = do _ <- traverse (\(arg,i) -> envPush arg (Arg nodeName i)) (zip args [0..])
                                pure code
               | otherwise = selectAlt nodeName cn rest
    -- ^ TODO do I have to resolve project field names? These are like ARGs

    go h ( [(name, n)] , [] , RMain ) []
      = pure () -- End of program!

    go h frame locs = error $ unlines
                        ["Non-exhaustive patterns in go for frame:"
                        , show frame
                        , "And locs"
                        , show locs]

wSim :: Atom -> Sim ()
wSim x = do (nodes, conts, ret) <- sPop
            go nodes conts ret
  where go nodes (ICatch' (Ref h) : conts) ret
          = do n <- hLookup h
               sPush ( [("_",n)]
                     , Apply' [x] : conts
                     , ret )
               qPush (Ref h)
               step EvalE
        go nodes (c : conts) ret
          = do sPush ( nodes, conts, ret )
               step $ EvalW x
        go nodes [] RMain
          = pure () -- Exit with exception
        go nodes [] ret
          =  step $ EvalW x

{-

I'm not sure if node stack entries should even have a name?

Need to work out if we should look for locals in queue or as atoms from node
Maybe annotate names in InstrSet?

26/08/22

I reread the semantics! the eSim mode is actually two sets of rules which should
be _composed_. We shouldn't take separate cycles to perform the continuation
_and_ return.
-}
