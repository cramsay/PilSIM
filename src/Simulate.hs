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


maxHeapSize = 2^14

data EvalMode = EvalI Block
              | EvalE
              | EvalW Atom

data CycleDep
  = CDNothing
  | CDBranch
  | CDHazard Int -- There's a data dependency hazard with n cycles between
                 -- allocation (now) and first use point
  deriving (Show, Ord, Eq)

data CycleType
  = CTStore
  | CTPushCaf
  | CTPrimOp
  | CTConstant
  | CTCall
  | CTForce
  | CTReturn
  | CTJump
  | CTCase
  | CTIf
  | CTThrow
  | CTStkEval
  | CTStkException
  deriving (Show, Ord, Eq)

type Program = [(Name, Func)]
data SimState = SimState { heap :: Heap
                         , stack :: Stack
                         , locals :: Locals
                         , env :: Env
                         , code :: Program
                         , gcHeap :: Heap
                         , stats :: Stats }

instance Show SimState where
  show s = concat  [ "HEAP   ~> " ++ showLines ( M.toList $ heap s)
                   , "Stack  ~> " ++ showLines (stack s)
                   , "Locals ~> " ++ showLines (locals s)
                   , "Env    ~> " ++ showLines (take 10 $ env s)
                   , "Stats  ~> " ++ show (stats s)]
    where indent =   "          "
          showLines :: Show a => [a] -> String
          showLines [] = unlines [""]
          showLines (a:as) = unlines $ show a : map (\a -> indent ++ show a) as

data NameType = Local
              | Arg Int
              | Heap
  deriving (Eq, Show)

type Sim = State SimState

type StackFrame = ([(Name,Node')], [Cont'], Return)
type Stack      = [StackFrame]
type Locals     = [(Name,Atom)]
type Env        = [(Name, Name, NameType)] -- Reference name, Stack node name, reference type

type Arity = Int

data Atom = Ref Name
          | Pri Int
  deriving Show

data Node' = Node' Tag [Atom]
  deriving Show

resolveNode :: Name -> Node' -> Sim Node
resolveNode nname (Node' tag args) = do args' <- traverse toName (zip [0..] args)
                                        pure $ Node tag args'
  where toName (_,Ref n) = pure n
        toName (i,Pri p) = do n <- newName "narg"
                              envPush n nname (Arg i)
                              pure n

data Cont' = Apply' [Atom]
           | Select' Int
           | ICatch' Atom
           | Update' Atom
  deriving Show

data Return = RNTo (Node -> Block)
            | RRTo (Name -> Block)
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
                   , heapMax :: Int
                   , maxStkDepth :: Int
                   , cycles :: Int
                   , cycleTypes :: M.Map CycleType (M.Map CycleDep Float)
                   , nameI :: Int}
  deriving Show

newName :: String -> Sim Name
newName base
  = do s <- get
       let st = stats s
           n = base ++ show (1 + (nameI st))
       put $ s { stats = st {nameI = 1 + nameI st} }
       pure n

envPush :: Name -> Name -> NameType -> Sim ()
envPush n sname nt = do s <- get
                        put $ s { env = (n, sname, nt) : env s }

envPush' :: Name -> NameType -> Sim ()
envPush' n nt = do sname <- curSName
                   envPush n sname nt

-- Empty env entries of items referencing given stack nodes
-- Cleans local queue too.
envEmpty :: [Name] -> Sim ()
envEmpty snames
  = do s <- get
       let s' = s {locals = filter (\(n,_) -> not $ n `elem` oldLocals s) (locals s)}
       put $ s' { env = filter (\(_,sname,_) -> not $ sname `elem` snames) (env s') }
  where oldLocals s = map (\(n,_,_)->n) $ filter (\(n,sname,nt) -> nt==Local && (sname `elem` snames)) (env s)

envTranslate :: Name -> Sim (Name, NameType)
envTranslate n =
  do s <- get
     case lookup n (map (\(x,sname,r) -> (x,(sname,r))) $ env s) of
       Just (sname, a) -> pure (sname, a)
       Nothing         -> error $ "Name not found in env: " ++ show n

envRead :: Name -> Sim Atom
envRead n =
  do (sname, nt) <- envTranslate n
     case nt of
       Local -> qRead n
       (Arg field) -> sReadA sname field
       Heap -> pure $ Ref n -- Don't actually fetch from heap... just resolve the name to a reference

curSName :: Sim Name
curSName = do s <- stack <$> get
              let (nodes, _, _) = s!!0
              pure $ fst (nodes!!0)

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
     envPush' n Local
     return n

qRead :: Name -> Sim Atom
qRead n =
  do s <- get
     case lookup n (locals s) of
       Just a  -> pure a
       Nothing -> error $ "Local variable not found in queue: " ++ show n

sCompress :: Node' -> Sim ()
sCompress n
  = do s <- get
       let ((topNodes,topConts,topRets) : rest) = stack s
       envEmpty (map fst topNodes)
       nname <- newName "ret"
       s <- get
       put $ s { stack = ([(nname, n)],topConts,topRets) : rest }

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

stepAccounting :: EvalMode -> Sim ()
stepAccounting m
  = do -- Update cycle count
       s <- get
       if M.size (heap s) > maxHeapSize-200
         then do garbageCollect m
                 s <- get
                 let sts = stats s
                     sts' = sts { heapMax = length (heap s) }
                 put s {stats = sts'}
         else pure ()
       s <- get
       if M.size (heap s) > (maxHeapSize-200)
         then error "Heap exhausted even after GC!"
         else pure ()
       logCycleType m
       s <- get
       let sts = stats s
           sts'  = sts {cycles = 1 + cycles sts}
       put $ s {stats = sts'}

statsIncCycleType :: CycleType -> CycleDep -> Sim ()
statsIncCycleType ct cd
  = do s <- get
       let sts = stats s
           sts' = sts {cycleTypes = M.insertWith (\k a -> M.insertWith (+) cd 1 a) ct (M.singleton cd 1) (cycleTypes sts)}
       put $ s {stats = sts'}

statsIncCycleType' :: Call -> CycleType -> Sim ()
statsIncCycleType' (Eval    x) t = statsIncCycleType t CDBranch
statsIncCycleType' (EvalCaf g) t = statsIncCycleType t CDNothing
statsIncCycleType' (TLF  f as) t = statsIncCycleType t CDNothing
statsIncCycleType' (IFix f as) t = statsIncCycleType t CDNothing

-- Count instrs before next use
findUse :: Block -> CycleDep
findUse b = fromMaybe CDNothing $ fmap CDHazard (go $ markLHS b)
  where
  go b@(Store    n :> _)
    | inNode n = Just 1
    | otherwise = findNext b
  go b@(PushCaf  n :> _)
    = findNext b
  go b@(IPrimOp  o x y :> _)
    | markName == x = Just 1
    | markName == y = Just 1
    | otherwise = findNext b
  go b@(Constant i :> _)
    = findNext b
  go b@(ICall    ca co :> _) -- For all continuations, we _might_ see hazards, but
                             -- we would need to inspect what is called first...
                             -- I'm going to ignore those and assume the call
                             -- introduces sufficient latency to avoid the hazard.
    | inCall ca = Just 1
    | otherwise = Nothing  -- Again, being kind here. We don't know how long the call will be.
  go b@(Force    ca co :> _)
    | inCall ca = Just 1
    | otherwise = Nothing
  go (Terminate (Return n))
    | inNode n  = Just 1
    | otherwise = Nothing
  go (Terminate (Jump ca co))
    | inCall ca = Just 1
    | otherwise = Nothing
  go (Terminate (ICase ca co alts))
    | inCall ca = Just 1
    | otherwise = Nothing
  go (Terminate (IIf o x y))
    | inCmp o = Just 1
    | otherwise = Nothing
  go (Terminate (IThrow x))
    | markName == x = Just 1
    | otherwise = Nothing

  findNext rest = fmap (+1) (go (feedBlock rest))

  markName = "__HAZARD_DETECT__"

  markLHS :: Block -> Block
  markLHS (Store    n     :> b) = b markName
  markLHS (PushCaf  n     :> b) = b markName
  markLHS (IPrimOp  o x y :> b) = b markName
  markLHS (Constant i     :> b) = b markName
  markLHS (ICall    ca co :> b) = b (Node (CTag "") $ replicate 4 markName)
  markLHS (Force    ca co :> b) = b markName

  feedBlock :: Block -> Block
  feedBlock (Store    n     :> b) = b ""
  feedBlock (PushCaf  n     :> b) = b ""
  feedBlock (IPrimOp  o x y :> b) = b ""
  feedBlock (Constant i     :> b) = b ""
  feedBlock (ICall    ca co :> b) = b (Node (CTag "") $ replicate 4 "")

  inNode (Node _ args) = markName `elem` args

  inCmp (IntEQ x y)  = x == markName || y == markName
  inCmp (IntLT x y)  = x == markName || y == markName
  inCmp (IntGT x y)  = x == markName || y == markName
  inCmp (IntLTE x y) = x == markName || y == markName

  inCall (Eval x)    = x==markName
  inCall (EvalCaf g) = g==markName
  inCall (TLF _ as)  = markName `elem` as
  inCall (IFix _ as) = markName `elem` as

logCycleTypeB :: Block -> Sim ()
logCycleTypeB b@(Store    n     :> _) = statsIncCycleType CTStore (findUse b)
logCycleTypeB b@(PushCaf  n     :> _) = statsIncCycleType CTPushCaf (findUse b)
logCycleTypeB b@(IPrimOp  o x y :> _) = statsIncCycleType CTPrimOp (findUse b)
logCycleTypeB b@(Constant i     :> _) = statsIncCycleType CTConstant (findUse b)
logCycleTypeB b@(ICall    ca co :> _) = statsIncCycleType' ca CTCall
logCycleTypeB b@(Force    ca co :> _) = statsIncCycleType' ca CTForce
logCycleTypeB (Terminate (Return n)) = statsIncCycleType CTReturn CDBranch
logCycleTypeB (Terminate (Jump ca co)) = statsIncCycleType' ca CTJump
logCycleTypeB (Terminate (ICase ca co alts)) = statsIncCycleType' ca CTCase
logCycleTypeB (Terminate (IIf o x y)) = statsIncCycleType CTIf CDBranch
logCycleTypeB (Terminate (IThrow x)) = statsIncCycleType CTThrow CDBranch

logCycleType :: EvalMode -> Sim ()
logCycleType (EvalI b) = logCycleTypeB b
logCycleType EvalE = statsIncCycleType CTStkEval CDNothing
logCycleType (EvalW x) = statsIncCycleType CTStkException CDNothing


statsNormaliseCycleTypes :: SimState -> SimState
statsNormaliseCycleTypes s
  = let sts = stats s
        sts' = sts {cycleTypes = norm (cycleTypes sts)}
    in s {stats = sts'}
  where
    totalCs cts = sum . concat . map (map snd . M.toList . snd) $ M.toList cts
    norm cts = M.map (M.map (\v -> 100*v / totalCs cts)) cts

statsCollectCycleDeps :: SimState -> M.Map CycleDep Float
statsCollectCycleDeps s
  = let cts = (cycleTypes . stats) s
    in M.fromListWith (+) . concat . map (M.toList . snd) $ M.toList cts

statsCollectCycleTypes :: SimState -> M.Map CycleType Float
statsCollectCycleTypes s
  = let cts = (cycleTypes . stats) s
    in M.map (sum . map snd . M.toList) cts

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
statInitial = Stats 0 0 0 0 0 0 (-1) M.empty 0

simInitial :: Program -> SimState
simInitial prog = SimState hInitial [frame] [] [] prog M.empty statInitial  -- TODO Add CAF nodes to heap
  where frame = ([("main", Node' (FTag "main") [])]
                ,[]
                ,RMain)

modeInitial :: Program -> EvalMode
modeInitial prog = let Just (IFun _ code) = lookup "main" prog
                   in EvalI code

traceState :: String -> Sim () -> Sim ()
traceState header m
  = do s <- get
       --trace (header ++ show s) m
       m

sim prog = execState (step $ modeInitial prog) (simInitial prog)

step :: EvalMode -> Sim ()
step m = do stepAccounting m
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
                       nname <- newName "eval"
                       sPush ( [(nname, n)]
                             , e'
                             , r )
                       --qPush (Ref p) --FIXME we need this to still be called, x, right?
                       step EvalE

cSim (EvalCaf g) e r
  = do e' <- constructCont e
       n  <- hLookup g
       nname <- newName "evalcaf"
       sPush ( [(nname, n)]
             , e'
             , r )
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
       _ <- traverse (\(n,i) -> envPush n nodeName (Arg i)) (zip argNames [0..])
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
       _ <- traverse (\(n,i) -> envPush n nodeName (Arg i)) (zip argNames [0..])
       is <- cLookup f
       qPush (Ref y)
       step $ EvalI is

rSim :: Node' -> Sim ()
rSim n = do sCompress n
            step EvalE

eSim :: Sim ()
eSim = do s <- get
          go (heap s) (head $ stack s) (locals s)
  where
    go h ( [(sname, Node' (FTag f) vals)] , c, r ) ((_,u):qs)
      = do _ <- sPop
           nodeName <- newName "fn"
           argNames <- cLookupArgs f
           _ <- traverse (\(n,i) -> envPush n nodeName (Arg i)) (zip argNames [0..])
           sPush ( [(nodeName, Node' (FTag f) vals)]
                 , c --Update' u : c
                 , r )
           envEmpty [sname]
           is <- cLookup f
           step $ EvalI is

    go h ( [(sname, Node' (FTag f) vals)] , c, r ) _
      = do _ <- sPop
           nodeName <- newName "fn"
           argNames <- cLookupArgs f
           _ <- traverse (\(n,i) -> envPush n nodeName (Arg i)) (zip argNames [0..])
           sPush ( [(nodeName, Node' (FTag f) vals)]
                 , c
                 , r )
           envEmpty [sname]
           is <- cLookup f
           step $ EvalI is

    go h ( [(name, n)] , Update' (Ref u) : c, r ) _
      = do hUpdate u n
           _ <- sPop
           sPush ( [ (name, n) ]
                 , c
                 , r )
           thenRet

    go h ( [(name, n)] , ICatch' _ : c, r ) _
      = do _ <- sPop
           sPush ( [ (name, n) ]
                 , c
                 , r )
           thenRet

    go h ( [(sname, Node' (CTag cn) vals)] , Select' i : c, r ) _
      = do let (Ref xi) = vals !! i
           n <- hLookup xi
           _ <- sPop
           nname <- newName "select"
           sPush ( [ (nname, n) ]
                 , c
                 , r )
           envEmpty [sname]
           thenRet

    go h ( [(sn, Node' (PTag fn m) vals)] , Apply' as : c, r ) _
      | m >  length as = do _ <- sPop
                            sPush ( [(sn, Node' (PTag fn (m-length as)) (vals ++ as))]
                                  , c
                                  , r )
                            thenRet
      | m == length as = do _ <- sPop
                            sPush ( [(sn, Node' (FTag fn) (vals ++ as))]
                                  , c
                                  , r )
                            thenRet
      | otherwise      = do let rem = length as - m
                            _ <- sPop
                            sPush ( [(sn, Node' (FTag fn) (vals ++ take rem as))]
                                  , Apply' (drop rem as) : c
                                  , r )
                            thenRet

    go h frame locs = thenRet

    thenRet
      = do s <- get
           goRet (head $ stack s)

    goRet ( [n] , [] , RNext )
      = do _ <- sPop
           (nodes, conts, ret) <- sPop
           sPush ( [n]
                 , conts
                 , ret )
           envEmpty (map fst nodes)
           step EvalE

    goRet ( [(name,n)], [] , RNTo retCode )
      = do _ <- sPop
           (nodes, conts, ret) <- sPop
           sPush ( (name, n) : nodes
                 , conts
                 , ret )
           n' <- resolveNode name n
           step $ EvalI $ (retCode n')

    goRet ( [(name,n)], [] , RRTo retCode )
      = do _ <- sPop
           y <- hAlloc "h" n
           y' <- qPush (Ref y)
           sname <- curSName
           envPush y sname Heap
           envPush y' sname Local
           envEmpty [name]
           step $ EvalI $ (retCode y')

    goRet ( [(n, Node' (CTag cn) vals)] , [] , RCase alts )
      = do _ <- sPop
           (nodes, conts, ret) <- sPop
           nodeName <- newName "scr"
           sPush ( (nodeName, Node' (CTag cn) vals) : nodes
                 , conts
                 , ret )
           is <- selectAlt nodeName cn alts
           step $ EvalI is
       where selectAlt nodeName cn ( IAlt (Node (CTag cn') args) code : rest )
               | cn == cn' = do _ <- traverse (\(arg,i) -> envPush arg nodeName (Arg i)) (zip args [0..])
                                pure code
               | otherwise = selectAlt nodeName cn rest
    -- ^ TODO do I have to resolve project field names? These are like ARGs

    goRet ( [(name, n)] , [] , RMain )
      = pure () -- End of program!

wSim :: Atom -> Sim ()
wSim x = do (nodes, conts, ret) <- sPop
            go nodes conts ret
  where go nodes (ICatch' (Ref h) : conts) ret
          = do n <- hLookup h
               nname <- newName "catch"
               sPush ( [(nname,n)]
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

garbageCollect :: EvalMode -> Sim ()
garbageCollect m
  = do s <- get
       -- Collect from stack
       _ <- traverse gcCopyFrame (stack s)
       -- Collect from queue
       _ <- traverse gcCopyAtom (map snd $ locals s)
       -- Update heap
       s <- get
       put $ s { heap = gcHeap s, gcHeap = M.empty }

gcCopyNode :: Node' -> Sim ()
gcCopyNode (Node' _ args)
  = do _ <- traverse gcCopyAtom args
       pure ()

isCollected :: Name -> Sim Bool
isCollected n
  = do gcheap <- gcHeap <$> get
       pure $ isJust $ n `M.lookup` gcheap

gcCopyAtom :: Atom -> Sim ()
gcCopyAtom (Pri _) = pure ()
gcCopyAtom (Ref n)
  = do s <- get
       case lookup n (map (\(n,sn,nt)->(n,(sn,nt))) $ env s) of
         Just (_, Heap) -> copyHeapNode n
         Nothing   -> copyHeapNode n
         _         -> pure ()

  where copyHeapNode n
          = do done <- isCollected n
               s <- get
               if done
                 then pure ()
                 else case n `M.lookup` (heap s) of
                        Nothing -> pure ()
                        Just node ->
                          do s <- get
                             let s' = s { gcHeap = M.insert n node (gcHeap s) }
                             put s'
                             gcCopyNode node

gcCopyCont :: Cont' -> Sim ()
gcCopyCont (Apply' args) = do _ <- traverse gcCopyAtom args
                              pure ()
gcCopyCont (Select' _  ) = pure ()
gcCopyCont (ICatch' a  ) = gcCopyAtom a
gcCopyCont (Update' a  ) = gcCopyAtom a

gcCopyFrame :: StackFrame -> Sim ()
gcCopyFrame (ns, conts, rets)
  =  do _ <- traverse gcCopyNode (map snd ns)
        _ <- traverse gcCopyCont conts
        pure ()
