module Examples where

import SimpleCore
import Translate
import Pretty
import Simulate

import qualified Data.Map.Lazy as M

run = statsNormaliseCycleTypes . sim . translate . (prelude ++)
trans = putStrLn . unlines . map pretty . translate

reportPipelining prog
  = do let state = run prog
       putStrLn $ "Cycles = " ++ show (cycles . stats $ state)
       putStrLn $ "Dependencies = " ++ show (statsCollectCycleDeps  state)
       putStrLn $ "Cycle types  = " ++ show (statsCollectCycleTypes state)
       putStrLn "----------------------"

guessPerf prog stages clk bubbleMap
  = do let state = run prog
       let cycleCount = fromIntegral . cycles . stats $ state
       let cycleDeps = statsCollectCycleDeps state
       let withBubs = M.mapWithKey (\k x -> x * (1+(M.findWithDefault 0 k bubbleMap))) cycleDeps
       let pipelinedCycles = stages + (sum $ M.elems withBubs)/100.0*cycleCount
       putStrLn $ "Time = " ++ show (pipelinedCycles / clk)

-- Prelude functions for lazy evaluation.
-- Could generate more variations of these with TH.
coreAp1 = Fun "ap_1" ["a"] $
              Simple $ VAp "a" []
coreAp2 = Fun "ap_2" ["a", "b"] $
              Simple $ VAp "a" ["b"]
coreAp3 = Fun "ap_3" ["a", "b", "c"] $
              Simple $ VAp "a" ["b", "c"]
coreAp4 = Fun "ap_4" ["a", "b", "c", "d"] $
              Simple $ VAp "a" ["b", "c", "d"]

coreSel0 = Fun "sel_0" ["x"] $
              Simple $ Proj "" 0 "x"
coreSel1 = Fun "sel_1" ["x"] $
              Simple $ Proj "" 1 "x"
coreSel2 = Fun "sel_2" ["x"] $
              Simple $ Proj "" 2 "x"
coreSel3 = Fun "sel_3" ["x"] $
              Simple $ Proj "" 3 "x"
coreSel4 = Fun "sel_4" ["x"] $
              Simple $ Proj "" 4 "x"

prelude = [coreAp1, coreAp2, coreAp3, coreAp4, coreSel0, coreSel1, coreSel2, coreSel3, coreSel4]

-- Example programs written in PilGRIM Core
--
-- These are translated automatically to PilGRIM assembly with `trans`
-- or simulated with `run`

-- A set of fold functions
coreFoldr = Fun "foldr" ["f", "a", "xs"] $
              Case (SVar "xs")
                [ Alt "Nil"  [         ] $ Simple (SVar "a")
                , Alt "Cons" ["y", "ys"] $ Let (Binding "rs" $
                                                   FAp "foldr" ["f", "a", "ys"])
                                               (Simple $ VAp "f" ["y", "rs"])
                ]

coreFoldl  = Fun "foldl" ["f", "a", "xs"] $
               Case (SVar "xs")
                 [ Alt "Nil"  [         ] $ Simple (SVar "a")
                 , Alt "Cons" ["y", "ys"] $ Let (Binding "b" $
                                                  (VAp "f" ["a", "y"]))
                                                (Simple $ FAp "foldl" ["f", "b", "ys"])
                 ]

coreFoldl' = Fun "foldl'" ["f", "a", "xs"] $
               Case (SVar "xs")
                 [ Alt "Nil"  [         ] $ Simple (SVar "a")
                 , Alt "Cons" ["y", "ys"] $ LetS (Binding "b" $
                                                   (VAp "f" ["a", "y"]))
                                                 (Simple $ FAp "foldl'" ["f", "b", "ys"])
                 ]

-- An example of folding boolean `and` over a list.
-- Here we see higher-order functions but everything is symbolic (no primitives)
coreAnds = let and = Fun "and" ["x", "y"] $
                        Case (SVar "x")
                          [ Alt "False" [] $ Simple $ CAp "False" []
                          , Alt "True"  [] $ Case (SVar "y")
                            [ Alt "False" [] $ Simple $ CAp "False" []
                            , Alt "True"  [] $ Simple $ CAp "True"  []
                            ]
                          ]
               sum2 = Fun "main" [] $
                        LetS (Binding "cTrue"  $ CAp "True" []) $
                        LetS (Binding "cFalse" $ CAp "False" []) $
                        LetS (Binding "l0" $ CAp "Nil" []) $
                        LetS (Binding "l1" $ CAp "Cons" ["cTrue", "l0"]) $
                        LetS (Binding "l2" $ CAp "Cons" ["cTrue", "l1"]) $
                        Let  (Binding "f"  $ FAp "and" []) $
                        Simple $ FAp "foldl" ["f", "cTrue", "l2"]
           in [and, coreFoldl, sum2]

-- A naive example of folding primitive addition over a list. Introduces
-- primitives and their operations.
coreSum
  = let plus = Fun "plus" ["x", "y"] $ -- Indirection to saturate prim op
                 Case (SVar "x")
                   [ Alt "Int#" ["xi"] $
                       Case (SVar "y")
                         [ Alt "Int#" ["yi"] $
                             LetS (Binding "zi" $ POp Plus ["xi","yi"]) $
                             Simple $ CAp "Int#" ["zi"]
                         ]
                   ]
        sum2 = Fun "main" [] $
                 LetS (Binding "i0" $ Int 1) $
                 LetS (Binding "i1" $ Int 2) $
                 LetS (Binding "i2" $ Int 3) $
                 LetS (Binding "c0" $ CAp "Int#" ["i0"]) $
                 LetS (Binding "c1" $ CAp "Int#" ["i1"]) $
                 LetS (Binding "c2" $ CAp "Int#" ["i2"]) $
                 LetS (Binding "l0" $ CAp "Nil" []) $
                 LetS (Binding "l1" $ CAp "Cons" ["c1", "l0"]) $
                 LetS (Binding "l2" $ CAp "Cons" ["c2", "l1"]) $
                 Let (Binding "f"  $ FAp "plus" []) $
                 Simple $ FAp "foldl" ["f", "c0", "l2"]
    in [plus, coreFoldl, sum2]

-- An optimised version of `coreSum`. We specialise on plus and allow use
-- unboxed primitives where possible. Note that cases with one alt are compiled
-- away to a call, and indexing into a returned "boxed" int constructor is done
-- for free.
coreSumO2
  = let foldrSum = Fun "foldrSum" ["a", "xs"] $
                       Case (SVar "xs")
                         [ Alt "Nil"  [         ] $ Simple (CAp "Int#" ["a"])
                         , Alt "Cons" ["y", "ys"] $
                             Case (FAp "foldrSum" ["a", "ys"])
                               [ Alt "Int#" ["s"] $
                                   LetS (Binding "ans" $ POp Plus ["y", "s"]) $
                                   Simple $ CAp "Int#" ["ans"]
                               ]
                         ]
        sum2 = Fun "main" [] $
                 LetS (Binding "i0" $ Int 1) $
                 LetS (Binding "i1" $ Int 2) $
                 LetS (Binding "i2" $ Int 3) $
                 LetS (Binding "l0" $ CAp "Nil" []) $
                 LetS (Binding "l1" $ CAp "Cons" ["i1", "l0"]) $
                 LetS (Binding "l2" $ CAp "Cons" ["i2", "l1"]) $
                 Simple $ FAp "foldrSum" ["i0", "l2"]
    in [foldrSum, sum2]

-- A simple nonsense test for handling of non-strict terms.
-- Our expression (a+b) should be evaluated only once (and shared in two places)
-- The expression (c+b) should never be evaluated.
coreLazyTest
  = [Fun "plus" ["x", "y"] $ -- Indirection to saturate prim op
       Case (SVar "x")
         [ Alt "Int#" ["xi"] $
             Case (SVar "y")
               [ Alt "Int#" ["yi"] $
                   LetS (Binding "zi" $ POp Plus ["xi","yi"]) $
                   Simple $ CAp "Int#" ["zi"]
               ]
         ]
    ,Fun "foo" ["x", "y"] $
          Simple $ FAp "plus" ["x", "x"]
    ,Fun "main" [] $
          LetS (Binding "ai" $ Int 100) $
          LetS (Binding "a"  $ CAp "Int#" ["ai"]) $
          LetS (Binding "bi" $ Int 42 ) $
          LetS (Binding "b"  $ CAp "Int#" ["bi"]) $
          LetS (Binding "ci" $ Int 999) $
          LetS (Binding "c"  $ CAp "Int#" ["ci"]) $
          Let  (Binding "x"  $ FAp "plus" ["a", "b"]) $
          Let  (Binding "y"  $ FAp "plus" ["c", "b"]) $
          Simple $ FAp "foo" ["x", "y"]
    ]

-- A naive Fibonacci example. All primitives are boxed.
coreFib n
  = let fib = Fun "fib" ["n"] $
                Case (SVar "n")
                  [ Alt "Int#" ["xi"] $
                      LetS (Binding "i1" $ Int 1) $
                      LetS (Binding "i2" $ Int 2) $
                      If (IntLT "xi" "i2")
                          (Simple $ CAp "Int#" ["i1"])
                          (LetS (Binding "a" $ POp Sub ["xi", "i1"]) $
                           LetS (Binding "ac" $ CAp "Int#" ["a"]) $
                           Let  (Binding "fiba" $ FAp "fib" ["ac"]) $
                           LetS (Binding "b" $ POp Sub ["xi", "i2"]) $
                           LetS (Binding "bc" $ CAp "Int#" ["b"]) $
                           Let  (Binding "fibb" $ FAp "fib" ["bc"]) $
                           Case (SVar "fiba")
                             [ Alt "Int#" ["ai"] $
                                 Case (SVar "fibb") $
                                   [ Alt "Int#" ["bi"] $
                                       LetS (Binding "res"  $ POp Plus ["ai", "bi"]) $
                                       Simple $ CAp "Int#" ["res"]
                                   ]
                             ]
                          )
                  ]
        main = Fun "main" [] $
                 LetS (Binding "n" $ Int n) $
                 LetS (Binding "cn" $ CAp "Int#" ["n"]) $
                 Simple $ FAp "fib" ["cn"]
    in [fib, main]

-- A Fibonacci example with better primitive handling. Note that cases with one
-- alt are compiled away to a call, and indexing into a returned "boxed" int
-- constructor is done for free.
coreFibO2 n
  = let fib = Fun "fibw" ["n"] $
                LetS (Binding "i1" $ Int 1) $
                LetS (Binding "i2" $ Int 2) $
                If (IntLT "n" "i2")
                   -- Terminate
                    (Simple $ CAp "Int#" ["i1"])
                   -- Recurse
                   (
                    LetS (Binding "nm2" $ POp Sub ["n", "i2"]) $
                    Case (FAp "fibw" ["nm2"])
                    [ Alt "Int#"  ["f2"] $
                        LetS (Binding "nm1" $ POp Sub ["n", "i1"]) $
                        Case (FAp "fibw" ["nm1"])
                        [ Alt "Int#"  ["f1"] $
                            LetS (Binding "ans" $ POp Plus ["f1", "f2"]) $
                            Simple $ CAp "Int#" ["ans"]
                        ]
                    ]
                   )
        main = Fun "main" [] $
                 LetS (Binding "x" $ Int n) $
                 LetS (Binding "res" $ FAp "fibw" ["x"]) $
                 Simple $ SVar "res"
    in [fib, main]


-- OrdList benchmark from Introducing the PilGRIM/Reduceron papers.
coreOrdList depth
  = [Fun "nil" [] $
       Simple $ CAp "Nil" []

    ,Fun "true" [] $
       Simple $ CAp "True" []

    ,Fun "false" [] $
       Simple $ CAp "False" []

    ,Fun "implies" ["x", "y"] $
       Case (SVar "x")
         [ Alt "False" [] $ Simple (CAp "True" [])
         , Alt "True"  [] $ Simple (SVar "y")
         ]

    ,Fun "and" ["x", "y"] $
       Case (SVar "x")
         [ Alt "False" [] $ Simple (CAp "False" [])
         , Alt "True"  [] $ Simple (SVar "y")
         ]

    ,Fun "andList" ["xs"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (CAp "True" [])
         , Alt "Cons" ["y", "ys"] $
             Case (SVar "y")
               [ Alt "False" [] $ Simple $ CAp "False" []
               , Alt "True"  [] $ Simple $ FAp "andList" ["ys"]
               ]
         ]

    ,Fun "append" ["xs", "ys"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (SVar "ys")
         , Alt "Cons" ["z", "zs"] $
             Let (Binding "rest" $ FAp "append" ["zs", "ys"]) $
             Simple $ CAp "Cons" ["z", "rest"]
         ]

    ,Fun "map" ["f", "xs"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (CAp "Nil" [])
         , Alt "Cons" ["y", "ys"] $
             Let (Binding "head" $ VAp "f" ["y"]) $
             Let (Binding "rest" $ FAp "map" ["f", "ys"]) $
             Simple $ CAp "Cons" ["head", "rest"]
         ]

    ,Fun "ord" ["xs"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (CAp "True" [])
         , Alt "Cons" ["y", "ys"] $
             Case (SVar "ys")
               [ Alt "Nil"  [] $ Simple (CAp "True" [])
               , Alt "Cons" ["z", "zs"] $
                   LetS (Binding "a" $ FAp "implies" ["y","z"]) $
                   Let  (Binding "b" $ FAp "ord" ["ys"]) $ -- Appeal to `and` short-circuiting
                   Simple $ FAp "and" ["a", "b"]
               ]
         ]

    ,Fun "insert" ["x", "ys"] $
       Case (SVar "ys")
         [ Alt "Nil"  [] $
             LetS (Binding "nil" $ CafAp "cafNil" []) $
             Simple (CAp "Cons" ["x", "nil"])
         , Alt "Cons" ["z", "zs"] $
             Case (FAp "implies" ["x", "z"])
               [ Alt "True"  [] $
                   Simple (CAp "Cons" ["x", "ys"])
               , Alt "False" [] $
                   Let (Binding "rest" $ FAp "insert" ["x","zs"]) $
                   Simple $ CAp "Cons" ["z", "rest"]
               ]
         ]

    ,Fun "prop" ["x", "xs"] $
       LetS (Binding "inserted" $ FAp "insert" ["x", "xs"]) $
       LetS (Binding "origOrd"  $ FAp "ord" ["xs"]) $
       Let  (Binding "newOrd"   $ FAp "ord" ["inserted"]) $
       Simple $ FAp "implies" ["origOrd", "newOrd"]

    ,Fun "cons" ["x", "xs"] $
       Simple $ CAp "Cons" ["x", "xs"]

    ,Fun "boolList" ["n"] $
       LetS (Binding "i1" $ Int 1) $
       If (IntLT "n" "i1")
         ( LetS (Binding "nil" $ CafAp "cafNil" []) $
           Simple $ CAp "Cons" ["nil", "nil"]
         )
         ( LetS (Binding "m"  $ POp Sub ["n", "i1"]) $
           LetS (Binding "rec" $ FAp "boolList" ["m"]) $
           LetS (Binding "f" $ CafAp "cafFalse" []) $
           LetS (Binding "consF" $ FAp "cons" ["f"]) $
           LetS (Binding "withF" $ FAp "map" ["consF", "rec"]) $
           LetS (Binding "t" $ CafAp "cafTrue" []) $
           LetS (Binding "consT" $ FAp "cons" ["t"]) $
           LetS (Binding "withT" $ FAp "map" ["consT", "rec"]) $
           LetS (Binding "news"  $ FAp "append" ["withF", "withT"]) $
           Simple $ FAp "append" ["rec", "news"]
         )

    ,Fun "top" ["n"] $
       LetS  (Binding "lists" $ FAp "boolList" ["n"]) $
       LetS  (Binding "f" $ CafAp "cafFalse" []) $
       LetS  (Binding "propWithF" $ FAp "prop" ["f"]) $
       LetS  (Binding "resF" $ FAp "map" ["propWithF", "lists"]) $
       LetS  (Binding "t" $ CafAp "cafTrue" []) $
       LetS  (Binding "propWithT" $ FAp "prop" ["t"]) $
       LetS  (Binding "resT" $ FAp "map" ["propWithT", "lists"]) $
       LetS  (Binding "res"  $ FAp "append" ["resF", "resT"]) $
       Simple $ FAp "andList" ["res"]

     ,Fun "main" [] $
        LetS (Binding "n"  $ Int depth) $
        Simple $ FAp "top" ["n"]

    ]

-- MSS benchmark from Introducing the PilGRIM/Reduceron papers.
coreMSS n
  = [Fun "init" ["xs"] $
       Case (SVar "xs")
         [ Alt "Nil" [] $
             Simple $ CafAp "cafNil" []
         , Alt "Cons" ["y", "ys"] $
             Case (SVar "ys")
               [ Alt "Nil" [] $
                   Simple $ CafAp "cafNil" []
               , Alt "Cons" ["z", "zs"] $
                   LetS (Binding "evaldYs" $ CAp "Cons" ["z", "zs"]) $
                   Let  (Binding "initRest" $ FAp "init" ["evaldYs"]) $
                   Simple $ CAp "Cons" ["y", "initRest"]
               ]
         ]

    ,Fun "inits" ["xs"] $
       Case (SVar "xs")
         [ Alt "Nil" [] $
             LetS (Binding "nil" $ CafAp "cafNil" []) $
             Simple $ CAp "Cons" ["nil", "nil"] -- Should be a CAF but no full support yet.
         , Alt "Cons" ["y", "ys"] $
             LetS (Binding "evaldXs" $ CAp "Cons" ["y", "ys"]) $
             Let  (Binding "initXs" $ FAp "init"  ["evaldXs"]) $
             Let  (Binding "rest"   $ FAp "inits" ["initXs"]) $
             Simple $ CAp "Cons" ["evaldXs", "rest"]
         ]

    ,Fun "tails" ["xs"] $
       Case (SVar "xs")
         [ Alt "Nil" [] $
             Simple $ CafAp "cafNil" []
         , Alt "Cons" ["y", "ys"] $
             LetS (Binding "evaldXs" $ CAp "Cons" ["y", "ys"]) $
             Let  (Binding "rest"   $ FAp "tails" ["ys"]) $
             Simple $ CAp "Cons" ["evaldXs", "rest"]
         ]

    ,Fun "map" ["f", "xs"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (CafAp "Nil" [])
         , Alt "Cons" ["y", "ys"] $
             Let (Binding "head" $ VAp "f" ["y"]) $
             Let (Binding "rest" $ FAp "map" ["f", "ys"]) $
             Simple $ CAp "Cons" ["head", "rest"]
         ]

    ,Fun "append" ["xs", "ys"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (SVar "ys")
         , Alt "Cons" ["z", "zs"] $
             Let (Binding "rest" $ FAp "append" ["zs", "ys"]) $
             Simple $ CAp "Cons" ["z", "rest"]
         ]

    ,Fun "concatMap" ["f", "xs"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (CafAp "cafNil" [])
         , Alt "Cons" ["y", "ys"] $
             Let  (Binding "fy"   $ VAp "f" ["y"]) $
             Let  (Binding "rest" $ FAp "concatMap" ["f", "ys"]) $
             Simple $ FAp "append" ["fy", "rest"]
         ]

    ,Fun "segments" ["xs"] $
       Let  (Binding "initsXs" $ FAp "inits" ["xs"]) $
       Let  (Binding "f"       $ FAp "tails" []) $
       Simple $ FAp "concatMap" ["f", "initsXs"]

    ,Fun "max" ["m", "xs"] $
       Case (SVar "xs")
         [ Alt "Nil" [] $
             Simple $ CAp "Int#" ["m"]
         , Alt "Cons" ["y", "ys"] $
             If (IntLTE "m" "y")
               (Simple $ FAp "max" ["y", "ys"])
               (Simple $ FAp "max" ["m", "ys"])
         ]

    ,Fun "maximum" ["xs"] $
       Case (SVar "xs")
         [ Alt "Nil" [] $
             LetS (Binding "zero" $ Int 0) $
             Simple $ CAp "Int#" ["zero"]
         , Alt "Cons" ["y", "ys"] $
             Simple $ FAp "max" ["y", "ys"]
         ]

    ,Fun "sumAcc" ["acc", "xs"] $
       Case (SVar "xs")
         [ Alt "Nil" [] $
             Simple $ CAp "Int#" ["acc"]
         , Alt "Cons" ["y", "ys"] $
             LetS (Binding "y'" $ POp Plus ["acc", "y"]) $
             Simple $ FAp "sumAcc" ["y'", "ys"]]

    ,Fun "sum" ["xs"] $
       LetS (Binding "zero" $ Int 0) $
       Simple $ FAp "sumAcc" ["zero", "xs"]

    ,Fun "mapSum" ["xs"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (CafAp "cafNil" [])
         , Alt "Cons" ["y", "ys"] $
             Case (FAp "sum" ["y"])
               [ Alt "Int#" ["head"] $
                   Let (Binding "rest" $ FAp "mapSum" ["ys"]) $
                   Simple $ CAp "Cons" ["head", "rest"]
               ]
         ]

    ,Fun "mss" ["xs"] $
       Let (Binding "segs"  $ FAp "segments" ["xs"]) $
       Let (Binding "segss" $ FAp "mapSum" ["segs"]) $
       --Simple $ SVar "segss"
       Simple $ FAp "maximum" ["segss"]

    ,Fun "fromTo" ["n", "m"] $
       If (IntLTE "n" "m")
         (LetS (Binding "one" $ Int 1) $
          LetS (Binding "next" $ POp Plus ["one", "n"]) $
          Let  (Binding "rest" $ FAp "fromTo" ["next", "m"]) $
          Simple $ CAp "Cons" ["n", "rest"])
         (Simple $ CAp "Nil" [])

    ,Fun "main" [] $
       LetS (Binding "iLim"  $ Int   n ) $
       LetS (Binding "inLim" $ Int (-n)) $
       Let (Binding "range" $ FAp "fromTo" ["inLim", "iLim"]) $
       Simple $ FAp "mss" ["range"]
    ]

-- TODO What about the fixpoint operator?
-- TODO Add CAF support
