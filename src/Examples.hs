module Examples where

import SimpleCore
import Translate
import Pretty
import Simulate

run = statsNormaliseCycleTypes . sim . translate . (prelude ++)

trans = putStrLn . unlines . map pretty . translate

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
                        LetS (Binding "f"  $ FAp "and" []) $
                        Simple $ FAp "foldr" ["f", "cTrue", "l2"]
           in [and, coreFoldr, sum2]

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
                           LetS (Binding "fiba" $ FAp "fib" ["ac"]) $
                           LetS (Binding "b" $ POp Sub ["xi", "i2"]) $
                           LetS (Binding "bc" $ CAp "Int#" ["b"]) $
                           LetS (Binding "fibb" $ FAp "fib" ["bc"]) $
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
         [ Alt "False"  [] $ Simple (CAp "True" [])
         , Alt "True" [] $ Simple (SVar "y")
         ]

    ,Fun "and" ["x", "y"] $
       Case (SVar "x")
         [ Alt "False"  [] $ Simple (CAp "False" [])
         , Alt "True" [] $ Simple (SVar "y")
         ]

    ,Fun "andList" ["xs"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (CAp "True" [])
         , Alt "Cons" ["y", "ys"] $
             LetS (Binding "rest" $ FAp "andList" ["ys"]) $
             Simple $ FAp "and" ["y", "rest"]
         ]

    ,Fun "append" ["xs", "ys"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (SVar "ys")
         , Alt "Cons" ["z", "zs"] $
             LetS (Binding "rest" $ FAp "append" ["zs", "ys"]) $
             Simple $ CAp "Cons" ["z", "rest"]
         ]

    ,Fun "map" ["f", "xs"] $
       Case (SVar "xs")
         [ Alt "Nil"  [] $ Simple (CAp "Nil" [])
         , Alt "Cons" ["y", "ys"] $
             LetS (Binding "head" $ VAp "f" ["y"]) $
             LetS (Binding "rest" $ FAp "map" ["f", "ys"]) $
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
                   Let  (Binding "b" $ FAp "ord" ["ys"]) $
                   Simple $ FAp "and" ["a", "b"]
               ]
         ]

    ,Fun "insert" ["x", "ys"] $
       Case (SVar "ys")
         [ Alt "Nil"  [] $
             LetS (Binding "nil" $ FAp "nil" []) $ -- CAF
             Simple (CAp "Cons" ["x", "nil"])
         , Alt "Cons" ["z", "zs"] $
             Case (FAp "implies" ["x", "z"])
               [ Alt "True"  [] $
                   Simple (CAp "Cons" ["x", "ys"])
               , Alt "False" [] $
                   LetS (Binding "rest" $ FAp "insert" ["x","zs"]) $
                   Simple $ CAp "Cons" ["z", "rest"]
               ]
         ]

    ,Fun "prop" ["x", "xs"] $
       LetS (Binding "inserted" $ FAp "insert" ["x", "xs"]) $
       LetS (Binding "origOrd"  $ FAp "ord" ["xs"]) $
       LetS (Binding "newOrd"   $ FAp "ord" ["inserted"]) $
       Simple $ FAp "implies" ["origOrd", "newOrd"]

    ,Fun "cons" ["x", "xs"] $
       Simple $ CAp "Cons" ["x", "xs"]

    ,Fun "boolList" ["n"] $
       LetS (Binding "i1" $ Int 1) $
       If (IntLT "n" "i1")
         ( Let (Binding "nil" $ FAp "nil" []) $ -- CAF
           Simple $ CAp "Cons" ["nil", "nil"]
         )
         ( LetS (Binding "m"  $ POp Sub ["n", "i1"]) $
           LetS  (Binding "rec" $ FAp "boolList" ["m"]) $
           LetS  (Binding "f" $ FAp "false" []) $ -- CAF
           LetS  (Binding "consF" $ FAp "cons" ["f"]) $
           LetS  (Binding "withF" $ FAp "map" ["consF", "rec"]) $
           LetS  (Binding "t" $ FAp "true" []) $ -- CAF
           LetS  (Binding "consT" $ FAp "cons" ["t"]) $
           LetS  (Binding "withT" $ FAp "map" ["consT", "rec"]) $
           LetS (Binding "news"  $ FAp "append" ["withF", "withT"]) $
           Simple $ FAp "append" ["rec", "news"]
         )

    ,Fun "top" ["n"] $
       LetS  (Binding "lists" $ FAp "boolList" ["n"]) $
       LetS  (Binding "f" $ FAp "false" []) $ -- CAF
       LetS  (Binding "propWithF" $ FAp "prop" ["f"]) $
       LetS  (Binding "resF" $ FAp "map" ["propWithF", "lists"]) $
       LetS  (Binding "t" $ FAp "true" []) $ -- CAF
       LetS  (Binding "propWithT" $ FAp "prop" ["t"]) $
       LetS  (Binding "resT" $ FAp "map" ["propWithT", "lists"]) $
       LetS  (Binding "res"  $ FAp "append" ["resF", "resT"]) $
       Simple $ FAp "andList" ["res"]

     ,Fun "main" [] $
        LetS (Binding "n"  $ Int depth) $
        Simple $ FAp "top" ["n"]

    ]

-- TODO What about the fixpoint operator?
-- TODO Add CAF support