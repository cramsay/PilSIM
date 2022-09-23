--module Examples where

import SimpleCore
import Translate
import Pretty
import Simulate

go = putStrLn . unlines . map pretty . translate

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















coreSum2 = let plus = Fun "plus" ["x", "y"] $ -- Indirection to saturate prim op
                        LetS (Binding "sx" $ SVar "x") $ -- Indirection to force prim op to be strict in its args
                        LetS (Binding "sy" $ SVar "y") $
                        LetS (Binding "z" $ POp Plus ["sx", "sy"]) $ -- Indirection to force prim op to be translated via strict subexpr rules
                        Simple $ SVar "z" -- TODO Does this set of hacks really exist in the simple core language?
                                          -- Or should we add these as translation rules?
               sum2 = Fun "main" [] $
                        LetS (Binding "c0" $ Int 0) $
                        LetS (Binding "c1" $ Int 1) $
                        LetS (Binding "c2" $ Int 2) $
                        LetS (Binding "c3" $ Int 3) $
                        LetS (Binding "l0" $ CAp "Nil" []) $
                        LetS (Binding "l1" $ CAp "Cons" ["c1", "l0"]) $
                        LetS (Binding "l2" $ CAp "Cons" ["c2", "l1"]) $
                        LetS (Binding "l3" $ CAp "Cons" ["c3", "l2"]) $
                        LetS (Binding "f"  $ FAp "plus" []) $
                        Simple $ FAp "foldl'" ["f", "c0", "l2"]
           in [plus, coreFoldl', sum2]













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

















run = sim . translate . (prelude ++)
trans = putStrLn . unlines . map pretty . translate

coreSum2Boxed
  = let plus = Fun "plus" ["x", "y"] $ -- Indirection to saturate prim op. We pass boxed primitives
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

coreSum2Unboxed
  = let plus = Fun "plus" ["x", "y"] $
                             LetS (Binding "z" $ POp Plus ["x","y"]) $
                             Simple $ SVar "z"
        sum2 = Fun "main" [] $
                 LetS (Binding "i0" $ Int 0) $
                 LetS (Binding "i1" $ Int 1) $
                 LetS (Binding "i2" $ Int 2) $
                 LetS (Binding "i3" $ Int 3) $
                 LetS (Binding "l0" $ CAp "Nil" []) $
                 LetS (Binding "l1" $ CAp "Cons" ["i1", "l0"]) $
                 LetS (Binding "l2" $ CAp "Cons" ["i2", "l1"]) $
                 LetS (Binding "l3" $ CAp "Cons" ["i3", "l2"]) $
                 Let (Binding "f"  $ FAp "plus" []) $
                 Simple $ FAp "foldl'" ["f", "i0", "l3"]
    in [plus, coreFoldl', sum2]

coreFib20 n
  = let fib = Fun "fib" ["n"] $
                Case (SVar "n")
                  [ Alt "Int#" ["xi"] $
                      LetS (Binding "i2" $ Int 2) $
                      If (IntLT "xi" "i2") -- I think the bug is to do with this comparison!
                          (LetS (Binding "i1" $ Int 1) $
                           Simple $ CAp "Int#" ["i1"])
                          (LetS (Binding "ii1" $ Int 1) $
                           LetS (Binding "a" $ POp Sub ["xi", "ii1"]) $
                           LetS (Binding "ac" $ CAp "Int#" ["a"]) $
                           LetS (Binding "fiba" $ FAp "fib" ["ac"]) $
                           LetS (Binding "ii2" $ Int 2) $
                           LetS (Binding "b" $ POp Sub ["xi", "ii2"]) $
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

{- Looking for a more fair fib comparison with reduceron...

   Currently takes 284,000 cycles! at 100 MHz, that's nearly 3ms.

   Here's stg output with -O2

Main.$wfib [InlPrag=NOUSERINLINE[2], Occ=LoopBreaker]
  :: GHC.Prim.Int# -> GHC.Prim.Int#
[GblId, Arity=1, Caf=NoCafRefs, Str=<S,U>, Unf=OtherCon []] =
    [] \r [ww_s4cX]
        case <=# [ww_s4cX 1#] of {
          __DEFAULT ->
              case -# [ww_s4cX 1#] of sat_s4cZ [Occ=Once] {
                __DEFAULT ->
                    case Main.$wfib sat_s4cZ of ww1_s4d0 [Occ=Once] {
                      __DEFAULT ->
                          case -# [ww_s4cX 2#] of sat_s4d1 [Occ=Once] {
                            __DEFAULT ->
                                case Main.$wfib sat_s4d1 of ww2_s4d2 [Occ=Once] {
                                  __DEFAULT -> +# [ww1_s4d0 ww2_s4d2];
                                };
                          };
                    };
              };
          1# -> 1#;
        };

Main.main1 :: GHC.Base.String
[GblId] =
    [] \u []
        case Main.$wfib 20# of ww_s4d3 [Occ=Once] {
          __DEFAULT ->
              case GHC.Show.$wshowSignedInt 0# ww_s4d3 GHC.Types.[] of {
                (#,#) ww5_s4d5 [Occ=Once] ww6_s4d6 [Occ=Once] ->
                    : [ww5_s4d5 ww6_s4d6];
              };
        };

-}
coreFibO2 n
  = let fib = Fun "fibw" ["n"] $
                LetS (Binding "i2" $ Int 2) $
                If (IntLT "n" "i2")
                   -- Terminate
                   (LetS (Binding "i1" $ Int 1) $
                    Simple $ CAp "Int#" ["i1"])
                   -- Recurse
                   (
                    LetS (Binding "nm2" $ POp Sub ["n", "i2"]) $
                    Case (FAp "fibw" ["nm2"])
                    [ Alt "Int#"  ["f2"] $
                        LetS (Binding "i1" $ Int 1) $
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


coreSumO2 = let plus = Fun "plus" ["x", "y"] $
                         LetS (Binding "z" $ POp Plus ["x", "y"]) $
                         Simple $ CAp "Int#" ["z"]

                foldl  = Fun "foldl" ["f", "a", "xs"] $
                           Case (SVar "xs")
                             [ Alt "Nil"  [         ] $ Simple (CAp "Int#" ["a"])
                             , Alt "Cons" ["y", "ys"] $ Case (VAp "f" ["a", "y"])
                                                          [ Alt "Int#" ["b"] $
                                                              (Simple $ FAp "foldl" ["f", "b", "ys"])
                                                          ]
                             ]

                sum2 = Fun "main" [] $
                         LetS (Binding "c0" $ Int 0) $
                         LetS (Binding "c1" $ Int 1) $
                         LetS (Binding "c2" $ Int 2) $
                         LetS (Binding "c3" $ Int 3) $
                         LetS (Binding "l0" $ CAp "Nil" []) $
                         LetS (Binding "l1" $ CAp "Cons" ["c1", "l0"]) $
                         LetS (Binding "l2" $ CAp "Cons" ["c2", "l1"]) $
                         LetS (Binding "l3" $ CAp "Cons" ["c3", "l2"]) $
                         LetS (Binding "f"  $ FAp "plus" []) $
                         Simple $ FAp "foldl" ["f", "c0", "l3"]
            in [plus, foldl, sum2]

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
             Let (Binding "rest" $ FAp "andList" ["ys"]) $
             Simple $ FAp "and" ["y", "rest"]
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
                   Let (Binding "a" $ FAp "implies" ["y","z"]) $
                   Let (Binding "b" $ FAp "ord" ["ys"]) $
                   Simple $ FAp "and" ["a", "b"]
               ]
         ]

    ,Fun "insert" ["x", "ys"] $
       Case (SVar "ys")
         [ Alt "Nil"  [] $
             Let (Binding "nil" $ FAp "nil" []) $ -- CAF
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
       Let (Binding "inserted" $ FAp "insert" ["x", "xs"]) $
       Let (Binding "origOrd"  $ FAp "ord" ["xs"]) $
       Let (Binding "newOrd"   $ FAp "ord" ["inserted"]) $
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
           Let  (Binding "rec" $ FAp "boolList" ["m"]) $
           Let  (Binding "f" $ FAp "false" []) $ -- CAF
           Let  (Binding "consF" $ FAp "cons" ["f"]) $
           Let  (Binding "withF" $ FAp "map" ["consF", "rec"]) $
           Let  (Binding "t" $ FAp "true" []) $ -- CAF
           Let  (Binding "consT" $ FAp "cons" ["t"]) $
           Let  (Binding "withT" $ FAp "map" ["consT", "rec"]) $
           Let  (Binding "news"  $ FAp "append" ["withF", "withT"]) $
           Simple $ FAp "append" ["rec", "news"]
         )

    ,Fun "top" ["n"] $
       Let  (Binding "lists" $ FAp "boolList" ["n"]) $
       Let  (Binding "f" $ FAp "false" []) $ -- CAF
       Let  (Binding "propWithF" $ FAp "prop" ["f"]) $
       Let  (Binding "resF" $ FAp "map" ["propWithF", "lists"]) $
       Let  (Binding "t" $ FAp "true" []) $ -- CAF
       Let  (Binding "propWithT" $ FAp "prop" ["t"]) $
       Let  (Binding "resT" $ FAp "map" ["propWithT", "lists"]) $
       Let  (Binding "res"  $ FAp "append" ["resF", "resT"]) $
       Simple $ FAp "andList" ["res"]

     ,Fun "main" [] $
        LetS (Binding "n"  $ Int depth) $
        Simple $ FAp "top" ["n"]

     -- TODO What about the fixpoint operator?
     -- Add CAF support
    ]

main = print $ run (coreOrdList 4)
