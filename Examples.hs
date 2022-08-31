module Examples where

import SimpleCore
import Translate
import Pretty
import Simulate

go = putStrLn . unlines . map pretty . translate

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

















run = sim . translate
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
                 LetS (Binding "f"  $ FAp "plus" []) $
                 Simple $ FAp "foldr" ["f", "c0", "l2"]
    in [plus, coreFoldr, sum2]

coreFib20
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
                 LetS (Binding "n" $ Int 20) $
                 LetS (Binding "cn" $ CAp "Int#" ["n"]) $
                 Simple $ FAp "fib" ["cn"]
    in [fib, main]
