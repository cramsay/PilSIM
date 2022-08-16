module Examples where

import SimpleCore
import Translate

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
                        LetS (Binding "z" $ POp Plus ["x", "y"]) $ -- Indirection to force prim op to be strict
                        Simple $ SVar "z"
               sum2 = Fun "main" [] $
                        LetS (Binding "c0" $ Int 0) $
                        LetS (Binding "c1" $ Int 1) $
                        LetS (Binding "c2" $ Int 2) $
                        LetS (Binding "l0" $ CAp "Nil" []) $
                        LetS (Binding "l1" $ CAp "Cons" ["c1", "l0"]) $
                        LetS (Binding "l2" $ CAp "Cons" ["c2", "l1"]) $
                        LetS (Binding "f"  $ FAp "plus" []) $
                        Simple $ FAp "foldr" ["f", "c0", "l2"]
           in [plus, coreFoldr, sum2]
{-

"foldr" is

IFun ["f","a","xs"]:
  ICase (Eval "xs") NOp
    [IAlt (CTag "Nil") [] ->
       Jump (Eval "a") NOp;
    ,IAlt (CTag "Cons") ["y","ys"]
       "rs" <- Store (FTag ("foldr",3)) ["f","a","ys"];
       Jump (Eval "f") (Apply ["y","rs"]);
]

-}
