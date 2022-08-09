module Examples where

import SimpleCore
import Translate

coreFoldr = Fun ("foldr", 3) ["f", "a", "xs"] $
              Case (SVar "xs")
                [ Alt "Nil"  [         ] $ Simple (SVar "a")
                , Alt "Cons" ["y", "ys"] $ Let (Binding "rs" $
                                                   FAp ("foldr",3) ["f", "a", "ys"])
                                               (Simple $ VAp "f" ["y", "rs"])
                ]

coreFoldl  = Fun ("foldl", 3) ["f", "a", "xs"] $
               Case (SVar "xs")
                 [ Alt "Nil"  [         ] $ Simple (SVar "a")
                 , Alt "Cons" ["y", "ys"] $ Let (Binding "b" $
                                                  (VAp "f" ["a", "y"]))
                                                (Simple $ FAp ("foldl",3) ["f", "b", "ys"])
                 ]

coreFoldl' = Fun ("foldl'", 3) ["f", "a", "xs"] $
               Case (SVar "xs")
                 [ Alt "Nil"  [         ] $ Simple (SVar "a")
                 , Alt "Cons" ["y", "ys"] $ LetS (Binding "b" $
                                                   (VAp "f" ["a", "y"]))
                                                 (Simple $ FAp ("foldl'",3) ["f", "b", "ys"])
                 ]

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
