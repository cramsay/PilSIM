myfoldr f a [] = a
myfoldr f a (x:xs) = f x (myfoldr f a xs)

myfoldl f a [] = a
myfoldl f a (x:xs) = myfoldl f (f a x) xs

main = do xs <- read <$> getLine
          print $ myfoldr (+) (0::Integer) xs

{-

ghc -ddump-stg output includes...

==================== STG syntax: ====================
myfoldr_r1PZ :: forall t1 t2. (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
[GblId, Arity=3, Caf=NoCafRefs, Unf=OtherCon []] =
    sat-only [] \r [f_s1Qr a_s1Qs ds_s1Qt]
        case ds_s1Qt of {
          [] -> a_s1Qs;
          : x_s1Qv [Occ=Once] xs_s1Qw [Occ=Once] ->
              let {
                sat_s1Qx [Occ=Once] :: t1_a1ei
                [LclId] =
                    [f_s1Qr a_s1Qs xs_s1Qw] \u [] myfoldr_r1PZ f_s1Qr a_s1Qs xs_s1Qw;
              } in  f_s1Qr x_s1Qv sat_s1Qx;
        };

We get a fairly familiar structure but with a _lot_ of annotation information!
\u is updatable
\r is reentrant    (non-updatable)
\s is single entry (non-updatable)

"sat-only" tells us that all occurrences are saturated function calls.


Without annotations, we get something like

myfoldr = [] \r [f, a, ds] ->
  case ds of {
    [] -> a;
    (:) x xs -> let {sat = [f a xs] \u [] myfoldr f a xs;}
                in f x sat
  }

This is reasonably analogous to PilGRIM Core language. Biggest differences are
PilGRIM's lack of lambdas (everything is lifted to SCs) and handling strictness
and CAF annotations as explicit constructs rather than an annotation. Feels like
quite a small translation between the two.

This representation is, however, much more abstract that the level of PilGRIM
assembly. PilGRIM _assembly_ sits somewhere between STG and GHC's Cmm. Heap
allocations are explicit via Store/Force, explicit evaluation methods with Eval
(called by Call/Force/Jump/Case) and their set of continuations (Apply/Select).
Stack management is still left implicit.

We have code to represent PilGRIM Core, Assembly, and the translation between
the two. I should probably try to implement the semantics listed in the appendix
to get an intuition for how these assembly instructions manipulate the stacks.
I'd learn a lot and then we actually can have a fair^{ish} apples-to-apples
comparison between template instantiation and PilGRIM's assembly.

STG vs GRIN? Look very similar on the surface... GRIN has explicit heap
allocation, fetching, and updating. As a consequence, let bindings can just
become sequenced expressions doing applications and heap operations. It is also
lambda-lifted, so has only SCs. GRIN is, in essence tagged, but p31 of the
thesis claims that these "tags" could well be pointers themselves to code or
dispatch tables.

GRIN vs PilGRIM ASM? Well, although GRIN seems to use naming conventions to
differentiate between constructors, suspended function applications, and partial
functions, BUT:
  "The GRIN language itself does not make and a priori interpretation of different tags. "

PilGRIM ASM doubles down on this big time.

There is also a conspicuous lack of an Eval construct. That is because they
encode the "eval" function directly in the GRIN language itself! That's a bold
and interesting step... IMPORTANT! That is a nice step towards a very tight RISC
approach. But can we easily parallelise it on FPGA? Wait... STG doesn't have an
eval either? They jump to a code block on "entering" a closure. How is GRIN
different from this?


-}
