# PilSIM --- An experimental simulator for the non-pipelined PilGRIM architecture

Being an experimental, cycle-accurate simulator for the non-pipelined PilGRIM
architecture, as detailed in ["Introducing the
PilGRIM"](https://research.utwente.nl/en/publications/introducing-the-pilgrim-a-processor-for-executing-lazy-functional).

The architecture is a specialised processor for the evaluation of lazy
functional languages. Although never fully implemented in its pipelined form, a
non-pipelined semantics has been documented. We implement a representation of
the PilGRIM Core language, the translation to Assembly language, and a painfully
slow simulator for PilGRIM assembly programs.

This work is experimental in the worst sense of the word.
