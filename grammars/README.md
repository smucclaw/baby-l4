# GF grammars 

This directory contains early explorations that led to the paper [Towards CNL-based verbalization of computational contracts](https://github.com/smucclaw/complaw/tree/primary/Publications/Papers/CNL2021). Some parts were loosely based on the work of Aarne Ranta (https://github.com/GrammaticalFramework/gf-contrib/tree/master/cade-2011), but the grammars have diverged considerably.

The main CNL is in the files Predicates*.gf and ParsePredicates*.gf. The parsing, disambiguation and NLG is done in the various Haskell modules in https://github.com/smucclaw/baby-l4/tree/main/src/ToGF.

This work is currently on hold. Since autumn 2021, we have concentrated our efforts in a new task, that is, creating [Natural L4](https://github.com/smucclaw/dsl/tree/main/lib/haskell/natural4#readme). It has very different parsing and NLG challenges than what Core L4 had, so the two branches of work are currently not meeting in the middle. Ultimately, we want to provide both, for different use cases and users.

In addition, there is a third line of work, which aims to parse fully natural language. That code can be found at https://github.com/smucclaw/sandbox/tree/default/aarne#readme.




