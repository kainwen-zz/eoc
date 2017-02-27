File Description
================

* `common.hrl`: ast type defines
* `uniquify.erl`: rename variables in the ast of R1
* `name_server`: used in `uniquify.erl` to generate new variable names
* `senv.erl`: used in `uniquify` to staticly emulator environment
* `r1.erl` and `env.erl`: a r1 interperter for R1 langauge
* `r1_tok.erl` and `r1_parse.erl`: the compiler frontend of R1
* `ast_to_src.erl`: generate source code from ast
* `random_ast`: generator random programs
* `tests.py`: python program to test
