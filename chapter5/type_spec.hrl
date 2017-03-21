%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree of R3 %%%%%%%%%%%%%%%%%%%%%%
-type r3_program() :: {r3_program, r3_exp()}.

-type r3_cmp() :: 'eq?' | '<=' | '>=' | '<' | '>'.

-type r3_exp() :: {int, integer()}
                | {read}
                | {minus, r3_exp()}
                | {plus, r3_exp(), r3_exp()}
                | {var, atom()}
                | {'let', [atom()], [r3_exp()], r3_exp()}
                | {true_exp}
                | {false_exp}
                | {and_exp, r3_exp(), r3_exp()}
                | {not_exp, r3_exp()}
                | {{cmp, r3_cmp()}, r3_exp(), r3_exp()}
                | {if_exp, r3_exp(), r3_exp(), r3_exp()}
                | {void}
                | {tuple_exp, [r3_exp()]}
                | {vector_set_exp, r3_exp(), integer(), r3_exp()}
                | {vector_ref_exp, r3_exp(), integer()}.

-type r3_token() :: 'program' | '(' | ')' | 'read' | '[' | ']'
                  | '+' | {integer, integer()} | {id, atom()}
                  | '#t' | '#f' | 'and' | 'not' | 'if' | 'eq?'
                  | '<' | '>' | '<=' | '>=' | 'let' | 'void'
                  | 'vector' | 'vector-ref' | 'vector-set!'.

%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree of R3_ext %%%%%%%%%%%%%%%%%%%%%%
-type r3_ext_program() :: {r3_ext_program, r3_ext_exp()}.

-type r3_ext_cmp() :: 'eq?' | '<=' | '>=' | '<' | '>'.

-type r3_ext_exp() :: {int, integer()}
                    | {read}
                    | {minus, r3_ext_exp()}
                    | {plus, r3_ext_exp(), r3_ext_exp()}
                    | {var, atom()}
                    | {'let', [atom()], [r3_ext_exp()], r3_ext_exp()}
                    | {true_exp}
                    | {false_exp}
                    | {and_exp, r3_ext_exp(), r3_ext_exp()}
                    | {not_exp, r3_ext_exp()}
                    | {{cmp, r3_ext_cmp()}, r3_ext_exp(), r3_ext_exp()}
                    | {if_exp, r3_ext_exp(), r3_ext_exp(), r3_ext_exp()}
                    | {void}
                    | {vector_set_exp, r3_ext_exp(), integer(), r3_ext_exp()}
                    | {vector_ref_exp, r3_ext_exp(), integer()}
                    | {collect, integer()}
                    | {allocate, integer(), place_holder}
                    | {global_value, atom()}.

%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree of R3_tp %%%%%%%%%%%%%%%%%%%%%%
-type r3_tp_program() :: {r3_tp_program, {type, r3_tp_type()}, r3_tp_exp()}.

-type r3_tp_cmp() :: 'eq?' | '<=' | '>=' | '<' | '>'.

-type r3_tp_type() :: int | bool | {tuple, [r3_tp_type()]} | void.

-type r3_tp_exp() :: {has_type, {int, integer()}, int}
                   | {has_type, {read}, int}
                   | {has_type, {minus, r3_tp_exp()}, int}
                   | {has_type, {plus, r3_tp_exp(), r3_tp_exp()}, int}
                   | {has_type, {var, atom()}, r3_tp_type()}
                   | {has_type,
                      {'let',
                       [atom()],
                       [r3_tp_exp()],
                       r3_tp_exp()},
                      r3_tp_type()}
                   | {has_type, {true_exp}, bool}
                   | {has_type, {false_exp}, bool}
                   | {has_type, {and_exp, r3_tp_exp(), r3_tp_exp()}, bool}
                   | {has_type, {not_exp, r3_tp_exp()}, bool}
                   | {has_type,
                      {{cmp, r3_tp_cmp()},
                       r3_tp_exp(),
                       r3_tp_exp()},
                      bool}
                   | {has_type,
                      {if_exp,
                       r3_tp_exp(),
                       r3_tp_exp(),
                       r3_tp_exp()},
                      r3_tp_type()}
                   | {has_type, {void}, void}
                   | {has_type,
                      {vector_set_exp,
                       r3_tp_exp(),
                       integer(),
                       r3_tp_exp()},
                      r3_tp_type()}
                   | {has_type,
                      {vector_ref_exp,
                       r3_tp_exp(),
                       integer()},
                      r3_tp_type()}
                   | {has_type,
                      {collect, integer()},
                      void}
                   | {has_type,
                      {allocate, integer(), r3_tp_type()},
                      r3_tp_type()}
                   | {has_type,
                      {global_value, atom()},
                      r3_tp_type()}.

%%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for C2 %%%%%%%%%%%%%%%%%%%%%%%%%
-type c2_program() :: {c2_program,
                       [{c2_var(), c2_type()}],
                       {type, c2_type()},
                       [c2_statement()]}.

-type c2_var() :: atom().

-type c2_type() :: int | bool | void | {tuple, [c2_type()]}.

-type c2_exp() :: c2_arg() | {read} | {minus, c2_arg()}
                | {plus, c2_arg(), c2_arg()} | {not_op, c2_arg()}
                | {{cmp, c2_cmp()}, c2_arg(), c2_arg()}
                | {allocate, integer(), c2_type()}
                | {vector_ref, c2_arg(), integer()}
                | {vector_set, c2_arg(), integer(), c2_arg()}
                | {global_value, atom()}
                | {void}.

-type c2_statement() :: {assign, c2_var(), c2_exp()}
                      | {return, c2_arg()}
                      | {c2_if,
                         {{cmp, c2_cmp()}, c2_arg(), c2_arg()},
                         [c2_statement()],
                         [c2_statement()]}
                      | {collect, integer()}.

-type c2_arg() :: {int, integer()} | c2_var()
                | {bool, c2_true} | {bool, c2_false} | {void}.

-type c2_cmp() :: 'eq?' | '<' | '>' | '=<' | '>='.
