%%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for R1 %%%%%%%%%%%%%%%%%%%%%%%%%
-type r2_program() :: {r2_program, {type, r2_type()}, r2_exp()}.

-type r2_cmp() :: 'eq?' | '<=' | '>=' | '<' | '>'.

-type r2_exp() :: {int, integer()}
                | {read}
                | {minus, r2_exp()}
                | {plus, r2_exp(), r2_exp()}
                | {var, atom()}
                | {'let', [atom()], [r2_exp()], r2_exp()}
                | {true_exp}
                | {false_exp}
                | {and_exp, r2_exp(), r2_exp()}
                | {not_exp, r2_exp()}
                | {{cmp, r2_cmp()}, r2_exp(), r2_exp()}
                | {if_exp, r2_exp(), r2_exp(), r2_exp()}.

-type r2_token() :: 'program' | '(' | ')' | 'read' | '[' | ']'
                  | '+' | {integer, integer()} | {id, atom()}
                  | '#t' | '#f' | 'and' | 'not' | 'if' | 'eq?'
                  | '<' | '>' | '<=' | '>=' | 'let'.

-type r2_type() :: int | bool.

%%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for C1 %%%%%%%%%%%%%%%%%%%%%%%%%
-type c1_program() :: {c1_program,
                       {[c1_var()], {type, c1_type()}},
                       [c1_statement()]}.

-type c1_var() :: atom().

-type c1_type() :: int | bool.

-type c1_exp() :: c1_arg() | {read} | {minus, c1_arg()}
                | {plus, c1_arg(), c1_arg()} | {not_op, c1_arg()}
                | {{c1_cmp, c1_cmp()}, c1_arg(), c1_arg()}.

-type c1_statement() :: {assign, c1_var(), c1_exp()}
                      | {return, c1_arg()}
                      | {c1_if,
                         {{c1_cmp, c1_cmp()}, c1_arg(), c1_arg()},
                         [c1_statement()],
                         [c1_statement()]}.

-type c1_arg() :: {int, integer()} | c1_var() | {bool, c1_true} | {bool, c1_false}.

-type c1_cmp() :: 'eq?' | '<' | '>' | '=<' | '>='.
