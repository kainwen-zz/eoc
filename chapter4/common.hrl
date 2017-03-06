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
