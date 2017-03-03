-module(print_x86).

-include("common.hrl").

-export([print_x86/1]).


-spec print_x86(x86_64_program()) -> string().
print_x86({program, N, Insts}) ->
    Global_line = "\t.globl main\nmain:\n",
    Build_stack_frame_lines = build_stack_frame(N),
    Inst_lines = [print_inst(Inst) || Inst <- Insts],
    Print_ans_lines = print_ans(),
    Return_lines = return(N),
    string:join([Global_line,
                 "\n",
                 Build_stack_frame_lines,
                 "\n",
                 string:join(Inst_lines, ""),
                 "\n",
                 Print_ans_lines,
                 "\n",
                 Return_lines], "").

return(N) ->
    Line1 = "\taddq\t$" ++ integer_to_list(N) ++ ", " ++ "%rsp\n",
    Line2 = "\tpopq\t%rbp\n",
    Line3 = "\tretq\n",
    string:join([Line1, Line2, Line3], "").

print_ans() ->
    Line1 = "\tmovq\t%rax, %rdi\n",
    Line2 = "\tcallq\tprint_int\n",
    string:join([Line1, Line2], "").

build_stack_frame(N) ->
    Line1 = "\tpushq\t%rbp\n",
    Line2 = "\tmovq\t%rsp, %rbp\n",
    Line3 = "\tsubq\t$" ++ integer_to_list(N) ++ ", %rsp\n",
    string:join([Line1, Line2, Line3], "").

print_inst({Inst, Arg1, Arg2}) ->
    Args = string:join([print_arg(Arg1), print_arg(Arg2)], ", "),
    Inst_string = atom_to_list(Inst),
    "\t" ++ Inst_string ++ "\t" ++ Args ++ "\n";
print_inst({Inst, Arg}) ->
    Argstring = print_arg(Arg),
    Inst_string = atom_to_list(Inst),
    "\t" ++ Inst_string ++ "\t" ++ Argstring ++ "\n";
print_inst({Inst}) ->
    Inst_string = atom_to_list(Inst),
    "\t" ++ Inst_string ++ "\n".

print_arg({int, N}) -> "$" ++ integer_to_list(N);
print_arg({register, R}) -> "%" ++ atom_to_list(R);
print_arg({deref, N, Reg}) -> integer_to_list(N) ++ "(%" ++ print_arg(Reg) ++  ")";
print_arg(V) when is_atom(V) -> atom_to_list(V).
