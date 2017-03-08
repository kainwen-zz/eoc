-module(register_allocation).

-include("common.hrl").

-compile(export_all).


%%%%%%%%%%%%%%%%%%%liveness analysis and helper functions%%%%%%%%%%%%%%%%%%%%%%
uncover_liveness(Insts) ->
    find_lives(lists:reverse(Insts), [[]], []).

find_lives([I], Live_vars_list, Code) -> {[I|Code], Live_vars_list};
find_lives([{c1_if, {{cmp, Cmp}, Arg1, Arg2}, Insts1, Insts2}|Insts],
           Lvl=[Live_vars|_Live_vars_list],
           Code) ->
    {Code_then, Lvl_then} = find_lives(lists:reverse(Insts1) ++ [{retq}],
                                       [Live_vars],
                                       []),
    {Code_else, Lvl_else} = find_lives(lists:reverse(Insts2) ++ [{retq}],
                                       [Live_vars],
                                       []),
    New_lvs = reduce_dup(lists:append([lists:nth(1, Lvl_then),
                                       lists:nth(1, Lvl_else),
                                       find_vars_in_arg(Arg1),
                                       find_vars_in_arg(Arg2)])),
    New_code = [{c1_if,
                 {{cmp, Cmp}, Arg1, Arg2},
                 {lists:sublist(Code_then, 2, length(Code_then)), Lvl_then},
                 {lists:sublist(Code_else, 2, length(Code_else)), Lvl_else}} |
                Code],
    find_lives(Insts, [New_lvs|Lvl], New_code);
find_lives([Inst|Insts], Lvl=[Live_vars|_Live_vars_list], Code) ->
    Write_vars = get_write_var(Inst),
    Read_vars = get_read_vars(Inst),
    Sw = sets:from_list(Write_vars),
    Sr = sets:from_list(Read_vars),
    Slv = sets:from_list(Live_vars),
    S_new_live_vars = sets:union(sets:subtract(Slv, Sw), Sr),
    find_lives(Insts, [sets:to_list(S_new_live_vars)|Lvl], [Inst|Code]).

reduce_dup(L) ->
    sets:to_list(sets:from_list(L)).

find_vars_in_arg(V) when is_atom(V) -> [V];
find_vars_in_arg(_) -> [].

get_write_var({addq, _, Arg}) -> find_vars_in_arg(Arg);
get_write_var({subq, _, Arg}) ->find_vars_in_arg(Arg);
get_write_var({negq, Arg}) -> find_vars_in_arg(Arg);
get_write_var({movq, _, Arg}) -> find_vars_in_arg(Arg);
get_write_var({popq, Arg}) -> find_vars_in_arg(Arg);
get_write_var({xorq, _, Arg}) -> find_vars_in_arg(Arg);
get_write_var({movzbq, _, Arg}) -> find_vars_in_arg(Arg);
get_write_var(_) -> [].

get_read_vars({addq, Arg1, Arg2}) ->
    find_vars_in_arg(Arg1) ++ find_vars_in_arg(Arg2);
get_read_vars({subq, Arg1, Arg2}) ->
    find_vars_in_arg(Arg1) ++ find_vars_in_arg(Arg2);
get_read_vars({negq, Arg}) -> find_vars_in_arg(Arg);
get_read_vars({movq, Arg, _}) -> find_vars_in_arg(Arg);
get_read_vars({pushq, Arg}) -> find_vars_in_arg(Arg);
get_read_vars({xorq, Arg1, Arg2}) ->
    find_vars_in_arg(Arg1) ++ find_vars_in_arg(Arg2);
get_read_vars({cmpq, Arg1, Arg2}) ->
    find_vars_in_arg(Arg1) ++ find_vars_in_arg(Arg2);
get_read_vars({movzbq, Arg, _}) -> find_vars_in_arg(Arg);
get_read_vars(_) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%% build interference %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_interference(Vars, {Insts, Live_vars_list}) ->
    Inst_live_vars_pairs = lists:zip(Insts, Live_vars_list),
    {G, Reg_vars} = loop(Inst_live_vars_pairs, sets:new(), sets:new()),
    io:format("~p~n", [sets:to_list(G)]),
    io:format("-----------------------------------------~n"),
    Graph = digraph:new([cyclic, private]),
    [digraph:add_vertex(Graph, Var) || Var <- Vars],
    [add_edge(Graph, A, B) || {A, B} <- sets:to_list(G)],
    {Graph, Reg_vars, remove_live_vars(Insts)}.

remove_live_vars([]) -> [];
remove_live_vars([{c1_if, Cmp_exp, {Insts1, _}, {Insts2, _}}|Insts]) ->
    [{c1_if, Cmp_exp, Insts1, Insts2}|remove_live_vars(Insts)];
remove_live_vars([Inst|Insts]) -> [Inst|remove_live_vars(Insts)].


add_edge(G, V, D) ->
    digraph:add_edge(G, V, D),
    digraph:add_edge(G, D, V).

loop([], Graph, Reg_vars) -> {Graph, Reg_vars};
loop([{c1_if,
       {{cmp, _Cmp}, _Arg1, _Arg2},
       {Insts1, Live_vars_then},
       {Insts2, Live_vars_else}} |
      Inst_live_vars_pairs],
     Graph,
     Reg_vars) ->
    {G1, Rv1} = loop(lists:zip(Insts1, Live_vars_then), Graph, Reg_vars),
    {G2, Rv2} = loop(lists:zip(Insts2, Live_vars_else), G1, Rv1),
    loop(Inst_live_vars_pairs, G2, Rv2);
loop([{Inst, Live_vars}|Inst_live_vars_pairs], Graph, Reg_vars) ->
    case Inst of
        {movq, S, D} when is_atom(D) ->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars,
                                                      V /= D, V /= S]),
                                   Graph),
            New_reg_vars = Reg_vars;
        {addq, _S, D} when is_atom(D)->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars, V /= D]),
                                   Graph),
            New_reg_vars = Reg_vars;
        {subq, _S, D} when is_atom(D)->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars, V /= D]),
                                   Graph),
            New_reg_vars = Reg_vars;
        {callq, _L} ->
            New_graph = Graph,
            New_reg_vars = sets:union(Reg_vars, sets:from_list(Live_vars));
        {popq, D} when is_atom(D) ->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars, V /= D]),
                                   Graph),
            New_reg_vars = Reg_vars;
        {xorq, _S, D} when is_atom(D)->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars, V /= D]),
                                   Graph),
            New_reg_vars = Reg_vars;
        {movzbq, S, D} when is_atom(D) ->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars,
                                                      V /= D, V /= S]),
                                   Graph),
            New_reg_vars = Reg_vars;
        _ ->
            New_graph = Graph,
            New_reg_vars = Reg_vars
    end,
    loop(Inst_live_vars_pairs, New_graph, New_reg_vars).
