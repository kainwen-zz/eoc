-module(r1).

-include("common.hrl").

-export([eval/2, eval_exp/3, eval_script/2]).

-spec eval(r1_program(), pid()) -> integer().
eval({r1_program, Exp}, IO_dev) ->
    eval_exp(Exp, env:empty_env(), IO_dev).

-spec eval_exp(r1_exp(), env:env(), pid()) -> integer().
eval_exp({int, N}, _Env, _IO_dev) -> N;
eval_exp({read}, _Env, IO_dev) ->
    {ok, [N]} = io:fread(IO_dev, '', "~d"),
    N;
eval_exp({minus, E}, Env, IO_dev) ->
    -eval_exp(E, IO_dev, Env);
eval_exp({plus, E1, E2}, Env, IO_dev) ->
    eval_exp(E1, Env, IO_dev) + eval_exp(E2, Env, IO_dev);
eval_exp({var, Var}, Env, _IO_dev) ->
    env:apply_env(Env, Var);
eval_exp({'let', Vars, Exps, Body}, Env, IO_dev) ->
    Vals = [eval_exp(Exp, Env, IO_dev) || Exp <- Exps],
    New_env = env:extend_env(Env, lists:zip(Vars, Vals)),
    eval_exp(Body, New_env, IO_dev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
-spec eval_script(string(), string()) -> integer().
eval_script(Fn, Input) ->
    Program = r1_parse:parse_file(Fn),
    {ok, IO_dev} = file:open(Input, [read]),
    eval(Program, IO_dev).
