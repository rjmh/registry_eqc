-module(registry_eqc).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-compile(export_all).
-compile({no_auto_import,[unregister/1]}).

%% generators

-define(names,[a,b,c,d,e]).

name() ->
  elements(?names).

%% state

-record(state,{pids=[],regs=[],dead=[]}).

initial_state() ->
  #state{}.

%% spawn

spawn() ->
  erlang:spawn(fun() ->
                   timer:sleep(5000)
               end).

spawn_args(_) ->
  [].

spawn_next(S,Pid,[]) ->
  S#state{pids=S#state.pids++[Pid]}.

%% register

register(Name,Pid) ->
  erlang:register(Name,Pid).

register_args(S) ->
  [name(),elements(S#state.pids)].

register_pre(S) ->
  S#state.pids /= [].

register_pre(S,[Name,Pid]) ->
  not lists:keymember(Name,1,S#state.regs) 
    andalso not lists:keymember(Pid,2,S#state.regs)
    andalso not lists:member(Pid,S#state.dead).

register_next(S,_,[Name,Pid]) ->
  S#state{regs=S#state.regs++[{Name,Pid}]}.

%% whereis

whereis(Name) ->
  erlang:whereis(Name).

whereis_args(_) ->
  [name()].

whereis_post(S,[Name],Result) ->
  eq(Result,proplists:get_value(Name,S#state.regs)).

%% unregister

unregister(Name) ->
  erlang:unregister(Name).

unregister_args(_) ->
  [name()].

unregister_pre(S,[Name]) ->
  lists:keymember(Name,1,S#state.regs).

unregister_next(S,_,[Name]) ->
  S#state{regs=lists:keydelete(Name,1,S#state.regs)}.

%% kill

kill(Pid) ->
  exit(Pid,kill),
  timer:sleep(1).

kill_args(S) ->
  [elements(S#state.pids)].

kill_pre(S) ->
  S#state.pids /= [].

kill_next(S,_,[Pid]) ->
  S#state{dead=S#state.dead++[Pid]}.

%% property

prop_registry() ->
  eqc_statem:show_states(
  ?FORALL(Cmds, commands(?MODULE),
          begin
            [catch unregister(N) || N <- ?names],
            {H, S, Res} = run_commands(?MODULE,Cmds),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
                            aggregate(command_names(Cmds),
                                      ?IMPLIES(Res/=precondition_failed,
                                               Res == ok)))
          end)).
