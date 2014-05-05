-module(registry_eqc).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-compile(export_all).

%% generators

-define(names,[a,b,c,d,e]).

name() ->
  elements(?names).

%% state

-record(state,{pids=[],regs=[]}).

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
    andalso not lists:keymember(Pid,2,S#state.regs).

register_next(S,_,[Name,Pid]) ->
  S#state{regs=S#state.regs++[{Name,Pid}]}.

%% property

prop_registry() ->
  ?FORALL(Cmds, commands(?MODULE),
          begin
            [catch unregister(N) || N <- ?names],
            {H, S, Res} = run_commands(?MODULE,Cmds),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
                            aggregate(command_names(Cmds),
                                      Res == ok))
          end).
