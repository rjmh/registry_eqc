-module(registry_eqc).
-compile({parse_transform,eqc_cover}).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-compile(export_all).
-compile({no_auto_import,[unregister/1]}).

prop_version() ->
  numtests(1,
           begin
             io:format("Version ~p\n",[erlang:system_info(otp_release)]),
             true
           end).

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
  catch erlang:register(Name,Pid).

register_args(S) ->
  [name(),elements(S#state.pids)].

register_pre(S) ->
  S#state.pids /= [].

register_ok(S,[Name,Pid]) ->
  not lists:keymember(Name,1,S#state.regs) 
    andalso not lists:keymember(Pid,2,S#state.regs)
    andalso not lists:member(Pid,S#state.dead).

register_next(S,_,[Name,Pid]) ->
  case register_ok(S,[Name,Pid]) of
    true ->
      S#state{regs=S#state.regs++[{Name,Pid}]};
    false ->
      S
  end.

register_post(S,[Name,Pid],Result) ->
  (Result==true) == register_ok(S,[Name,Pid]).

register_features(S,[Name,Pid],Result) ->
  [success || Result==true] ++
    [name_already_registered || lists:keymember(Name,1,S#state.regs)] ++
    [pid_already_registered || lists:keymember(Pid,2,S#state.regs)] ++
    [pid_is_dead || lists:member(Pid,S#state.dead)].

%% whereis

whereis(Name) ->
  erlang:whereis(Name).

whereis_args(_) ->
  [name()].

whereis_post(S,[Name],Result) ->
  eq(Result,proplists:get_value(Name,S#state.regs)).

whereis_features(_,_,Result) ->
  [Result /= undefined].

%% unregister

unregister(Name) ->
  catch erlang:unregister(Name).

unregister_args(_) ->
  [name()].

unregister_ok(S,[Name]) ->
  lists:keymember(Name,1,S#state.regs).

unregister_next(S,_,[Name]) ->
  S#state{regs=lists:keydelete(Name,1,S#state.regs)}.

unregister_post(S,[Name],Res) ->
  (Res==true) == unregister_ok(S,[Name]).

unregister_features(_,_,Result) ->
  case Result of
    true ->
      [success];
    {'EXIT',_} ->
      [failure]
  end.

%% kill

kill(Pid) ->
  exit(Pid,kill),
  timer:sleep(1).

kill_args(S) ->
  [elements(S#state.pids)].

kill_pre(S) ->
  S#state.pids /= [].

kill_next(S,_,[Pid]) ->
  S#state{dead=S#state.dead++[Pid],
          regs=lists:keydelete(Pid,2,S#state.regs)}.

%% weight

weight(_,spawn)      -> 50;
weight(_,register)   -> 20;
weight(_,unregister) -> 1;
weight(_,kill)       -> 5;
weight(_,_)          -> 10.

%% property

prop_registry() ->
  eqc:numtests(1000,
  eqc_statem:show_states(
  ?FORALL(Cmds, more_commands(3,commands(?MODULE)),
          begin
            [catch unregister(N) || N <- ?names],
            {H, S, Res} = run_commands(?MODULE,Cmds),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
              aggregate(call_features(H),
                aggregate(call_features(register,H),
                  aggregate(call_features(unregister,H),
                    aggregate(call_features(whereis,H),
                      measure(registered,length(S#state.regs),
                        ?IMPLIES(Res/=precondition_failed,
                                 Res == ok)))))))
          end))).
