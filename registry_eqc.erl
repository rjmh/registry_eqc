-module(registry_eqc).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-compile(export_all).

%% property

prop_registry() ->
  ?FORALL(Cmds, commands(?MODULE),
          begin
            {H, S, Res} = run_commands(?MODULE,Cmds),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
                            aggregate(command_names(Cmds),
                                      Res == ok))
          end).
