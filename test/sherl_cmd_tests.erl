-module(sherl_cmd_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_join_split() ->
    ?FORALL(Cmd, [string()],
            begin
                io:format("Cmd = ~p~n", [Cmd]),
                R = (sherl_cmd:split(sherl_cmd:join(Cmd)) =:= Cmd),
                io:format("R = ~p~n", [R]),
                R
            end).

proper_test_() ->
    [{atom_to_list(F),
      fun () ->
               ?assert(proper:quickcheck(?MODULE:F(), [long_result])) end}
     || {F, 0} <- ?MODULE:module_info(exports),
        F >= 'prop_',
        F < 'prop`'].
