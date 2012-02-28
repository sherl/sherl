-module(sherl_cmd_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_join_split_string() ->
    ?FORALL(Cmd, [string()],
            (sherl_cmd:split(sherl_cmd:join(Cmd)) =:= Cmd)).

prop_join_split_ascii() ->
    ?FORALL(Cmd, [[integer(1, 126)]],
            (sherl_cmd:split(sherl_cmd:join(Cmd)) =:= Cmd)).

state_char() ->
    state_char([]).

state_char(Extra) ->
    union(Extra ++ [$\s, $\t, $\r, $\n, $z, $Z, $$]).

escape() ->
    escape([state_char(), $']).

escape(Extra) ->
    ?LET(X, union(Extra ++ [$\\, $"]), [$\\, X]).

dquote() ->
    ?LET(S, [union([state_chars([$']), escape()])],
         ("\"" ++ S ++ "\"")).

squote() ->
    ?LET(S, state_chars([ $\\ ]),
         ("'" ++ S ++ "'")).

state_chars() ->
    state_chars([]).

state_chars(Extra) ->
    ?LET(S, [state_char(Extra)], S).

state_chunk() ->
    union([state_chars(), escape(), squote(), dquote()]).

state_string_int() ->
    ?LAZY(
       frequency([{1, []},
                  {10, [state_chunk() | state_string_int()]}])).

state_string() ->
    ?LET(S, state_string_int(), lists:flatten(S)).

prop_split_valid() ->
    ?FORALL(Cmd, state_string(),
            begin
                Split = sherl_cmd:split(Cmd),
                Split =:= sherl_cmd:split(sherl_cmd:join(Split))
            end).

proper_test_() ->
    [{atom_to_list(F),
      fun () ->
               ?assert(proper:quickcheck(?MODULE:F(), [long_result])) end}
     || {F, 0} <- ?MODULE:module_info(exports),
        F >= 'prop_',
        F < 'prop`'].
