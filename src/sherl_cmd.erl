%% @doc POSIX shell command split, join and escape. Note that $ is escaped on
%%      split in single quotes as the shell would do it. It is not escaped
%%      on join.
-module(sherl_cmd).
-export([split/1, join/1, escape/1]).

-define(WHITESPACE(C), (C =:= $\t orelse C =:= $\s orelse
                        C =:= $\r orelse C =:= $\n)).

-define(QUOTE(C), (C =:= $' orelse C =:= $")).
-define(ESCAPE(C), (C =:= $\\)).
-define(DOLLAR(C), (C =:= $$)).

-record(state, {token=null, tokens=[], quote=null, escape=false,
                f=fun dispatch/2}).

-spec split(string()) -> [string()].
split(S) ->
    State = #state{},
    lists:reverse((emit(lists:foldl(fun next/2, State, S)))#state.tokens).

-spec join([string()]) -> string().
join(Cmds) ->
    string:join(lists:map(fun escape/1, Cmds), " ").

-spec escape(string()) -> string().
escape(S) ->
    case (S =:= [] orelse lists:any(fun should_escape/1, S)) of
        true ->
            [$" | escape_c(S)];
        false ->
            S
    end.

%% Internal API
should_escape(C) ->
    ?QUOTE(C) orelse ?ESCAPE(C) orelse ?WHITESPACE(C).

escape_c([C | S]) when C =:= $" orelse C =:= $\\ ->
    [$\\, C | escape_c(S)];
escape_c([C | S]) ->
    [C | escape_c(S)];
escape_c([]) ->
    [$"].

next(C, S=#state{f=F}) ->
    F(C, S).

emit(S=#state{escape=true}) ->
    throw({unexpected_eof, escape, S});
emit(S) when S#state.quote =/= null ->
    throw({unexpected_eof, quote, S});
emit(S=#state{token=null}) ->
    S;
emit(S=#state{token=T, tokens=L}) ->
    S#state{token=null, tokens=[lists:reverse(T) | L]}.

char(C, S=#state{token=null}) ->
    S#state{token=[C]};
char(C, S=#state{token=T}) ->
    S#state{token=[C | T]}.

ensure_token(S=#state{token=null}) ->
    S#state{token=""};
ensure_token(S) ->
    S.

whitespace(C, S) when ?WHITESPACE(C) ->
    emit(S);
whitespace(C, S) ->
    next(C, S#state{f=fun dispatch/2}).

escape(_C, S=#state{escape=false}) ->
    S#state{escape=true};
escape(C, S) when ?DOLLAR(C) ->
    (char(C, char($\\, S)))#state{escape=false, f=fun dispatch/2};
escape(C, S) ->
    (char(C, S))#state{escape=false, f=fun dispatch/2}.

quote(C, S=#state{quote=null}) ->
    ensure_token(S#state{quote=C});
quote(C, S=#state{quote=$", escape=false}) when ?ESCAPE(C) ->
    S#state{escape=true};
quote(C, S=#state{quote=$", escape=true}) when ?ESCAPE(C) orelse C =:= $" ->
    (char(C, S))#state{escape=false};
quote(C, S=#state{quote=$", escape=true}) ->
    (char(C, char($\\, S)))#state{escape=false};
quote(C, S=#state{quote=$'}) when ?DOLLAR(C) ->
    char(C, char($\\, S));
quote(C, S=#state{quote=C}) ->
    S#state{quote=null, f=fun dispatch/2};
quote(C, S) ->
    char(C, S).

dispatch(C, S) when ?WHITESPACE(C) ->
    next(C, S#state{f=fun whitespace/2});
dispatch(C, S) when ?ESCAPE(C) ->
    next(C, S#state{f=fun escape/2});
dispatch(C, S) when ?QUOTE(C) ->
    next(C, S#state{f=fun quote/2});
dispatch(C, S) ->
    char(C, S).
