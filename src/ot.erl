-module(ot).
-export([apply/2]).

apply(State, {Type, Opts}) ->
    apply_op(Type, Opts, State).

apply_op(<<"na">>, [[], Number], N) ->
    N + Number;
apply_op(<<"na">>, [[Key|Keys], Number], State) when is_binary(Key) ->
    Value = proplists:get_value(Key, State),
    NewValue = apply_op(<<"na">>, [Keys, Number], Value),
    lists:keyreplace(Key, 1, State, {Key, NewValue});
apply_op(<<"na">>, [[Index|Keys], Number], State) when is_integer(Index) ->
    Value = lists:nth(Index + 1, State),
    NewValue = apply_op(<<"na">>, [Keys, Number], Value),
    replace(Index, State, NewValue).

replace(0, [Head|Tail], Value) ->
    [Value|Tail];
replace(Index, [Head|Tail], Value) ->
    [Head|replace(Index - 1, Tail, Value)].

