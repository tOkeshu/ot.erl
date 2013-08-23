-module(ot).
-export([apply/2, transform/2]).

apply(State, {Type, Opts}) ->
    apply_op(Type, Opts, State).

apply_op(<<"na">>, [[], Number], N) ->
    N + Number;
apply_op(<<"si">>, [[], String, Offset], State) ->
    <<Prefix:Offset/binary, Suffix/binary>> = State,
    <<Prefix/binary, String/binary, Suffix/binary>>;
apply_op(<<"sd">>, [[], String, Offset], State) ->
    Size = size(String),
    <<Prefix:Offset/binary, String:Size/binary, Suffix/binary>> = State,
    <<Prefix/binary, Suffix/binary>>;
apply_op(Type, [[Key|Keys]|Opts], State) when is_binary(Key) ->
    Value = proplists:get_value(Key, State),
    NewValue = apply_op(Type, [Keys|Opts], Value),
    lists:keyreplace(Key, 1, State, {Key, NewValue});
apply_op(Type, [[Index|Keys]|Opts], State) when is_integer(Index) ->
    Value = lists:nth(Index + 1, State),
    NewValue = apply_op(Type, [Keys|Opts], Value),
    replace(Index, State, NewValue).

transform({<<"si">>, _Opts} = Op2, Op1) ->
    transform_s(Op2, Op1);
transform({<<"sd">>, _Opts} = Op2, Op1) ->
    transform_s(Op2, Op1).
transform_s({Type2, [Path|Opts2]}, {Type1, [Path|Opts1]})  ->
    [String1, Offset1] = Opts1,
    [String2, Offset2] = Opts2,
    NewOpts =
        if
            Offset1 =< Offset2, Type1 =:= <<"si">> ->
                [String2, Offset2 + size(String1)];
            Offset1 =< Offset2, Type1 =:= <<"sd">> ->
                [String2, Offset2 - size(String1)];
            true -> % noop
                [String2, Offset2]
        end,
    {Type2, [Path|NewOpts]}.

replace(0, [_Head|Tail], Value) ->
    [Value|Tail];
replace(Index, [Head|Tail], Value) ->
    [Head|replace(Index - 1, Tail, Value)].

