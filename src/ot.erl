%% This is an implementation of the real-time algorithm [Operational
%% Transformation][ot] (or OT) written in Erlang. OT is an optimistic
%% concurrency algorithm that lets clients apply operations to a document
%% immediately as they are created, and only synchronizes the changes with the
%% server after they have been made. If another client has altered the document
%% while the first client was making the first operation, we can transform each
%% operation so that the server and every client will converge to the same
%% document state.
%%
%% [ot]: http://en.wikipedia.org/wiki/Operational_transformation

-module(ot).
-export([apply/2, transform/2]).

%%
apply(State, {Type, Opts}) ->
    apply_op(Type, Opts, State).

%% # Operations

%% ## Numbers

%% **na**: adds a value to a number.
%%
%% `{<<"na">>, [Path, Number]}`
%%
%% Where `Number` is the value to add to the number at `Path`.
%%
apply_op(<<"na">>, [[], Number], N) ->
    N + Number;

%% ## Strings

%% **si**: inserts a string.
%%
%% `{<<"si">>, [Path, String, Offset]}`
%%
%% Where `String` is the text to insert at the index `Offset` in the string
%% located at `Path`.
%%
apply_op(<<"si">>, [[], String, Offset], State) ->
    <<Prefix:Offset/binary, Suffix/binary>> = State,
    <<Prefix/binary, String/binary, Suffix/binary>>;

%% **sd**: deletes a string.
%%
%% `{<<"sd">>, [Path, String, Offset]}`
%%
%% Where `String` is the text to delete, starting at the index `Offset`, in the
%% string located at `Path`.
%%
apply_op(<<"sd">>, [[], String, Offset], State) ->
    Size = size(String),
    <<Prefix:Offset/binary, String:Size/binary, Suffix/binary>> = State,
    <<Prefix/binary, Suffix/binary>>;

%% **sr**: replaces a string.
%%
%% `{<<"sr">>, [Path, OldString, NewString, Offset]}`
%%
%% Where `NewString` is the text replacing `OldString`, starting at `Offset`,
%% in the string located at `Path`.
%%
apply_op(<<"sr">>, [[], OldString, NewString, Offset], State) ->
    Size = size(OldString),
    <<Prefix:Offset/binary, OldString:Size/binary, Suffix/binary>> = State,
    <<Prefix/binary, NewString/binary, Suffix/binary>>;

%% ## Path and State
%%
%% A state is an erlang structure representing a document. It's close to the
%% erlang representation of JSON structures in the [jsx project][jsx].
%%
%% [jsx]: https://github.com/talentdeficit/jsx#json---erlang-mapping
%%
%% In other words a state should respect the following mapping:
%%
%% **number**: `integer()` or `float()`
%%
%% **string**: `binary()`
%%
%% **true, false and null**: `true`, `false` and `null`
%%
%% **array**: lists of any type above and/or objects
%%
%% **object**: objects are represented by erlang proplists. The empty object
%% has the special representation `[{}]` to differentiate it from the empty
%% list. The shorthand form of proplists are **prohibited** and the keys
%% **must** be encoded as binaries.
%%
%% A path is a list of keys and/or indexes describing the location of an object
%% inside a state. Keys traverse objects while indexes traverse arrays.
%%
%% Example:
%%
%% The path `[<<"x">>, 2, <<"z">>]` would points to `<<"value">>` in the
%% following state:
%%
%% `[{<<"x">>, [1, 2, [{<<"z">>, <<"value">>}]]}]`
%%
%% The equivalent JSON structure of this state would be:
%%
%% `{"x": [1, 2, {"z": "value"}]}`
%%
%%
apply_op(Type, [[Key|Keys]|Opts], State) when is_binary(Key) ->
    Value = proplists:get_value(Key, State),
    NewValue = apply_op(Type, [Keys|Opts], Value),
    lists:keyreplace(Key, 1, State, {Key, NewValue});
apply_op(Type, [[Index|Keys]|Opts], State) when is_integer(Index) ->
    Value = lists:nth(Index + 1, State),
    NewValue = apply_op(Type, [Keys|Opts], Value),
    replace(Index, State, NewValue).

%% # Transformations
transform(OpToTr, Ops) when is_list(Ops) ->
    lists:foldl(fun(Op1, Op2) -> transform(Op2, Op1) end, OpToTr, Ops);
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

