-module(ot_tests).
-include_lib("eunit/include/eunit.hrl").

na_int_test() ->
    State = [{<<"x">>, 0}],
    Op = {<<"na">>, [[<<"x">>], 1]},
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, 1}], NewState).

na_float_test() ->
    State = [{<<"x">>, 0.25}],
    Op = {<<"na">>, [[<<"x">>], 1.25]},
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, 1.50}], NewState).

na_deep_path_test() ->
    State = [{<<"x">>,
              [{<<"y">>,
                [{<<"z">>, 1}]
               }]
             }],
    Op = {<<"na">>, [[<<"x">>, <<"y">>, <<"z">>], 2]},
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>,
                   [{<<"y">>,
                     [{<<"z">>, 3}]
                    }]
                  }], NewState).

na_multiple_keys_state_test() ->
    State = [{<<"x">>, 0}, {<<"y">>, 2}],
    Op = {<<"na">>, [[<<"y">>], 2]},
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, 0}, {<<"y">>, 4}], NewState).

na_complex_state_test() ->
    State = [{<<"x">>, [1, 2, [{<<"z">>, 1}]]},
             {<<"y">>, 3}],
    Op = {<<"na">>, [[<<"x">>, 2, <<"z">>], 2]},
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, [1, 2, [{<<"z">>, 3}]]},
                  {<<"y">>, 3}], NewState).

si_append_test() ->
    State = [{<<"x">>, <<"abc">>}],
    Op = {<<"si">>, [[<<"x">>], <<"def">>, 3]},
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, <<"abcdef">>}], NewState).

si_prepend_test() ->
    State = [{<<"x">>, <<"abc">>}],
    Op = {<<"si">>, [[<<"x">>], <<"def">>, 0]},
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, <<"defabc">>}], NewState).

