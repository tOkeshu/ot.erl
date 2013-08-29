-module(ot_tests).
-include_lib("eunit/include/eunit.hrl").

na_int_test() ->
    State = [{<<"x">>, 0}],
    Op = [<<"na">>, [<<"x">>], [1]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, 1}], NewState).

na_float_test() ->
    State = [{<<"x">>, 0.25}],
    Op = [<<"na">>, [<<"x">>], [1.25]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, 1.50}], NewState).

na_deep_path_test() ->
    State = [{<<"x">>,
              [{<<"y">>,
                [{<<"z">>, 1}]
               }]
             }],
    Op = [<<"na">>, [<<"x">>, <<"y">>, <<"z">>], [2]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>,
                   [{<<"y">>,
                     [{<<"z">>, 3}]
                    }]
                  }], NewState).

na_multiple_keys_state_test() ->
    State = [{<<"x">>, 0}, {<<"y">>, 2}],
    Op = [<<"na">>, [<<"y">>], [2]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, 0}, {<<"y">>, 4}], NewState).

na_complex_state_test() ->
    State = [{<<"x">>, [1, 2, [{<<"z">>, 1}]]},
             {<<"y">>, 3}],
    Op = [<<"na">>, [<<"x">>, 2, <<"z">>], [2]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, [1, 2, [{<<"z">>, 3}]]},
                  {<<"y">>, 3}], NewState).

si_append_test() ->
    State = [{<<"x">>, <<"abc">>}],
    Op = [<<"si">>, [<<"x">>], [<<"def">>, 3]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, <<"abcdef">>}], NewState).

si_prepend_test() ->
    State = [{<<"x">>, <<"abc">>}],
    Op = [<<"si">>, [<<"x">>], [<<"def">>, 0]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, <<"defabc">>}], NewState).

sd_prepend_test() ->
    State = [{<<"x">>, <<"abc">>}],
    Op = [<<"sd">>, [<<"x">>], [<<"ab">>, 0]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, <<"c">>}], NewState).

sd_append_test() ->
    State = [{<<"x">>, <<"abc">>}],
    Op = [<<"sd">>, [<<"x">>], [<<"bc">>, 1]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, <<"a">>}], NewState).

sr_same_size_test() ->
    State = [{<<"x">>, <<"abcde">>}],
    Op = [<<"sr">>, [<<"x">>], [<<"bcd">>, <<"xxx">>, 1]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, <<"axxxe">>}], NewState).

sr_bigger_size_test() ->
    State = [{<<"x">>, <<"abcde">>}],
    Op = [<<"sr">>, [<<"x">>], [<<"bcd">>, <<"sooobiiig">>, 1]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, <<"asooobiiige">>}], NewState).

sr_smaller_size_test() ->
    State = [{<<"x">>, <<"abcde">>}],
    Op = [<<"sr">>, [<<"x">>], [<<"bcd">>, <<"t">>, 1]],
    NewState = ot:apply(State, Op),
    ?assertEqual([{<<"x">>, <<"ate">>}], NewState).

si_transform_si_test() ->
    Op1 = [<<"si">>, [<<"x">>], [<<"abc">>, 0]],
    Op2 = [<<"si">>, [<<"x">>], [<<"def">>, 0]],
    NewOp2 = ot:transform(Op2, Op1),
    ?assertEqual([<<"si">>, [<<"x">>], [<<"def">>, 3]], NewOp2).

si_transform_sd_test() ->
    Op1 = [<<"si">>, [<<"x">>], [<<"abc">>, 0]],
    Op2 = [<<"sd">>, [<<"x">>], [<<"def">>, 0]],
    NewOp2 = ot:transform(Op2, Op1),
    ?assertEqual([<<"sd">>, [<<"x">>], [<<"def">>, 3]], NewOp2).

si_transform_noop_test() ->
    Op1 = [<<"si">>, [<<"x">>], [<<"abc">>, 3]],
    Op2 = [<<"si">>, [<<"x">>], [<<"def">>, 0]],
    NewOp2 = ot:transform(Op2, Op1),
    ?assertEqual([<<"si">>, [<<"x">>], [<<"def">>, 0]], NewOp2).

sd_transform_sd_test() ->
    Op1 = [<<"sd">>, [<<"x">>], [<<"abc">>, 0]],
    Op2 = [<<"sd">>, [<<"x">>], [<<"def">>, 3]],
    NewOp2 = ot:transform(Op2, Op1),
    ?assertEqual([<<"sd">>, [<<"x">>], [<<"def">>, 0]], NewOp2).

sd_transform_si_test() ->
    Op1 = [<<"sd">>, [<<"x">>], [<<"abc">>, 0]],
    Op2 = [<<"si">>, [<<"x">>], [<<"def">>, 3]],
    NewOp2 = ot:transform(Op2, Op1),
    ?assertEqual([<<"si">>, [<<"x">>], [<<"def">>, 0]], NewOp2).

sd_transform_noop_test() ->
    Op1 = [<<"sd">>, [<<"x">>], [<<"abc">>, 3]],
    Op2 = [<<"si">>, [<<"x">>], [<<"def">>, 0]],
    NewOp2 = ot:transform(Op2, Op1),
    ?assertEqual([<<"si">>, [<<"x">>], [<<"def">>, 0]], NewOp2).

transform_multiple_test() ->
    Op1 = [<<"si">>, [<<"x">>], [<<"x">>, 0]],
    Op2 = [<<"si">>, [<<"x">>], [<<"a">>, 0]],
    Op3 = [<<"si">>, [<<"x">>], [<<"b">>, 1]],
    Op4 = [<<"si">>, [<<"x">>], [<<"c">>, 2]],
    NewOp1 = ot:transform(Op1, [Op2, Op3, Op4]),
    ?assertEqual([<<"si">>, [<<"x">>], [<<"x">>, 3]], NewOp1).

transform_noop_test() ->
    Op1 = [<<"si">>, [<<"x">>], [<<"x">>, 0]],
    Op2 = ot:transform(Op1, []),
    ?assertEqual(Op1, Op2).

