-module(pjm_parse_trans_tests).
-include("../include/pjm.hrl").
-include("./test_helpers.hrl").

-pjm_stores_in(users).
-pjm_fields([
             {'Login', binary, <<"test">>}
            ]).

new_test_() ->
    {
      setup,
      fun() -> new() end,
      fun(_) -> ok end,
      fun(U) ->
              [
               ?_assertEqual({pjm_parse_trans_tests, {<<"test">>}, undefined}, U)
              ]
      end
    }.

read_field_test_() ->
    {
      setup,
      fun() -> new() end,
      fun(_) -> ok end,
      fun(U) ->
              [
               ?_assertEqual(<<"test">>, read_field('Login', undefined, U)),
               ?_assertEqual(undefined, read_field(login, undefined, U))
              ]
      end
    }.

write_field_test() ->
    U1 = write_field('Login', <<"changed">>, new()),
    ?assertEqual(<<"changed">>, read_field('Login', undefined, U1)).
write_field_coercion_test() ->
    %% test coercion
    U1 = write_field('Login', 125, new()),
    ?assertEqual(<<"125">>, read_field('Login', undefined, U1)).
write_field_unknown_field_test() ->
    U1 = write_field('Password', <<"secret">>, new()),
    ?assertEqual(<<"secret">>, read_field('Password', undefined, U1)).

get_one_test_() ->
    {
      setup,
      fun() -> new() end,
      fun(_) -> ok end,
      fun(U) ->
              [
               ?_assertEqual(<<"test">>, get_one('Login', undefined, U)),
               ?_assertEqual(undefined, get_one(login, undefined, U))
              ]
      end
    }.

set_one_test() ->
    U1 = set_one('Login', <<"changed">>, new()),
    ?assertEqual(<<"changed">>, read_field('Login', undefined, U1)).
set_one_coercion_test() ->
    %% test coercion
    U1 = set_one('Login', 125, new()),
    ?assertEqual(<<"125">>, read_field('Login', undefined, U1)).
set_one_unknown_field_test() ->
    U1 = set_one('Password', <<"secret">>, new()),
    ?assertEqual(<<"secret">>, read_field('Password', undefined, U1)).

get_test_() ->
    {
      setup,
      fun() -> new() end,
      fun(_) -> ok end,
      fun(U) ->
              [
               ?_assertEqual(<<"test">>, get('Login', undefined, U)),
               ?_assertEqual(<<"test">>, get('Login', U)),
               ?_assertEqual([<<"test">>], get(['Login'], U)),
               ?_assertEqual([<<"test">>, 1], get(['Login', {'Test', 1}], U))
              ]
      end
    }.

set_test() ->
    U1 = set('Login', <<"changed">>, new()),
    ?assertEqual(<<"changed">>, get('Login', U1)),
    U2 = set([{'Login', <<"ian">>},{'Password', <<"secret">>}], U1),
    ?assertEqual([<<"ian">>, <<"secret">>], get(['Login', 'Password'], U2)).

map_test() ->
    U1 = new([{'Test', 1}]),
    F = fun(_, _) -> <<"mapped">> end,
    ?assertEqual([{'Test', <<"mapped">>}, {'Login', <<"mapped">>}],
                 to_list(map(F, U1))).
