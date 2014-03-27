-module(pjm_parse_trans_more_tests).
-include("../include/pjm.hrl").
-include("./test_helpers.hrl").

-pjm_stores_in(users).
-pjm_fields([
             {'Login', binary, <<"test">>}
            ]).

get_one('LoginLength', _, Model) ->
    case read_field('Login', undefined, Model) of
        undefined -> 0;
        Login -> length(unicode:characters_to_list(Login))
    end;
get_one(K, D, M) ->
    read_field(K, D, M).

set_one('Password', Value, Model) ->
    write_field('PasswordDigest', <<"x", Value/binary>>, Model);
set_one(K, V, M) ->
    write_field(K, V, M).

get_one_test_() ->
    {
      setup,
      fun() -> new() end,
      fun(_) -> ok end,
      fun(U) ->
              [
               ?_assertEqual(4, get_one('LoginLength', undefined, U))
              ]
      end
    }.

set_one_test() ->
    U1 = set_one('Login', <<"changed">>, new()),
    ?assertEqual(<<"changed">>, read_field('Login', undefined, U1)).
set_one_hook_test() ->
    U1 = set_one('Password', <<"secret">>, new()),
    ?assertEqual(<<"xsecret">>, read_field('PasswordDigest', undefined, U1)).
