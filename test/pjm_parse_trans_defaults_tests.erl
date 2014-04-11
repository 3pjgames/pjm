-module(pjm_parse_trans_defaults_tests).
-include("../include/pjm.hrl").
-include("./test_helpers.hrl").

-pjm([{stores_in, users},
      {fields,
       [{created_at, timestamp, {mfa, erlang, now, []}}]}
     ]).

fun_default_test_() ->
    {
      setup,
      fun() -> new() end,
      fun(_) -> ok end,
      fun(U) ->
              Time = U:get(created_at),

              [
               ?_assert(is_tuple(Time)),
               ?_assert(element(1, Time) > 0)
              ]
      end
    }.

