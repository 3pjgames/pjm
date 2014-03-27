-module(pjm_tests).
-include("./test_helpers.hrl").

is_test_() ->
    [
     ?_assert(pjm:is(user_tests, user_tests:new())),
     ?_assertNot(pjm:is(user, user_tests:new()))
    ].

