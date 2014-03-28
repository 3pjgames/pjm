-module(pjm_tests).
-include("./test_helpers.hrl").

is_test_() ->
    [
     ?_assert(pjm:is(pjm_parse_trans_tests, pjm_parse_trans_tests:new())),
     ?_assertNot(pjm:is(pjm_parse_trans_test, pjm_parse_trans_tests:new()))
    ].

