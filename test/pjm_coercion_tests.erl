-module(pjm_coercion_tests).
-include("./test_helpers.hrl").

-export([coerce/2]).

coerce(T, V) -> {T, V}.

binary_test_() ->
    [
     ?_assertEqual(<<"test">>, pjm_coercion:coerce(binary, <<"test">>)),
     ?_assertEqual(<<"test">>, pjm_coercion:coerce(binary, "test")),
     ?_assertEqual(<<"test">>, pjm_coercion:coerce(binary, test)),
     ?_assertEqual(<<"123">>, pjm_coercion:coerce(binary, 123)),
     ?_assertEqual(<<"1.23">>, pjm_coercion:coerce(binary, 1.23)),
     ?_assertEqual(undefined, pjm_coercion:coerce(binary, undefined)),
     ?_assertEqual(undefined, pjm_coercion:coerce(binary, null))
    ].


integer_test_() ->
    [
     ?_assertEqual(1, pjm_coercion:coerce(integer, 1)),
     ?_assertEqual(0, pjm_coercion:coerce(integer, undefined)),
     ?_assertEqual(0, pjm_coercion:coerce(integer, null)),
     ?_assertEqual(1, pjm_coercion:coerce(integer, 1.2)),
     ?_assertEqual(12, pjm_coercion:coerce(integer, <<"12">>)),
     ?_assertEqual(123, pjm_coercion:coerce(integer, "123")),
     ?_assertError(badcoersion, pjm_coercion:coerce(integer, "test"))
    ].

float_test_() ->
    [
     ?_assertEqual(1, pjm_coercion:coerce(float, 1)),
     ?_assertEqual(0, pjm_coercion:coerce(float, undefined)),
     ?_assertEqual(0, pjm_coercion:coerce(float, null)),
     ?_assertEqual(1.2, pjm_coercion:coerce(float, 1.2)),
     ?_assertEqual(1.2, pjm_coercion:coerce(float, <<"1.2">>)),
     ?_assertEqual(1.23, pjm_coercion:coerce(float, "1.23")),
     ?_assertError(badcoersion, pjm_coercion:coerce(float, "test"))
    ].

boolean_test_() ->
    [
     ?_assertEqual(true, pjm_coercion:coerce(boolean, true)),
     ?_assertEqual(false, pjm_coercion:coerce(boolean, 1)),
     ?_assertEqual(false, pjm_coercion:coerce(boolean, <<"true">>))
    ].

array_test_() ->
    [
     ?_assertEqual([1], pjm_coercion:coerce([integer], 1)),
     ?_assertEqual([], pjm_coercion:coerce([integer], null)),
     ?_assertEqual([], pjm_coercion:coerce([integer], undefined)),
     ?_assertEqual([1], pjm_coercion:coerce([integer], [1])),
     ?_assertEqual([1, 123], pjm_coercion:coerce([integer], [1, "123"]))
    ].

object_test_() ->
    [
     ?_assertEqual({[]}, pjm_coercion:coerce({integer}, null)),
     ?_assertEqual({[]}, pjm_coercion:coerce({integer}, undefined)),
     ?_assertEqual({[{foo, 1}]}, pjm_coercion:coerce({integer}, [{foo, 1}])),
     ?_assertEqual({[{foo, 1}]}, pjm_coercion:coerce({integer}, {[{foo, 1}]})),
     ?_assertEqual({[{foo, 1}, {bar, 123}]}, pjm_coercion:coerce({integer}, {[{foo, 1}, {bar, "123"}]}))
    ].


custom_coerce_test_() ->
    [
     ?_assertEqual({?MODULE, <<"test">>}, pjm_coercion:coerce(?MODULE, <<"test">>)),
     ?_assertEqual({test, <<"test">>}, pjm_coercion:coerce({?MODULE, test}, <<"test">>)),
     ?_assertError(badcoersion, pjm_coercion:coerce(unknown, <<"test">>))
    ].
