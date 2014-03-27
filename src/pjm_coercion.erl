-module(pjm_coercion).
-export([coerce/2]).

%% Coerce Value to proper value of type Type.
-spec coerce(term(), term()) -> term().
coerce(Type, Value) ->
    try do_coerce(Type, Value)
    catch
        error:badarg -> error(badcoersion);
        error:undef -> error(badcoersion)
    end.

do_coerce(any, Value) -> Value;
do_coerce(Type, null) -> do_coerce(Type, undefined);
do_coerce(binary, undefined) ->
    undefined;
do_coerce(binary, Value) when is_binary(Value) ->
    Value;
do_coerce(binary, Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
do_coerce(binary, Value) when is_integer(Value) ->
    integer_to_binary(Value);
do_coerce(binary, Value) when is_list(Value) ->
    list_to_binary(Value);
do_coerce(binary, Value) ->
    list_to_binary(io_lib:format("~p", [Value]));
do_coerce(integer, Value) when is_integer(Value) ->
    Value;
do_coerce(integer, Value) when is_float(Value) ->
    trunc(Value);
do_coerce(integer, undefined) -> 0;
do_coerce(integer, Value) when is_binary(Value) ->
    binary_to_integer(Value);
do_coerce(integer, Value) when is_list(Value) ->
    list_to_integer(Value);
do_coerce(float, Value) when is_float(Value) ->
    Value;
do_coerce(float, Value) when is_integer(Value) ->
    Value;
do_coerce(float, undefined) -> 0;
do_coerce(float, Value) when is_binary(Value) ->
    binary_to_float(Value);
do_coerce(float, Value) when is_list(Value) ->
    list_to_float(Value);
do_coerce(boolean, true) ->
    true;
do_coerce(boolean, _) ->
    false;
do_coerce([Type], Value) when is_list(Value) ->
    F = fun(V) -> do_coerce(Type, V) end,
    lists:map(F, Value);
do_coerce([_], undefined) -> [];
do_coerce([Type], Value) ->
    [do_coerce(Type, Value)];
do_coerce({Type}, {Value}) when is_list(Value) ->
    F = fun({K, V}) -> {K, do_coerce(Type, V)} end,
    {lists:map(F, Value)};
do_coerce({_}, undefined) ->
    {[]};
do_coerce({Type}, Value) when is_list(Value) ->
    do_coerce({Type}, {Value});
do_coerce({M, T}, Value) ->
    M:coerce(T, Value);
do_coerce(M, Value) ->
    M:coerce(M, Value).
