-module(pjm_coercion).
-compile({ parse_transform, ct_expand }).
-export([coerce/2]).

%% Coerce Value to proper value of type Type.
-spec coerce(term(), term()) -> term().
coerce(Type, Value) ->
    try do_coerce(Type, Value)
    catch
        error:badarg -> throw({badcoersion, Type, Value});
        error:undef -> throw({badcoersion, Type, Value})
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
do_coerce(integer, Value) when is_atom(Value) ->
    binary_to_integer(atom_to_binary(Value, utf8));
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
do_coerce(timestamp, {_Mega, _Secs, _Micro} = Timestamp) ->
    Timestamp;
do_coerce(timestamp, {Date, Time} = Datetime) when is_tuple(Date) andalso is_tuple(Time) ->
    Seconds = calendar:datetime_to_gregorian_seconds(Datetime) - ct_expand:term(calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})),
    {Seconds div 1000000, Seconds rem 1000000, 0};
do_coerce(timestamp, Timestamp) when is_binary(Timestamp) ->
    %% "2009-04-12T20:44:55Z"
    {ok, Re} = ct_expand:term(re:compile(<<"^(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)T(\\d\\d):(\\d\\d):(\\d\\d)Z?">>, [caseless])),
    case re:run(Timestamp, Re, [{capture, all_but_first, binary}]) of
        {match, [Y, Mon, D, H, M, S]} ->
            do_coerce(timestamp,
                      {{binary_to_integer(Y), binary_to_integer(Mon), binary_to_integer(D)},
                       {binary_to_integer(H), binary_to_integer(M), binary_to_integer(S)}});
        _ -> throw({badcoersion, timestamp, Timestamp})
    end;
do_coerce(timestamp, undefined) -> undefined;
do_coerce([Type], Value) when is_list(Value) ->
    F = fun(V) -> do_coerce(Type, V) end,
    lists:map(F, Value);
do_coerce([_], undefined) -> [];
do_coerce([Type], Value) ->
    [do_coerce(Type, Value)];
do_coerce({Type}, {Value}) when is_list(Value) ->
    F = fun({K, V}) -> {K, do_coerce(Type, V)} end,
    {lists:map(F, Value)};
do_coerce({Type}, {pjm, _, _} = Model) ->
    do_coerce({Type}, pjm:to_list(Model));
do_coerce({_}, undefined) ->
    {[]};
do_coerce({Type}, Value) when is_list(Value) ->
    do_coerce({Type}, {Value});
do_coerce({dict, _KeyType, _ValueType}, Value) when is_tuple(Value) andalso element(1, Value) =:= dict ->
    Value;
do_coerce({dict, KeyType, ValueType}, Value) ->
    lists:foldl(
      fun({K, V}, Dict) ->
              dict:store(
                do_coerce(KeyType, K),
                do_coerce(ValueType, V),
                Dict
               )
      end,
      dict:new(),
      get_proplist(Value)
     );
do_coerce({orddict, KeyType, ValueType}, Value) ->
    lists:foldl(
      fun({K, V}, Dict) ->
              orddict:store(
                do_coerce(KeyType, K),
                do_coerce(ValueType, V),
                Dict
               )
      end,
      orddict:new(),
      get_proplist(Value)
     );
do_coerce({M, T}, Value) ->
    M:coerce(T, Value);
do_coerce(M, Value) ->
    M:coerce(M, Value).

get_proplist({List}) when is_list(List) -> List;
get_proplist(List) when is_list(List) -> List.
