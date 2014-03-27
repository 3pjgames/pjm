-module(pjm_parse_trans).

-export([parse_transform/2, format_error/1]).

-compile({parse_transform, parse_trans_codegen}).

-record(state, {
          module,
          stores_in,
          line,
          get_one_defined = false,
          set_one_defined = false,
          uses_dict = orddict,
          fields = []
         }).

-record(field, {name, index, type = any, default}).

parse_transform(Forms, Options) ->
    parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
    State = parse_trans:do_inspect(fun inspect_f/4, #state{}, Forms, Context),
    State1 = State#state{fields = lists:reverse(State#state.fields)},
    {Forms1, _} = case State1#state.line of
                      undefined -> {Forms, State1};
                      _ -> parse_trans:do_transform(fun generate_f/4, State1, Forms, Context)
                  end,
    %% io:format(">>>>>~n~p~n=====~n", [Forms]),
    %% io:format("~p~n<<<<<~n", [Forms1]),
    Forms1.

inspect_f(attribute, {attribute, _L, module, Module}, _Ctxt, Acc) ->
    Acc1 = case Acc#state.stores_in of
               undefined -> Acc#state{stores_in = Module};
               _ -> Acc
           end,
    {false, Acc1#state{module = Module}};
inspect_f(function, {function, _L, get_one, 3, _}, _Ctxt, Acc) ->
    {false, Acc#state{get_one_defined = true}};
inspect_f(function, {function, _L, set_one, 3, _}, _Ctxt, Acc) ->
    {false, Acc#state{set_one_defined = true}};
inspect_f(attribute, {attribute, L, pjm_stores_in, StoresIn}, _Ctxt, Acc) ->
    {false, Acc#state{stores_in = StoresIn, line = L}};
inspect_f(attribute, {attribute, L, pjm_fields, Fields}, _Ctxt, Acc) ->
    {false, inspect_field(Fields, Acc#state{line = L})};
inspect_f(attribute, {attribute, L, pjm_uses_dict, Module}, _Ctxt, Acc) ->
    {false, Acc#state{uses_dict = Module, line = L}};
inspect_f(attribute, {attribute, L, pjm, _}, _Ctxt, Acc) ->
    {false, Acc#state{line = L}};
inspect_f(_Type, _Form, _Context, Acc) ->
    {false, Acc}.

inspect_field([Name|T], Acc) when is_atom(Name) ->
    inspect_field(T, add_field(Name, binary, undefined, Acc));
inspect_field([{Name}|T], Acc) ->
    inspect_field(T, add_field(Name, binary, undefined, Acc));
inspect_field([{Name, Type}|T], Acc) ->
    inspect_field(T, add_field(Name, Type, undefined, Acc));
inspect_field([{Name, Type, Default}|T], Acc) ->
    inspect_field(T, add_field(Name, Type, Default, Acc));
inspect_field([], Acc) ->
    Acc.

add_field(Name, Type, Default, Acc) ->
    #state { fields = Fields } = Acc,
    Index = length(Fields) + 1,
    Field = #field { name = Name,
                     index = Index,
                     type = Type,
                     default = Default },
    Acc#state { fields = [Field | Fields] }.
generate_f(attribute, {attribute, L, _, _} = Form, _Ctxt, State) ->
    if
        L =:= State#state.line ->
            Forms = lists:append([generate_export(State),
                                  generate_new_0(State),
                                  generate_new_1(State),
                                  generate_read_field_3(State),
                                  generate_write_field_3(State),
                                  generate_get_one(State),
                                  generate_set_one(State),
                                  generate_set(State),
                                  generate_get(State),
                                  generate_pjm_info(State),
                                  generate_map(State),
                                  generate_fold(State),
                                  generate_to_list(State),
                                  generate_coerce(State)
                                 ]),
            {[], Form, Forms, false, State};
        true -> {Form, false, State}
    end;
generate_f(_Type, Form, _Ctxt, State) ->
    {Form, false, State}.

generate_export(#state{ line = L }) ->
    [{attribute, L, export, [{new, 0},
                             {new, 1},
                             {set, 2},
                             {set, 3},
                             {get, 2},
                             {get, 3},
                             {map, 2},
                             {fold, 3},
                             {to_list, 1},
                             {pjm_info, 1},
                             {coerce, 2}
                            ]}].

generate_new_0(#state{module = Module, fields = Fields}) ->
    Defaults = lists:map(fun(F) -> F#field.default end, Fields),
    NewModel = {Module, list_to_tuple(Defaults), undefined},
    [codegen:gen_function(new, fun() -> {'$var', NewModel} end)].

generate_new_1(#state{module = Module}) ->
    [codegen:gen_function(
       new,
       fun(Attrs) when is_list(Attrs) -> set(Attrs, new());
          ({{'$var', Module}, _, _} = Model) -> Model
       end)].

generate_read_field_3(#state{module = Module, uses_dict = Backend, line = L, fields = Fields}) ->
    Fun1 = codegen:gen_function(
             read_field,
             [ fun({'$var', Name}, Default, {{'$var', Module}, Tuple, _}) ->
                       case element({'$var', Index}, Tuple) of
                           undefined -> Default;
                           V -> V
                       end
               end || #field{name = Name, index = Index} <- Fields ]),
    {function,_,_,_,Clauses1} = Fun1,
    Fun2 = codegen:gen_function(
             read_field,
             fun(_Key, Default, {{'$var', Module}, _, undefined}) ->
                     Default;
                (Key, Default, {{'$var', Module}, _, Dict}) ->
                     case apply({'$var', Backend}, find, [Key, Dict]) of
                         {ok, V} -> V;
                         _ -> Default
                     end
             end),
    {function,_,_,_,Clauses2} = Fun2,
    [{function,L,read_field,3,
      (Clauses1 ++ Clauses2)}].

generate_write_field_3(#state{module = Module, uses_dict = Backend, line = L, fields = Fields}) ->
    Fun1 = codegen:gen_function(
             write_field,
             [ fun({'$var', Name}, Value, {{'$var', Module}, Tuple, Dict}) ->
                       {{'$var', Module}, setelement({'$var', Index}, Tuple, pjm_coercion:coerce({'$var', Type}, Value)), Dict}
               end || #field{name = Name, index = Index, type = Type} <- Fields ]),
    {function,_,_,_,Clauses1} = Fun1,
    CreateDict = {call,L,
                  {remote,L,{atom,L,Backend},{atom,57,store}},
                  [{var,L,'Key'},
                   {var,L,'Value'},
                   {call,L,{remote,L,{atom,L,Backend},{atom,L,new}},[]}]},
    StoreDict = {call,L,
                 {remote,L,{atom,L,Backend},{atom,L,store}},
                 [{var,L,'Key'},
                  {var,L,'Value'},
                  {var,L,'Dict'}]},
    Erase = {call,L,
             {remote,L,{atom,L,Backend},{atom,L,erase}},
             [{var,L,'Key'},
              {var,L,'Dict'}]},
    Fun2 = codegen:gen_function(
             write_field,
             fun(_Key, undefined, {{'$var', Module}, _Tuple, undefined} = Model) ->
                     Model;
                (Key, Value, {{'$var', Module}, Tuple, undefined}) ->
                     {{'$var', Module},
                      Tuple,
                      {'$form', CreateDict}};
                (Key, undefined, {{'$var', Module}, Tuple, Dict}) ->
                     {{'$var', Module},
                      Tuple,
                      {'$form', Erase}};
                (Key, Value, {{'$var', Module}, Tuple, Dict}) ->
                     {{'$var', Module},
                      Tuple,
                      {'$form', StoreDict}}
             end),
    {function,_,_,_,Clauses2} = Fun2,
    [{function,L,write_field,3,
      (Clauses1 ++ Clauses2)}].

generate_get_one(#state{get_one_defined = true}) -> [];
generate_get_one(_State) ->
    [
     codegen:gen_function(get_one, fun(Key, Default, Model) -> read_field(Key, Default, Model) end)
    ].

generate_get(_State) ->
    [
     codegen:gen_function(
       get,
       fun(Key, Model) when is_atom(Key) -> get_one(Key, undefined, Model);
          (Keys, Model) ->
               F = fun(K) when is_atom(K) -> get_one(K, undefined, Model);
                      ({K, Default}) -> get_one(K, Default, Model)
                   end,
               lists:map(F, Keys)
       end),
     codegen:gen_function(
       get,
       fun(Key, Default, Model) -> get_one(Key, Default, Model) end
      )
    ].

generate_set_one(#state{set_one_defined = true}) -> [];
generate_set_one(_State) ->
    [
     codegen:gen_function(set_one, fun(Key, Value, Model) -> write_field(Key, Value, Model) end)
    ].

generate_set(_State) ->
    [
     codegen:gen_function(
       set,
       fun([], Model) -> Model;
          ([{K, V}|Rest], Model) ->
               set(Rest, set_one(K, V, Model))
       end),
     codegen:gen_function(
       set,
       fun(Key, Value, Model) -> set_one(Key, Value, Model) end
      )
    ].

generate_map(#state { module = Module, uses_dict = Backend }) ->
    [
     codegen:gen_function(
       map,
       fun(Fun, {{'$var', Module}, Tuple, Dict}) ->
               {Model, _} = lists:foldl(
                              fun(Key, {ModelIn, Index}) ->
                                      { set_one(Key, Fun(Key, element(Index, Tuple)), ModelIn),
                                        Index + 1 }
                              end,
                              {new(), 1},
                              pjm_info(fields)),
               case Dict of
                   undefined -> Model;
                   _ ->
                       apply({'$var', Backend}, fold,
                             [fun(K, V, ModelIn) ->
                                      set_one(K, Fun(K, V), ModelIn)
                              end,
                              Model,
                              Dict])
               end
       end
      )
    ].

generate_fold((#state { module = Module, uses_dict = Backend })) ->
    [
     codegen:gen_function(
       fold,
       fun(Fun, Acc, {{'$var', Module}, Tuple, Dict}) ->
               {Acc1, _} = lists:foldl(
                             fun(Key, {AccIn, Index}) ->
                                     {Fun(Key, element(Index, Tuple), AccIn), Index + 1}
                             end,
                             {Acc, 1},
                             pjm_info(fields)
                            ),
               case Dict of
                   undefined -> Acc1;
                   _ ->
                       apply({'$var', Backend}, fold, [Fun, Acc1, Dict])
               end
       end
      )
    ].

generate_to_list(_State) ->
    [codegen:gen_function(
       to_list,
       fun(Model) ->
               fold(
                 fun(K, V, List) ->
                         [{K, V}|List]
                 end,
                 [],
                 Model
                )
       end
      )].

generate_pjm_info(#state { fields = Fields, stores_in = StoresIn }) ->
    FieldsNames = [ F#field.name || F <- Fields ],
    [
     codegen:gen_function(
       pjm_info,
       fun(fields) -> {'$var', FieldsNames};
          (stores_in) -> {'$var', StoresIn}
       end
      )
    ].

generate_coerce(#state{ module = Module }) ->
    [
     codegen:gen_function(
       coerce,
       fun({'$var', Module}, {{'$var', Module}, _, _} = Model) -> Model;
          ({'$var', Module}, Attrs) when is_list(Attrs) -> new(Attrs);
          ({'$var', Module}, {Attrs}) when is_list(Attrs) -> new(Attrs)
       end
      )
    ].

format_error(E) ->
    case io_lib:deep_char_list(E) of
        true ->
            E;
        _ ->
            io_lib:write(E)
    end.
