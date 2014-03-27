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
            Forms = lists:append([generate_new_0(State),
                                  generate_read_field_3(State),
                                  generate_write_field_3(State),
                                  generate_get_one(State),
                                  generate_set_one(State)
                                 ]),
            {[], Form, Forms, false, State};
        true -> {Form, false, State}
    end;
generate_f(_Type, Form, _Ctxt, State) ->
    {Form, false, State}.

generate_new_0(#state{line = L, module = Module, fields = Fields}) ->
    Defaults = lists:map(fun(F) -> F#field.default end, Fields),
    NewModel = {Module, list_to_tuple(Defaults), undefined},
    [{attribute,L,export,[{new, 0}]},
     codegen:gen_function(new, fun() -> {'$var', NewModel} end)].

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
    Fun2 = codegen:gen_function(
             write_field,
             fun(Key, Value, {{'$var', Module}, Tuple, undefined}) ->
                     {{'$var', Module},
                      Tuple,
                      {'$form', CreateDict}};
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

generate_set_one(#state{set_one_defined = true}) -> [];
generate_set_one(_State) ->
    [
     codegen:gen_function(set_one, fun(Key, Value, Model) -> write_field(Key, Value, Model) end)
    ].

format_error(E) ->
    case io_lib:deep_char_list(E) of
        true ->
            E;
        _ ->
            io_lib:write(E)
    end.
