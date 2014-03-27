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
          fields = [],
          field_of_name = orddict:new()
         }).

-record(field, {name, index, type = binary, default}).

parse_transform(Forms, Options) ->
    parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
    State = parse_trans:do_inspect(fun inspect_f/4, #state{}, Forms, Context),
    State1 = State#state{fields = lists:reverse(State#state.fields)},
    {Forms1, _} = case State1#state.line of
                      undefined -> {Forms, State1};
                      _ -> parse_trans:do_transform(fun generate_f/4, State1, Forms, Context)
                  end,
    io:format(">>>>>~n~p~n=====~n", [Forms]),
    io:format("~p~n<<<<<~n", [Forms1]),
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
    #state { fields = Fields, field_of_name = Map } = Acc,
    Index = length(Fields) + 1,
    Field = #field { name = Name,
                     index = Index,
                     type = Type,
                     default = Default },
    Acc#state { fields = [Field | Fields],
                field_of_name = orddict:store(Name, Field, Map) }.

generate_f(attribute, {attribute, L, _, _} = Form, _Ctxt, State) ->
    if
        L =:= State#state.line ->
            Forms = lists:append([generate_new_0(State),
                                  generate_read_field_3(State)
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
     {function,L,new,0,
      [{clause,L,[],[],
        codegen:exprs(fun() -> {'$var', NewModel} end)}]}].


generate_read_field_3(#state{module = Module, uses_dict = Backend, line = L, fields = Fields}) ->
    Clauses = [
               {clause,L,
                [{atom,L,F#field.name},
                 {var,L,'Default'},
                 {tuple,L,
                  [{atom,L,Module},{var,L,'Tuple'},{var,L,'_'}]}],
                [],
                [{'case',L,
                  {call,L,
                   {atom,L,element},
                   [{integer,L,F#field.index},{var,L,'Tuple'}]},
                  [{clause,L,
                    [{atom,L,undefined}],
                    [],
                    [{var,L,'Default'}]},
                   {clause,L,
                    [{var,L,'V'}],
                    [],
                    [{var,L,'V'}]}]}]} || F <- Fields
              ],
    Clauses1 = [{clause,L,
                 [{var,L,'Key'},
                  {var,L,'Default'},
                  {tuple,L,[{atom,L,Module},{var,L,'_'},{var,L,'Dict'}]}],
                 [],
                 [{'case',L,
                   {call,L,
                    {atom,L,apply},
                    [{atom,L,Backend},
                     {atom,L,find},
                     {cons,L,
                      {var,L,'Key'},
                      {cons,L,{var,L,'Dict'},{nil,L}}}]},
                   [{clause,L,
                     [{tuple,L,[{atom,L,ok},{var,L,'V'}]}],
                     [],
                     [{var,L,'V'}]},
                    {clause,L,[{var,L,'_'}],[],[{var,L,'Default'}]}]}]},
                {clause,L,
                 [{var,L,'_'},
                  {var,L,'Default'},
                  {tuple,L,[{atom,L,Module},{var,L,'_'},{atom,L,undefined}]}],
                 [],
                 [{var,L,'Default'}]} |
                Clauses ],
    [{function,L,read_field,3,
      lists:reverse(Clauses1)}].

%% generate_read_field_3(#state{line = L, module = Module, fields = Fields}) ->

%% FunForms = codegen:gen_function(

%%                 )
%%     [{clause,15,
%%                     [{atom,15,foo},
%%                      {atom,15,undefined},
%%                      {tuple,15,
%%                             [{atom,15,user},{var,15,'Tuple'},{var,15,'_'}]}],
%%                     [],
%%                     [{call,16,
%%                            {atom,16,element},
%%                            [{integer,16,2},{var,16,'Tuple'}]}]}]},

format_error(E) ->
    case io_lib:deep_char_list(E) of
        true ->
            E;
        _ ->
            io_lib:write(E)
    end.
