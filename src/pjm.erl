-module(pjm).

-export([is/1, is/2, type/1]).
-export([new/1, new/2]).
-export([set/2, set/3]).
-export([get/2, get/3]).
-export([map/2, fold/3]).
-export([to_list/1]).
-export([coerce/2]).

-type model() :: {atom(), tuple(), term()}.
-type model(T) :: {T, tuple(), term()}.
-export_type([model/0, model/1]).

-type proplist() :: [proplists:property()].

-spec is(term()) -> boolean().
is({pjm, Type, Tuple, _} = _Model) when is_atom(Type) andalso is_tuple(Tuple) -> true;
is(_) -> false.

-spec is(atom(), term()) -> boolean().
is(Type, {pjm, Type, Tuple, _} = _Model) when is_tuple(Tuple) -> true;
is(_, _) -> false.

-spec type(model()) -> atom().
type({pjm, Type, Tuple, _} = _Model) when is_atom(Type) andalso is_tuple(Tuple) -> Type.

-spec new(module()) -> model().
new(Type) -> Type:new().

-spec new(module(), model() | proplist()) -> model().
new(Type, Attrs) -> Type:new(Attrs).

-spec set(proplist(), model()) -> model().
set(Attrs, {pjm, Module, _, _} = Model) ->
    Module:set(Attrs, Model).

-spec set(atom(), term(), model()) -> model().
set(K, V, {pjm, Module, _, _} = Model) ->
    Module:set(K, V, Model).

-spec get([atom() | {atom(), term()}] | atom(), model()) -> [term()].
get(Keys, {pjm, Module, _, _} = Model) ->
    Module:get(Keys, Model).

-spec get(atom(), term(), model()) -> term().
get(K, Def, {pjm, Module, _, _} = Model) ->
    Module:set(K, Def, Model).

-spec map(fun((atom(), term()) -> term()), model()) -> model().
map(F, {pjm, Module, _, _} = Model) ->
    Module:map(F, Model).

-spec fold(fun((atom(), term(), term()) -> term()), term(), model()) -> term().
fold(F, Acc, {pjm, Module, _, _} = Model) ->
    Module:fold(F, Acc, Model).

-spec to_list(model()) -> proplist().
to_list({pjm, Module, _, _} = Model) ->
    Module:to_list(Model).

-spec coerce(atom(), term()) -> term().
coerce(Module, Value) ->
    Module:coerce(Module, Value).
