-module(pjm).
-export([is/2]).

-type model() :: {atom(), tuple(), term()}.
-type model(T) :: {T, tuple(), term()}.
-export_type([model/0, model/1]).

-spec is(atom(), model()) -> boolean().
is(Type, {Type, _, _} = _Model) -> true;
is(_, _) -> false.
