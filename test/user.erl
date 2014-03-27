-module(user).
-include("./test_helpers.hrl").

-export([start/0]).

-compile({parse_transform, pjm_parse_trans}).

-pjm_stores_in(users).
-pjm_fields([
             {'Login', binary, <<"test">>}
            ]).

new_test_() ->
    {
      setup,
      fun() -> new() end,
      fun(_) -> ok end,
      fun(U) ->
              [
               ?_assertEqual({user, {<<"test">>}, undefined}, U)
              ]
      end
    }

write_field('Login', Value, {user, Tuple, Dict}) ->
    {user, setelement(2, Tuple, Value), Dict};
write_field(Key, Value, {user, Tuple, undefined}) ->
    {user, Tuple, apply(orddict, append, [Key, Value, apply(orddict, new)])};
write_field(Key, Value, {user, Tuple, Dict}) ->
    {user, Tuple, apply(orddict, store, [Key, Value, Dict])}.

