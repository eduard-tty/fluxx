-module(shuffle).
-export([shuffle/1]).

-spec shuffle([any()]) -> [any()].
shuffle(L) -> [ X || {_,X} <- lists:sort([ {random:uniform(), N} || N <- L]) ].
