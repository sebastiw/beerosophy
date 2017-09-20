-module(beerosophy_utils).

-export([ binary_to_date/1 ]).

binary_to_date(Day) ->
    [Y, M, D] = binary:split(Day, <<"-">>, [global]),
    {binary_to_integer(Y),
     binary_to_integer(M),
     binary_to_integer(D)}.
