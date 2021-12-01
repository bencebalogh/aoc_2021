-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    count_increases(input()).

part2() ->
    Input = input(),
    Count = length(Input),
    count_increases(lists:map(fun (I) -> lists:sum(take3(drop(Input, I))) end, lists:seq(0, Count - 3))).

count_increases(L) -> count_increases(tl(L), hd(L), 0).
count_increases([], _, S) -> S;
count_increases([H | T], P, S) when H > P -> count_increases(T, H, S + 1);
count_increases([H | T], _, S) -> count_increases(T, H, S).

drop(L, 0) -> L;
drop([], _) -> [];
drop([_ | T], N) -> drop(T, N - 1).

take3(L) -> take3(L, [], 3).
take3(_, S, 0) -> lists:reverse(S);
take3([H | T], S, C) -> take3(T, [H | S], C - 1).

input() ->
    {ok, I} = file:read_file(input),
    lists:map(fun (X) -> binary_to_integer(X) end, binary:split(I, <<"\n">>, [global, trim_all])).
