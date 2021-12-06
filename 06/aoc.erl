-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    lists:sum(maps:values((evolve(input(), 80)))).

part2() ->
    lists:sum(maps:values((evolve(input(), 256)))).

evolve(Fish, 0) -> Fish;
evolve(Fish, Index) ->
    Next = maps:fold(fun
        (0, Count, M) ->
            maps:put(8, Count, maps:update_with(6, fun (V) -> V + Count end, Count, M));
        (Day, Count, M) ->
            maps:update_with(Day - 1, fun (V) -> V + Count end, Count, M)
    end, maps:new(), Fish),
    evolve(Next, Index - 1).

input() ->
    {ok, I} = file:read_file(input),
    Formatted = lists:map(fun binary_to_integer/1, binary:split(I, <<",">>, [global, trim_all])),
    lists:foldl(fun (D, M) -> maps:update_with(D, fun(V) -> V + 1 end, 1, M) end, maps:new(), Formatted).