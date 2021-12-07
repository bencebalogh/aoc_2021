-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    Input = input(),
    Keys = maps:keys(Input),
    lists:min(lists:map(fun (V) ->
        lists:sum(lists:map(fun (P) -> abs(P - V) * maps:get(P, Input) end, Keys))
    end, lists:seq(lists:min(Keys), lists:max(Keys)))).

part2() ->
    Input = input(),
    Keys = maps:keys(Input),
    lists:min(lists:map(fun (V) ->
        lists:sum(lists:map(fun (P) -> (abs(P - V) * (abs(P - V) + 1) div 2 ) * maps:get(P, Input) end, Keys))
    end, lists:seq(lists:min(Keys), lists:max(Keys)))).

input() ->
    {ok, I} = file:read_file(input),
    Formatted = lists:map(fun binary_to_integer/1, binary:split(I, <<",">>, [global, trim_all])),
    lists:foldl(fun (D, M) -> maps:update_with(D, fun(V) -> V + 1 end, 1, M) end, maps:new(), Formatted).
