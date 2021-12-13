-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    Routes = input(),
    length(find_paths(maps:get(<<"start">>, Routes), Routes, [<<"start">>], false, [<<"start">>])).

part2() ->
    Routes = input(),
    length(find_paths(maps:get(<<"start">>, Routes), Routes, [<<"start">>], true, [<<"start">>])).

find_paths([], _, _, _, _) -> [];
find_paths([Cave | Caves], Routes, Seen, CanUseSmallTwice, PathAcc) ->
    Current = case {string:lowercase(Cave) =:= Cave, lists:member(Cave, Seen), CanUseSmallTwice} of
        {true, true, false} -> [];
        {true, M, Ss} ->
            case Cave of
                <<"start">> -> [];
                <<"end">> -> [[Cave | PathAcc]];
                _ -> find_paths(maps:get(Cave, Routes), Routes, [Cave | Seen], Ss and not M, [Cave | PathAcc])
            end;
        {false, false, Ss} -> find_paths(maps:get(Cave, Routes), Routes, Seen, Ss, [Cave | PathAcc])
    end,
    Nexts = find_paths(Caves, Routes, Seen, CanUseSmallTwice, PathAcc),
    Current ++ Nexts.

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global, trim_all]),
    lists:foldl(fun ([From, To], Routes) ->
        M = maps:update_with(From, fun (Prev) -> [To | Prev] end, [To], Routes),
        maps:update_with(To, fun (Prev) -> [From | Prev] end, [From], M)
    end, maps:new(), lists:map(fun (L) -> binary:split(L, <<"-">>) end, Lines)).