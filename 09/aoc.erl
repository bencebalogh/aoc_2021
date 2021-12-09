-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    lists:sum(lists:map(fun ({_, V}) -> V + 1 end, get_low_points(input()))).

part2() ->
    Grid = input(),
    [A, B, C | _] = lists:reverse(lists:sort(lists:map(fun ({Coord, V}) -> get_basin(Grid, Coord, V, maps:new()) end, get_low_points(Grid)))),
    maps:size(A) * maps:size(B) * maps:size(C).

adjacent_coords({X, Y}) -> [{X + 1, Y}, {X - 1, Y}, {X, Y + 1}, {X, Y - 1}].

get_low_points(Grid) ->
    lists:filter(fun ({Coord, Value}) ->
        Adjacents = lists:map(fun (C) -> maps:get(C, Grid, 10) end, adjacent_coords(Coord)),
        lists:all(fun (V) -> Value < V end, Adjacents)
    end, maps:to_list(Grid)).

get_basin(Grid, Coord, Value, Acc) ->
    Neighbours = adjacent_coords(Coord),
    NotSeen = lists:filter(fun (N) -> not maps:is_key(N, Acc) end, Neighbours),
    Nexts = lists:filter(fun (N) -> maps:get(N, Grid, 9) =/= 9 end, NotSeen),
    lists:foldl(fun (N, A) -> get_basin(Grid, N, maps:get(N, Grid), A) end, maps:put(Coord, Value, Acc), Nexts).

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global, trim_all]),
    lists:foldl(fun ({Line, Y}, Grid) ->
        lists:foldl(fun ({N, X}, G) ->
            maps:put({X, Y}, list_to_integer([N]), G)
        end, Grid, lists:zip(binary:bin_to_list(Line), lists:seq(0, size(Line) - 1)))
    end, maps:new(), lists:zip(Lines, lists:seq(0, length(Lines) - 1))).