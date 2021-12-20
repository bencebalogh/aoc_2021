-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    p(2).

part2() ->
    p(50).

p(Times) ->
    {Algorithm, Image} = input(),
    Final = lists:foldl(fun (I, Img) ->
        case I rem 2 of
            1 -> transform(Img, Algorithm, 49);
            0 -> transform(Img, Algorithm, 48)
        end
    end, Image, lists:seq(0, Times - 1)),
    length(lists:filter(fun (X) -> X =:= 49 end, maps:values(Final))).

transform(Image, Algorithm, InfiniteState) ->
    Extended = extend_image(Image, InfiniteState),
    Changed = maps:from_list(lists:map(fun ({Coord, _}) ->
        Lookup = lists:map(fun (Adj) ->
            maps:get(Adj, Extended, InfiniteState)
        end, adjacent_positions(Coord)),
        {Coord, maps:get(list_to_integer(Lookup, 2), Algorithm)}
    end, maps:to_list(Extended))),
    Changed.

extend_image(Image, InfiniteState) ->
    {MinX, MinY} = lists:min(maps:keys(Image)),
    {MaxX, MaxY} = lists:max(maps:keys(Image)),
    Top = lists:flatmap(fun (X) ->
        lists:map(fun (Y) -> {X, Y} end, lists:seq(MinX - 2, MaxX + 2))
    end, lists:seq(MinY - 2, MinY - 1)),
    Bottom = lists:flatmap(fun (X) ->
        lists:map(fun (Y) -> {X, Y} end, lists:seq(MinX - 2, MaxX + 2))
    end, lists:seq(MaxY + 1, MaxY + 2)),
    Left = lists:flatmap(fun (X) ->
        lists:map(fun (Y) -> {X, Y} end, lists:seq(MinX - 2, MinX - 1))
    end, lists:seq(MinY, MaxY)),
    Right = lists:flatmap(fun (X) ->
        lists:map(fun (Y) -> {X, Y} end, lists:seq(MaxX + 1, MaxX + 2))
    end, lists:seq(MinY, MaxY)),
    Bottom,
    lists:foldl(fun (Section, Img) ->
        lists:foldl(fun (C, I) -> maps:put(C, InfiniteState, I) end, Img, Section)
    end, Image, [Top, Left, Right, Bottom]).

adjacent_positions({X, Y}) ->
    [
        {X - 1, Y - 1},
        {X, Y - 1},
        {X + 1, Y - 1},
        {X - 1, Y},
        {X, Y},
        {X + 1, Y},
        {X - 1, Y + 1},
        {X, Y + 1},
        {X + 1, Y + 1}
    ].

input() ->
    {ok, Input} = file:read_file(input),
    [Alg | Img] = binary:split(Input, <<"\n">>, [global, trim_all]),
    Algorithm = lists:foldl(fun
        ({I, 46}, M) -> maps:put(I, 48, M);
        ({I, 35}, M) -> maps:put(I, 49, M)
    end, #{}, lists:zip(lists:seq(0, size(Alg) - 1), binary_to_list(Alg))),
    Image = lists:foldl(fun ({Line, Y}, Grid) ->
        lists:foldl(fun
            ({46, X}, G) -> maps:put({X, Y}, 48, G);
            ({35, X}, G) -> maps:put({X, Y}, 49, G)
        end, Grid, lists:zip(binary:bin_to_list(Line), lists:seq(0, size(Line) - 1)))
    end, maps:new(), lists:zip(Img, lists:seq(0, length(Img) - 1))),
    {Algorithm, Image}.