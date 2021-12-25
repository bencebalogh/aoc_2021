-module(aoc).
-export([part1/0]).
-define(MAXX, 138).
-define(MAXY, 136).

part1() ->
    p(input(), 0).

p(Grid, Steps) ->
    case move(move(Grid, 62), 118) of
        NewGrid when NewGrid =:= Grid -> Steps + 1;
        NewGrid -> p(NewGrid, Steps + 1)
    end.

move(Grid, Char) ->
    maps:from_list(lists:map(fun
        ({Coords, C}) when C =:= Char ->
            NextCoords = next_coord(Char, Coords),
            case maps:get(NextCoords, Grid, free) of
                free -> {NextCoords, Char};
                _ -> {Coords, Char}
            end;
        (KV) -> KV
    end, maps:to_list(Grid))).

next_coord(62, {X, Y}) when X + 1 > ?MAXX -> {0, Y};
next_coord(62, {X, Y}) -> {X + 1, Y};
next_coord(118, {X, Y}) when Y + 1 > ?MAXY -> {X, 0};
next_coord(118, {X, Y}) -> {X, Y + 1}.

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global, trim_all]),
    lists:foldl(fun ({Line, Y}, Grid) ->
        lists:foldl(fun
            ({46, _}, G) ->
                G;
            ({N, X}, G) ->
                maps:put({X, Y}, N, G)
        end, Grid, lists:zip(binary:bin_to_list(Line), lists:seq(0, size(Line) - 1)))
    end, maps:new(), lists:zip(Lines, lists:seq(0, length(Lines) - 1))).