-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    lists:max(heighest(input())).

part2() ->
    length(heighest(input())).

heighest({{_, ToX}, {FromY, _}} = Range) ->
    [ Max || X <- lists:seq(1, ToX),
                Y <- lists:seq(FromY, abs(FromY)),
                (Max = max_height({X, Y}, {0, 0}, 0, Range)) =/= miss ].

max_height({VelX, VelY}, {X, Y} = Coord, Max, Range) ->
    case hit(Coord, Range) of
        hit -> Max;
        miss -> miss;
        before ->
            NextVelocity = {max(0, VelX - 1), VelY - 1},
            NextCoord = {X + VelX, Y + VelY},
            max_height(NextVelocity, NextCoord, max(Y, Max), Range)
    end.

hit({X, Y}, {{FromX, ToX}, {FromY, ToY}}) when X >= FromX, X =< ToX, Y >= FromY, Y =< ToY -> hit;
hit({X, Y}, {{_, ToX}, {FromY, _}}) when (X > ToX) or (Y < FromY) -> miss;
hit(_, _) -> before.

input() ->
    {ok, <<"target area: ", Coords/binary>>} = file:read_file(input),
    [<<"x=", RangesX/binary>>, <<" y=", RangesY/binary>>] = binary:split(Coords, <<",">>, [global, trim_all]),
    [FromX, ToX] = binary:split(RangesX, <<"..">>, [global, trim_all]),
    [FromY, ToY] = binary:split(RangesY, <<"..">>, [global, trim_all]),
    {{binary_to_integer(FromX), binary_to_integer(ToX)}, {binary_to_integer(FromY), binary_to_integer(ToY)}}.