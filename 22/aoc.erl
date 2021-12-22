-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    p([ Line || {_, Coords} = Line <- input(),
                    lists:all(fun ({Min, Max}) ->
                        (Min >= -50) and (Max =< 50)
                    end, Coords)]).

part2() ->
    p(input()).

p(Input) ->
    Lights = lists:foldl(fun switch_lights/2, [], Input),
    lists:sum(lists:map(fun count_lights/1, Lights)).

switch_lights({on, Coords}, LightsOn) ->
    [Coords | lists:flatmap(fun (On) -> range_diff(On, Coords) end, LightsOn)];
switch_lights({off, Coords}, LightsOn) ->
    lists:flatmap(fun (On) -> range_diff(On, Coords) end, LightsOn).

range_diff(A, B) ->
    [{MinXA, MaxXA} = X1, {MinYA, MaxYA}, {MinZA, MaxZA} = Z1] = A,
    [{MinXB, MaxXB}, {MinYB, MaxYB}, {MinZB, MaxZB}] = B,
    XAfter = MinXA > MaxXB,
    XBefore = MinXB > MaxXA,
    YAfter = MinYA > MaxYB,
    YBefore = MinYB > MaxYA,
    ZAfter = MinZA > MaxZB,
    ZBefore = MinZB > MaxZA,
    case lists:any(fun (Cond) -> Cond =:= true end, [XAfter, XBefore, YAfter, YBefore, ZAfter, ZBefore]) of
        true -> [A];
        false ->
            YRangeFromBoth = {max(MinYA, MinYB), min(MaxYA, MaxYB)},
            XRangeFromBoth = {max(MinXA, MinXB), min(MaxXA, MaxXB)},

            FromTop = case MinYB > MinYA of
                    true -> [[X1, {MinYA, MinYB}, Z1]];
                    false -> []
                end,
            FromBottom = case MaxYA > MaxYB of 
                    true -> [[X1, {MaxYB, MaxYA}, Z1] | FromTop];
                    false -> FromTop
                end,
            FromLeft = case MinXB > MinXA of
                    true -> [[{MinXA, MinXB}, YRangeFromBoth, Z1] | FromBottom];
                    false -> FromBottom
                end,
            FromRight = case MaxXA > MaxXB of
                    true -> [[{MaxXB, MaxXA}, YRangeFromBoth, Z1] | FromLeft];
                    false -> FromLeft
                end,
            FromFront = case MinZB > MinZA of
                    true -> [[XRangeFromBoth, YRangeFromBoth, {MinZA, MinZB}] | FromRight];
                    false -> FromRight
                end,
            case MaxZA > MaxZB of
                true -> [[XRangeFromBoth, YRangeFromBoth, {MaxZB, MaxZA}] | FromFront];
                false -> FromFront
            end
    end.

count_lights([{MinX, MaxX}, {MinY, MaxY}, {MinZ, MaxZ}]) ->
    (MaxX - MinX) * (MaxY - MinY) * (MaxZ - MinZ).

input() ->
    {ok, Input} = file:read_file(input),
    Lines = lists:map(fun (Line) ->
        case binary:split(Line, <<",">>, [global, trim_all]) of
            [<<"on x=", X/binary>> , <<"y=", Y/binary>>, <<"z=", Z/binary>>] ->
                {on, [X, Y, Z]};
            [<<"off x=", X/binary>> , <<"y=", Y/binary>>, <<"z=", Z/binary>>] ->
                {off, [X, Y, Z]}
        end
    end, binary:split(Input, <<"\n">>, [global, trim_all])),
    lists:map(fun ({S, Coords}) ->
        C = lists:map(fun (Raw) ->
            [Start, End] = binary:split(Raw, <<"..">>),
            {binary_to_integer(Start), binary_to_integer(End) + 1}
        end, Coords),
        {S, C}
    end, Lines).