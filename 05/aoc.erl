-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    p([Coord || {{Xa, Ya}, {Xb, Yb}} = Coord <- input(), (Xa =:= Xb) or (Ya =:= Yb)]).

part2() ->
    p(input()).

p(Coords) ->
    length(lists:filter(fun (X) -> X >= 2 end, maps:values(lists:foldl(fun (L, Grid) ->
        lists:foldl(fun (Coord, G) -> maps:put(Coord, maps:get(Coord, G, 0) + 1, G) end, Grid, L)
    end, maps:new(), lists:map(fun points_between/1, Coords))))).

points_between({{Xa, Ya}, {Xb, Yb}}) ->
   Dx = signum(Xb - Xa), 
   Dy = signum(Yb - Ya),
   Steps = max((Xb - Xa) * Dx, (Yb - Ya) * Dy),
   lists:map(fun (Step) -> {Step * Dx + Xa, Step * Dy + Ya} end, lists:seq(0, Steps)).

signum(V) when V > 0 -> 1;
signum(V) when V < 0 -> -1;
signum(_) -> 0.

input() ->
    {ok, I} = file:read_file(input),
    lists:map(fun (L) ->
            [From, To] = binary:split(L, <<" -> ">>),
            [Fx, Fy] = binary:split(From, <<",">>),
            [Tx, Ty] = binary:split(To, <<",">>),
            {{binary_to_integer(Fx), binary_to_integer(Fy)}, {binary_to_integer(Tx), binary_to_integer(Ty)}}
    end, binary:split(I, <<"\n">>, [global, trim_all])).
