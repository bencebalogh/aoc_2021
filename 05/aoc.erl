-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    p([Coord || {{Xa, Ya}, {Xb, Yb}} = Coord <- input(), (Xa =:= Xb) or (Ya =:= Yb)]).

part2() ->
    p(input()).

p(Coords) ->
    length(lists:filter(fun (X) -> X >= 2 end, maps:values(lists:foldl(fun (L, Grid) ->
        lists:foldl(fun (Coord, G) -> maps:put(Coord, maps:get(Coord, G, 0) + 1, G) end, Grid, L)
    end, maps:new(), lists:map(fun calculate_line/1, Coords))))).

calculate_line({{X, FromY}, {X, ToY}}) ->
    lists:map(fun (Y) -> {X, Y} end, lists:seq(min(FromY, ToY), max(FromY, ToY)));
calculate_line({{FromX, Y}, {ToX, Y}}) ->
    lists:map(fun (X) -> {X, Y} end, lists:seq(min(ToX, FromX), max(ToX, FromX)));
calculate_line({From, To}) ->
    diagonal(From, To, [From]).

diagonal(To, To, Agg) -> Agg;
diagonal({Xa, Ya}, {Xb, Yb} = From, Agg) when (Xb > Xa) and (Yb > Ya) ->
    diagonal({Xa + 1, Ya + 1}, From, [{Xa + 1, Ya + 1} | Agg]);
diagonal({Xa, Ya}, {Xb, Yb} = From, Agg) when (Xb < Xa) and (Yb < Ya) ->
    diagonal({Xa - 1, Ya - 1}, From, [{Xa - 1, Ya - 1} | Agg]);
diagonal({Xa, Ya}, {Xb, Yb} = From, Agg) when (Xb < Xa) and (Yb > Ya) ->
    diagonal({Xa - 1, Ya + 1}, From, [{Xa - 1, Ya + 1} | Agg]);
diagonal({Xa, Ya}, {Xb, Yb} = From, Agg) when (Xb > Xa) and (Yb < Ya) ->
    diagonal({Xa + 1, Ya - 1}, From, [{Xa + 1, Ya - 1} | Agg]).

input() ->
    {ok, I} = file:read_file(input),
    lists:map(fun (L) ->
            [From, To] = binary:split(L, <<" -> ">>),
            [Fx, Fy] = binary:split(From, <<",">>),
            [Tx, Ty] = binary:split(To, <<",">>),
            {{binary_to_integer(Fx), binary_to_integer(Fy)}, {binary_to_integer(Tx), binary_to_integer(Ty)}}
    end, binary:split(I, <<"\n">>, [global, trim_all])).
