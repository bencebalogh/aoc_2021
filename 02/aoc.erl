-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    move({0, 0}, input()).

part2() ->
    move2({0, 0, 0}, input()).

move({Horizontal, Depth}, []) -> Horizontal * Depth;
move({Horizontal, Depth}, [{forward, V} | T]) -> move({Horizontal + V, Depth}, T);
move({Horizontal, Depth}, [{up, V} | T]) -> move({Horizontal, Depth - V}, T);
move({Horizontal, Depth}, [{down, V} | T]) -> move({Horizontal, Depth + V}, T).

move2({Horizontal, Depth, _}, []) -> Horizontal * Depth;
move2({Horizontal, Depth, Aim}, [{forward, V} | T]) -> move2({Horizontal + V, Depth + (Aim * V), Aim}, T);
move2({Horizontal, Depth, Aim}, [{up, V} | T]) -> move2({Horizontal, Depth, Aim - V}, T);
move2({Horizontal, Depth, Aim}, [{down, V} | T]) -> move2({Horizontal, Depth, Aim + V}, T).

input() ->
    {ok, I} = file:read_file(input),
    lists:map(fun (L) ->
                      [C, V] = binary:split(L, <<"\s">>),
                      {binary_to_atom(C), binary_to_integer(V)}
              end, binary:split(I, <<"\n">>, [global, trim_all])).
