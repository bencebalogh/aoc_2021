-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    {_, Flashes} = lists:foldl(fun (_, Acc) -> step(Acc) end, {input(), 0}, lists:seq(0, 99)),
    Flashes.

part2() ->
    p2(input(), 0).

p2(Grid, Step) ->
    case step({Grid, 0}) of
        {_, 100} -> Step + 1;
        {G, _} -> p2(G, Step + 1)
    end.

step({Grid, FlashCount}) ->
    flash(increase(Grid), FlashCount).

flash(Grid, Count) ->
    case [C || {C, V} <- maps:to_list(Grid), V >= 10] of
        [] -> {Grid, Count};
        [H | _] ->
            Increase = [P || P <- adjacent_positions(H),
                                    maps:is_key(P, Grid),
                                    N <- [maps:get(P, Grid)],
                                    N =/= 0],
            After = lists:foldl(fun (P, G) ->
                maps:update_with(P, fun (V) -> V + 1 end, G)
            end, Grid, Increase),
            flash(maps:put(H, 0, After), Count + 1)
    end.

increase(Grid) ->
    maps:map(fun (_, V) -> V + 1 end, Grid).

adjacent_positions({X, Y}) ->
    [
        {X - 1, Y - 1},
        {X - 1, Y},
        {X - 1, Y + 1},
        {X + 1, Y - 1},
        {X + 1, Y},
        {X + 1, Y + 1},
        {X, Y - 1},
        {X, Y + 1}
    ].

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global, trim_all]),
    lists:foldl(fun ({Line, Y}, Grid) ->
        lists:foldl(fun ({N, X}, G) ->
            maps:put({X, Y}, list_to_integer([N]), G)
        end, Grid, lists:zip(binary:bin_to_list(Line), lists:seq(0, size(Line) - 1)))
    end, maps:new(), lists:zip(Lines, lists:seq(0, length(Lines) - 1))).