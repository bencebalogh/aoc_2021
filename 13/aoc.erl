-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    {Coords, Folds} = input(),
    sets:size(sets:from_list(fold(Coords, hd(Folds)))).

part2() ->
    {Coords, Folds} = input(),
    draw(fold_all(Coords, Folds)).

draw(Coords) ->
    MaxX = lists:max(lists:map(fun ({X, _}) -> X end, Coords)),
    MaxY = lists:max(lists:map(fun ({_, Y}) -> Y end, Coords)),
    CoordsSet = sets:from_list(Coords),
    lists:foreach(fun (Y) ->
        Line = lists:foldl(fun (X, L) ->
            case sets:is_element({X, Y}, CoordsSet) of
                true -> L ++ "#";
                false -> L ++ "."
            end
        end, "", lists:seq(0, MaxX)),
        erlang:display(Line)
    end, lists:seq(0, MaxY)).

fold_all(Coords, []) -> Coords;
fold_all(Coords, [H | T]) ->
    fold_all(fold(Coords, H), T).

fold(Coords, {Axis, FoldValue}) ->
    case Axis of
        "y" -> lists:map(fun
            ({X, Y}) when Y > FoldValue ->
                {X, FoldValue - (Y - FoldValue)};
            (C) -> C
        end, Coords);
        "x" -> lists:map(fun
            ({X, Y}) when X > FoldValue ->
                {FoldValue - (X - FoldValue), Y};
            (C) -> C
        end, Coords)
    end.

input() ->
    {ok, I} = file:read_file(input),
    [RawCoords, RawFolds] = binary:split(I, <<"\n\n">>, [global, trim_all]),
    Coords = lists:map(fun (C) ->
        [X, Y] = binary:split(C, <<",">>),
        {binary_to_integer(X), binary_to_integer(Y)}
    end, binary:split(RawCoords, <<"\n">>, [global, trim_all])),
    Folds = lists:map(fun (C) ->
        [<<"fold along ", Axis>>, V] = binary:split(C, <<"=">>),
        {[Axis], binary_to_integer(V)}
    end, binary:split(RawFolds, <<"\n">>, [global, trim_all])),
    {Coords, Folds}.