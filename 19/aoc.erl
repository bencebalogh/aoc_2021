-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    Readings = input(),
    {Beacons, _} = mapping([hd(Readings)], tl(Readings), [], []),
    length(Beacons).

part2() ->
    Readings = input(),
    {_, Scanners} = mapping([hd(Readings)], tl(Readings), [], []),
    lists:max(lists:flatmap(fun (Scanner1) ->
        lists:map(fun (Scanner2) -> distance(Scanner1, Scanner2) end, Scanners)
    end, Scanners)).

rotations() ->
    lists:flatmap(fun (Facing) ->
        lists:foldl(fun (P, Agg) ->
            case determinant(P) of
                1 -> [P | Agg];
                _ -> Agg
            end
        end, [], perms(Facing))
    end, [[[X, 0, 0], [0, Y, 0], [0, 0, Z]] || X <- [-1, 1], Y <- [-1, 1], Z <- [-1, 1]]).

determinant([[A, B, C], [D, E, F], [G, H, I]]) ->
    A * (E * I - F * H) - B * (D * I - F * G) + C *(D * H - E * G).

rotate({A, B, C}, [[J, K, L],[M, N, O], [P, Q, R]]) ->
    {A * J + B * K + C * L, A * M + B * N  + C * O, A * P + B * Q + C * R}.

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

find_scanner(_, _, []) -> false;
find_scanner(Readings1, Readings2, [Rotation | RestRotations]) ->
    Rotated2 = lists:map(fun (R) -> rotate(R, Rotation) end, Readings2),
    case matches_12(Readings1, Rotated2) of
        [] ->
            find_scanner(Readings1, Readings2, RestRotations);
        [Scanner] ->
            {Scanner, lists:map(fun (R) -> add(R, Scanner) end, Rotated2)}
    end.

mapping([], [], BeaconsFound, ScannersFound) ->
    {sets:to_list(sets:from_list(lists:concat(BeaconsFound))), ScannersFound};
mapping([H | T], Scanners, BeaconsFound, ScannersFound) ->
    {Follow, Retry, NewScannersFound} = rotate_and_find(H, Scanners, ScannersFound),
    mapping(T ++ Follow, Retry, [H | BeaconsFound], NewScannersFound).

rotate_and_find(Readings1, Scanners, ScannersFound) ->
    lists:foldl(fun (Readings2, {Follow, Retry, NewScanners}) ->
        case find_scanner(Readings1, Readings2, rotations()) of
            false -> {Follow, [Readings2 | Retry], NewScanners};
            {Scanner, Rotated2} -> {[Rotated2 | Follow], Retry, [Scanner | NewScanners]}
        end
    end, {[], [], ScannersFound}, Scanners).

matches_12(Readings1, Readings2) ->
    Differences = lists:flatmap(fun ({X1, Y1, Z1}) ->
        lists:map(fun ({X2, Y2, Z2}) -> {X2 - X1, Y2 - Y1, Z2 - Z1} end, Readings1)
    end, Readings2),
    Freq = lists:foldl(fun (D, M) ->
        maps:update_with(D, fun (V) -> V + 1 end, 1, M)
    end, #{}, Differences),
    [Diff || {Diff, Count} <- maps:to_list(Freq), Count >= 12]. 

add({X1, Y1, Z1}, {X2, Y2, Z2}) -> {X1 + X2, Y1 + Y2, Z1 + Z2}.
distance({X1, Y1, Z1}, {X2, Y2, Z2}) -> abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2).

input() ->
    {ok, I} = file:read_file(input),
    lists:foldl(fun
        (<<"--- scanner", _/binary>>, []) ->
            [];
        (Coords, []) ->
            [X, Y, Z] = binary:split(Coords, <<",">>, [global, trim_all]),
            [[{binary_to_integer(X), binary_to_integer(Y), binary_to_integer(Z)}]];
        (<<"--- scanner", _/binary>>, [H | T]) ->
            [[] | [H | T]];
        (Coords, [H | T]) ->
            [X, Y, Z] = binary:split(Coords, <<",">>, [global, trim_all]),
            [[{binary_to_integer(X), binary_to_integer(Y), binary_to_integer(Z)} | H] | T]
    end, [], binary:split(I, <<"\n">>, [global, trim_all])).