-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    dijkstra(input(), {0, 0}, {99, 99}).

part2() ->
    dijkstra(enlarge_grid(input()), {0, 0}, {499, 499}).

dijkstra(Grid, Start, End) ->
    Routes = gb_sets:singleton({0, Start}),
    dijkstra(Grid, Start, End, Routes, maps:new()).

dijkstra(Grid, Coord, End, Routes, Seen) ->
    case gb_sets:take_smallest(Routes) of
        {{Risk, End}, _} -> Risk;
        {{Risk, Node}, CurrentBestRoute} ->
            Adjacents = adjacents(Node, End),
            Unseen = lists:filter(fun (N) -> not maps:is_key(N, Seen) end, Adjacents),
            NextRoutes = lists:foldl(fun (N, Route) ->
                NewRisk = maps:get(N, Grid) + Risk,
                gb_sets:add_element({NewRisk, N}, Route)
            end, CurrentBestRoute, Unseen),
            dijkstra(Grid, Coord, End, NextRoutes, maps:put(Node, true, Seen))
    end.

adjacents({X, Y}, {MaxX, MaxY}) ->
    [{Xn, Yn} || {Xn, Yn} <- [ {X - 1, Y}, {X + 1, Y}, {X, Y - 1}, {X, Y + 1} ],
                            Xn >= 0, Yn >= 0,
                            Yn =< MaxY, Xn =< MaxX
    ].

enlarge_grid(Grid) ->
    lists:foldl(fun ({{X, Y}, Risk}, NewGrid) ->
        Offsets = [ {Xo, Yo} || Xo <- lists:seq(0,4), Yo <- lists:seq(0,4)],
        lists:foldl(fun ({OffsetX, OffsetY}, G) ->
            NewRisk = case Risk + OffsetX + OffsetY of
                Sum when Sum > 9 -> Sum - 9;
                Sum -> Sum
            end,
            maps:put({X + (OffsetX * 100), Y + (OffsetY * 100)}, NewRisk, G)
        end, NewGrid, Offsets)
    end, #{}, maps:to_list(Grid)).

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global, trim_all]),
    lists:foldl(fun ({Line, Y}, Grid) ->
        lists:foldl(fun ({N, X}, G) ->
            maps:put({X, Y}, list_to_integer([N]), G)
        end, Grid, lists:zip(binary:bin_to_list(Line), lists:seq(0, size(Line) - 1)))
    end, maps:new(), lists:zip(Lines, lists:seq(0, length(Lines) - 1))).