-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    {Boards, Draws} = input(),
    p(Draws, Boards, 0, false).

part2() ->
    {Boards, Draws} = input(),
    p(Draws, Boards, 0, true).

p([], _, LastWinnerScore, _) -> LastWinnerScore;
p([H | T], Boards, LastWinnerScore, Continue) ->
    Next = lists:map(fun ({Board, Marking}) ->
                             case maps:find(H, Board) of
                                 {ok, {X, Y}} -> {Board, maps:put({X, Y}, H, Marking)};
                                 _ -> {Board, Marking}
                             end
                     end, Boards),
    Winners = lists:filter(fun (B) -> is_winner(B) end, Next),
    case Winners of
        [Winner] ->
            case Continue of
                true -> p(T, lists:filter(fun (B) -> not is_winner(B) end, Next), score(Winner, H), Continue);
                false -> score(Winner, H)
            end;
        _ -> p(T, lists:filter(fun (B) -> not is_winner(B) end, Next), LastWinnerScore, Continue)
    end.

score({Board, Markings}, Draw) ->
    Draw * lists:sum(lists:map(fun ({V, _}) -> V end, lists:filter(fun ({_, Pos}) -> not maps:is_key(Pos, Markings) end, maps:to_list(Board)))).

is_winner({_, Marked}) ->
    V = lists:search(fun (X) ->
                             lists:all(fun (Y) -> maps:is_key({X, Y}, Marked) end, lists:seq(0, 4))
                     end, lists:seq(0, 4)) =/= false,
    H = lists:search(fun (Y) ->
                             lists:all(fun (X) -> maps:is_key({X, Y}, Marked) end, lists:seq(0, 4))
                     end, lists:seq(0, 4)) =/= false,
    H or V.

input() ->
    {ok, I} = file:read_file(input),
    [H | T] = binary:split(I, <<"\n">>, [global, trim_all]),
    Boards = get_boards(lists:flatmap(fun (Row) -> binary:split(Row, <<",">>, [global, trim_all]) end, T)),
    Draws = lists:map(fun (K) -> binary_to_integer(K) end, binary:split(H, <<",">>, [global, trim_all])),
    {lists:map(fun (B) -> {B, maps:new()} end, Boards), Draws}.

get_boards(Boards) -> get_boards(Boards, [], 0).

get_boards([], Acc, _) -> format_boards(lists:map(fun lists:reverse/1 , Acc));
get_boards([H | T], Acc, Index) when Index rem 5 == 0 -> get_boards(T, [[H] | Acc], Index + 1);
get_boards([H | T], [HAcc | TAcc], Index) -> get_boards(T, [[H | HAcc] | TAcc], Index + 1).

format_boards(RawBoards) ->
    P = lists:map(fun (Raw) ->
                          lists:map(fun (R) ->
                                            lists:map(fun (I) ->
                                                              binary_to_integer(I)
                                                      end, binary:split(R, <<" ">>, [global, trim_all]))
                                    end, Raw)
                  end, RawBoards),
    lists:map(fun (Pi) ->
                      lists:foldl(fun ({Y, Row}, Board) ->
                                          lists:foldl(fun ({X, V}, Bboard) ->
                                                              maps:put(V, {X, Y}, Bboard)
                                                      end, Board, lists:zip(lists:seq(0, length(Row) - 1), Row))
                                  end, maps:new(), lists:zip(lists:seq(0, length(Pi) - 1), Pi))
              end, P).
