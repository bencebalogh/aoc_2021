-module(aoc).
-export([part1/0,part2/0]).
-define(PLAYER1, 7).
-define(PLAYER2, 6).

part1() ->
    turn({?PLAYER1, 0}, {?PLAYER2, 0}, 1, 1).

part2() ->
    {Wins, _} = p(?PLAYER1, ?PLAYER2, 0, 0, #{}),
    lists:max(Wins).

p(_, _, _, Score2, Wins) when Score2 >= 21 ->
    {[0, 1], Wins};
p(Player1, Player2, Score1, Score2, Wins) when is_map_key({Player1, Player2, Score1, Score2}, Wins) ->
    {maps:get({Player1, Player2, Score1, Score2}, Wins), Wins};
p(Player1, Player2, Score1, Score2, Wins) ->
    lists:foldl(fun ({Move, Count}, {[SumA, SumB], Cache}) ->
        NextP1 = move(Player1, Move),
        NextS1 = Score1 + NextP1,
        {[C1, C2], NewCache} = p(Player2, NextP1, Score2, NextS1, Cache),
        [WinA, WinB] = [C2 * Count, C1 * Count],
        {[SumA + WinA, SumB + WinB], maps:put({Player1, Player2, Score1, Score2}, [SumA + WinA, SumB + WinB], NewCache)}
    end, {[0, 0], Wins}, maps:to_list(roll_options())).

roll_options() ->
    Rolls = lists:flatmap(fun (R1) ->
        lists:flatmap(fun (R2) ->
            lists:map(fun (R3) ->
                [R1,R2,R3]
            end, [1,2,3])
        end, [1,2,3])
    end, [1,2,3]),
    lists:foldl(fun (I, M) ->
        maps:update_with(I, fun (V) -> V + 1 end, 1, M)
    end, #{}, lists:map(fun lists:sum/1, Rolls)).

turn({P1Position, P1Score}, {P2Position, P2Score}, Dice, Turn) ->
    Move1 = Dice + incr_dice(Dice, 1) + incr_dice(Dice, 2),
    case move(P1Position, Move1) of
        P1 when P1 + P1Score >= 1000 ->
            P2Score * (Turn * 6 - 3);
        P1 -> 
            Move2 = incr_dice(Dice, 3) + incr_dice(Dice, 4) + incr_dice(Dice, 5),
            case move(P2Position, Move2) of
                P2 when P2 + P2Score >= 1000 ->
                    P1Score * Turn * 6;
                P2 ->
                    turn({P1, P1Score + P1}, {P2, P2Score + P2}, incr_dice(Dice, 6), Turn + 1)
            end
    end.

move(Pos, Move) -> (((Pos + Move) - 1) rem 10) + 1.

incr_dice(Dice, I) when Dice + I > 100 -> (Dice + I) rem 100;
incr_dice(Dice, I) -> Dice + I.