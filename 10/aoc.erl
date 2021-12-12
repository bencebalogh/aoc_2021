-module(aoc).
-export([part1/0,part2/0]).
-define(OPENS, ["(", "{", "<", "["]).
-define(CLOSINGS, [")", "}", ">", "]"]).
-define(PAIRINGS, lists:zip(?OPENS, ?CLOSINGS)).

part1() ->
    lists:sum(lists:map(fun
        ({invalid, ")"}) -> 3;
        ({invalid, "]"}) -> 57;
        ({invalid, "}"}) -> 1197;
        ({invalid, ">"}) -> 25137;
        (_) -> 0
    end, lists:map(fun validate/1, input()))).

part2() ->
    Scores = [ score_line(finish_line(R, []), 0) || {incomplete, R} <- lists:map(fun validate/1, input())],
    Index = length(Scores) div 2 + 1,
    lists:nth(Index, lists:sort(Scores)).

validate(Line) -> validate(Line, []).

validate([], []) -> valid;
validate([], Remainder) -> {incomplete, Remainder};
validate([Next | Rest], Openings) ->
    case {lists:member([Next], ?OPENS), lists:member([Next], ?CLOSINGS)} of
        {true, false} ->
            validate(Rest, [Next | Openings]);
        {false, true} ->
            case lists:member({[hd(Openings)], [Next]}, ?PAIRINGS) of
                true -> validate(Rest, tl(Openings));
                false -> {invalid, [Next]}
            end
    end.

finish_line([], Acc) -> lists:reverse(Acc);
finish_line([H | T], Acc) -> finish_line(T, [lists:nth(string:str(?OPENS, [[H]]), ?CLOSINGS) | Acc]).

score_line([], Acc) -> Acc;
score_line([H | T], Acc) ->
    Score = case H of
        ")" -> 1;
        "]" -> 2;
        "}" -> 3;
        ">" -> 4
    end,
    score_line(T, Acc * 5 + Score).

input() ->
    {ok, I} = file:read_file(input),
    [ binary:bin_to_list(L) || L <- binary:split(I, <<"\n">>, [global, trim_all])].