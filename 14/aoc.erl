-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    p(10).

part2() ->
    p(40).

p(Steps) ->
    {Start, Rules} = input(),
    {_, Counts} = lists:foldl(fun (_,  M) -> evolve(M, Rules) end, get_maps(Start, #{}, #{}), lists:seq(0, Steps - 1)),
    Freq = maps:values(Counts),
    lists:max(Freq) - lists:min(Freq).

evolve({Pairs, Counts}, Rules) ->
    lists:foldl(fun ([PairH1, PairH2] = Pair, {P, C}) ->
        ToAdd = maps:get(Pair, Rules),
        PairCount = maps:get(Pair, Pairs),
        NewCounts = increment_by(ToAdd, PairCount, C),
        NewPairs = increment_by([ToAdd, PairH2], PairCount, increment_by([PairH1, ToAdd], PairCount, P)),
        {NewPairs, NewCounts}
    end, {#{}, Counts}, maps:keys(Pairs)).

get_maps([H | []], Pairs, Counts) ->
    {Pairs, increment_by(H, 1, Counts)};
get_maps([H1 | [H2 | T]], Pairs, Counts) ->
    get_maps([H2 | T], increment_by([H1, H2], 1, Pairs), increment_by(H1, 1, Counts)).

increment_by(Key, By, Map) ->
    maps:update_with(Key, fun (V) -> V + By end, By, Map).

input() ->
    {ok, I} = file:read_file(input),
    [Start, Rules] = binary:split(I, <<"\n\n">>, [global, trim_all]),
    R = lists:foldl(fun (C, M) ->
        [From, To] = binary:split(C, <<" -> ">>),
        maps:put(binary_to_list(From), hd(binary_to_list(To)), M)
    end, maps:new(), binary:split(Rules, <<"\n">>, [global, trim_all])),
    {binary:bin_to_list(Start), R}.
