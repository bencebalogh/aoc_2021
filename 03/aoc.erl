-module(aoc).
-export([part1/0,part2/0]).

part1() ->
    Input = input(),
    Max = size(hd(input())),
    binary_to_integer(p1(Input, 0, Max, {49, 48}, <<"">>), 2) * binary_to_integer(p1(Input, 0, Max, {48, 49}, <<"">>), 2).

part2() ->
    Input = input(),
    binary_to_integer(p2(Input, 0, {49, 48}), 2) * binary_to_integer(p2(Input, 0, {48, 49}), 2).

p1(_, Max, Max, _, R) -> R;
p1(Values, Index, Max, Nrs, R) -> p1(Values, Index + 1, Max, Nrs, <<R/binary, (most_common(Values, Index, Nrs))>>).

p2(Values, Index, Nrs) ->
    V = most_common(Values, Index, Nrs),
    case lists:filter(fun (Val) -> binary:at(Val, Index) == V end, Values) of
        [X] -> X;
        Arr -> p2(Arr, Index + 1, Nrs)
    end.

most_common(Values, Index, {Nr1, Nr2}) ->
    Half = length(Values) / 2,
    case lists:sum(lists:map(fun (Reading) -> binary:at(Reading, Index) rem 48 end, Values)) of
        I when I >= Half -> Nr1;
        _ -> Nr2
    end.

input() ->
    {ok, I} = file:read_file(input),
    binary:split(I, <<"\n">>, [global, trim_all]).
