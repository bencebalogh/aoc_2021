-module(aoc).
-export([part1/0,part2/0,decode/1]).

part1() ->
    Lenghts = [2, 3, 4, 7],
    length(lists:filter(
        fun (O) -> lists:member(size(O), Lenghts) end,
    lists:flatmap(fun ({_, Output}) -> Output end, input()))).

part2() ->
    lists:sum(lists:map(fun decode/1, input())).

decode({Mapping, Output}) ->
    SortedMapping = lists:map(fun (O) -> lists:sort(binary:bin_to_list(O)) end, Mapping),
    One = lists:sort(get_mapping_by_length(2, SortedMapping)),
    Seven = lists:sort(get_mapping_by_length(3, SortedMapping)),
    Four = lists:sort(get_mapping_by_length(4, SortedMapping)),
    Eight = lists:sort(get_mapping_by_length(7, SortedMapping)),
    SortedOutput = lists:map(fun (O) -> lists:sort(binary:bin_to_list(O)) end, Output),
    [RT, RB, T, M, LT, LB, B] = find_positions(One, Seven, Four, Eight, Mapping),
    list_to_integer(lists:concat(lists:map(fun
        (Code) when Code == One -> "1";
        (Code) when Code == Four -> "4";
        (Code) when Code == Seven -> "7";
        (Code) when Code == Eight -> "8";
        (Code) ->
            case length(Code) of
                6 ->
                    Zero = lists:all(fun (C) -> lists:member(C, [T, RT, RB, LT, LB, B]) end, Code),
                    Six = lists:all(fun (C) -> lists:member(C, [T, LT, LB, M, RB, B]) end, Code),
                    Nine = lists:all(fun (C) -> lists:member(C, [T, LT, RT, RB, M, B]) end, Code),
                    case {Zero, Six, Nine} of
                        {true, false, false} -> "0";
                        {false, true, false} -> "6";
                        {false, false, true} -> "9"
                    end;
                5 ->
                    Two = lists:all(fun (C) -> lists:member(C, [T, RT, M, LB, B]) end, Code),
                    Three = lists:all(fun (C) -> lists:member(C, [T, RT, M, RB, B]) end, Code),
                    Five = lists:all(fun (C) -> lists:member(C, [T, LT, M, RB, B]) end, Code),
                    case {Two, Three, Five} of
                        {true, false, false} -> "2";
                        {false, true, false} -> "3";
                        {false, false, true} -> "5"
                    end
            end
    end, SortedOutput))).

find_positions([RT, RB], Top, Middle, All, Mapping) ->
    case count(Mapping, RB) of
        9 ->
            [T] = lists:delete(RB, lists:delete(RT, Top)),
            RemMiddle = lists:delete(RT, lists:delete(RB, Middle)),
            [M] = lists:filter(fun (Rm) -> count(Mapping, Rm) =:= 7 end, RemMiddle),
            [LT] = lists:filter(fun (Rm) -> count(Mapping, Rm) =:= 6 end, RemMiddle),
            Rem = lists:foldl(fun (C, A) -> lists:delete(C, A) end, All, [RT, RB, LT, M, T]),
            [LB] = lists:filter(fun (Rm) -> count(Mapping, Rm) =:= 4 end, Rem),
            [B] = lists:filter(fun (Rm) -> count(Mapping, Rm) =:= 7 end, Rem),
            [RT, RB, T, M, LT, LB, B];
        _ ->
            find_positions([RB, RT], Top, Middle, All, Mapping)
    end.

count(Mapping, C) ->
    length(lists:filter(fun (M) -> lists:member(C, binary:bin_to_list(M)) end, Mapping)).

get_mapping_by_length(Length, Mapping) ->
    {value, M} = lists:search(fun (L) -> length(L) =:= Length end, Mapping),
    M.

input() ->
    {ok, I} = file:read_file(input),
    lists:map(fun (L) ->
        [Pre, Post] = binary:split(L, <<"|">>, [global, trim_all]),
        {binary:split(Pre, <<" ">>, [global, trim_all]), binary:split(Post, <<" ">>, [global, trim_all])}
    end, binary:split(I, <<"\n">>, [global, trim_all])).