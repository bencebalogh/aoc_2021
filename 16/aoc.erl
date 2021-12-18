-module(aoc).
-export([part1/0,part2/0]).
-define(INPUT, <<"4057231006FF2D2E1AD8025275E4EB45A9ED518E5F1AB4363C60084953FB09E008725772E8ECAC312F0C18025400D34F732333DCC8FCEDF7CFE504802B4B00426E1A129B86846441840193007E3041483E4008541F8490D4C01A89B0DE17280472FE937C8E6ECD2F0D63B0379AC72FF8CBC9CC01F4CCBE49777098D4169DE4BF2869DE6DACC015F005C401989D0423F0002111723AC289DED3E64401004B084F074BBECE829803D3A0D3AD51BD001D586B2BEAFFE0F1CC80267F005E54D254C272950F00119264DA7E9A3E9FE6BB2C564F5376A49625534C01B0004222B41D8A80008446A8990880010A83518A12B01A48C0639A0178060059801C404F990128AE007801002803AB1801A0030A280184026AA8014C01C9B005CE0011AB00304800694BE2612E00A45C97CC3C7C4020A600433253F696A7E74B54DE46F395EC5E2009C9FF91689D6F3005AC0119AF4698E4E2713B2609C7E92F57D2CB1CE0600063925CFE736DE04625CC6A2B71050055793B4679F08CA725CDCA1F4792CCB566494D8F4C69808010494499E469C289BA7B9E2720152EC0130004320FC1D8420008647E8230726FDFED6E6A401564EBA6002FD3417350D7C28400C8C8600A5003EB22413BED673AB8EC95ED0CE5D480285C00372755E11CCFB164920070B40118DB1AE5901C0199DCD8D616CFA89009BF600880021304E0EC52100623A4648AB33EB51BCC017C0040E490A490A532F86016CA064E2B4939CEABC99F9009632FDE3AE00660200D4398CD120401F8C70DE2DB004A9296C662750663EC89C1006AF34B9A00BCFDBB4BBFCB5FBFF98980273B5BD37FCC4DF00354100762EC258C6000854158750A2072001F9338AC05A1E800535230DDE318597E61567D88C013A00C2A63D5843D80A958FBBBF5F46F2947F952D7003E5E1AC4A854400404A069802B25618E008667B7BAFEF24A9DD024F72DBAAFCB312002A9336C20CE84">>).

part1() ->
    {P, _} = parse(hex_to_binary(?INPUT)),
    sum_versions(P).

part2() ->
    {P, _} = parse(hex_to_binary(?INPUT)),
    value(P).

sum_versions({V, _, SubPackets}) when is_list(SubPackets) ->
    V + lists:foldl(fun (P, S) -> S + sum_versions(P) end, 0, SubPackets);
sum_versions({V, _, _}) -> V.

value({_, 0, SubPackets}) -> lists:sum(lists:map(fun value/1, SubPackets));
value({_, 1, SubPackets}) -> lists:foldl(fun (P, S) -> P * S end, 1, lists:map(fun value/1, SubPackets));
value({_, 2, SubPackets}) -> lists:min(lists:map(fun value/1, SubPackets));
value({_, 3, SubPackets}) -> lists:max(lists:map(fun value/1, SubPackets));
value({_, 5, [P1, P2]}) -> case value(P1) < value(P2) of true -> 1; _ -> 0 end;
value({_, 6, [P1, P2]}) -> case value(P1) > value(P2) of true -> 1; _ -> 0 end;
value({_, 7, [P1, P2]}) -> case value(P1) =:= value(P2) of true -> 1; _ -> 0 end;
value({_, 4, V}) -> V.

parse(Packet) ->
    [V1,V2,V3, T1, T2, T3 | Rest] = Packet,
    Version =  list_to_integer([V1, V2, V3], 2),
    Type = list_to_integer([T1, T2, T3], 2),
    {Value, Remainder} = case Type of
        4 ->
            parse_literal(Rest, []);
        _ ->
            parse_operator(Rest, [])
    end,
    {{Version, Type, Value}, Remainder}.


parse_operator([48 | T], []) ->
    {Length, Rest} = lists:split(15, T),
    {ToParse, Remaining} = lists:split(list_to_integer(Length, 2), Rest),
    {parse_packets(ToParse, []), Remaining};
parse_operator([49 | T], []) ->
    {Count, Remaining} = lists:split(11, T),
    lists:foldl(fun (_, {Parsed, Raw}) ->
        {P, Remainder} = parse(Raw),
        {[P | Parsed], Remainder}
    end, {[], Remaining}, lists:seq(1, list_to_integer(Count, 2))).

parse_packets(Raw, Parsed) ->
    case length(Raw) < 6 of
        true -> Parsed;
        false ->
            {P, Remainder} = parse(Raw),
            parse_packets(Remainder, [P | Parsed])
    end.

parse_literal([48, B1, B2, B3, B4 | T], Agg) -> {list_to_integer(lists:reverse([B4, B3, B2, B1 | Agg]), 2), T};
parse_literal([49, B1, B2, B3, B4 | Rest], Agg) ->
    parse_literal(Rest, [B4 ,B3 ,B2, B1 | Agg]).

hex_to_binary(Hex) ->
    L = integer_to_list(binary_to_integer(Hex, 16), 2),
    case length(L) rem 4 of
        0 -> L;
        I -> lists:duplicate(4 - I, 48) ++ L
    end.