-module(couch_seqs_b64url_tests).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


proper_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 6401},
        {numtests, 1000}
    ],
    {timeout, 3600, ?_assertEqual([], proper:module(?MODULE, PropErOpts))}.


prop_encode_binary() ->
    ?FORALL(Bin, binary(), begin
        A = couch_encode_base64url(Bin),
        B = couch_seqs_b64url:encode(Bin),
        A == B
    end).


prop_encode_iolist() ->
    ?FORALL(Bin, binary(), begin
        A = couch_encode_base64url(Bin),
        B = couch_seqs_b64url:encode(to_iolist(Bin)),
        A == B
    end).


couch_encode_base64url(Url) ->
    Url1 = iolist_to_binary(re:replace(base64:encode(Url), "=+$", "")),
    Url2 = iolist_to_binary(re:replace(Url1, "/", "_", [global])),
    iolist_to_binary(re:replace(Url2, "\\+", "-", [global])).


couch_decode_base64url(Url64) ->
    Url1 = re:replace(iolist_to_binary(Url64), "-", "+", [global]),
    Url2 = iolist_to_binary(
        re:replace(iolist_to_binary(Url1), "_", "/", [global])
    ),
    Padding = list_to_binary(lists:duplicate((4 - size(Url2) rem 4) rem 4, $=)),
    base64:decode(<<Url2/binary, Padding/binary>>).


to_iolist(<<>>) ->
    case random:uniform(2) of
        1 -> <<>>;
        2 -> [<<>>]
    end;
to_iolist(B) when is_binary(B), size(B) > 0 ->
    S = random:uniform(size(B)),
    <<First:S/binary, Second/binary>> = B,
    case random:uniform(3) of
        1 ->
            [to_iolist(First), Second];
        2 ->
            [First, to_iolist(Second)];
        3 ->
            [First, Second]
    end.


