-module(b64url_tests).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


table_test_() ->
    ?_assertEqual(ok, b64url:check_tables()).


proper_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 6401},
        {numtests, 500}
    ],
    {timeout, 3600, ?_assertEqual([], proper:module(?MODULE, PropErOpts))}.


prop_encode_binary() ->
    ?FORALL(Bin, binary(), begin
        A = couch_encode_base64url(Bin),
        B = b64url:encode(Bin),
        A == B
    end).


prop_encode_iolist() ->
    ?FORALL(IoList, shallow_iolist(), begin
        A = couch_encode_base64url(iolist_to_binary(IoList)),
        B = b64url:encode(IoList),
        A == B
    end).


prop_decode_binary() ->
    ?FORALL(Bin, binary(), begin
        B64UrlBin = couch_encode_base64url(Bin),
        Dec = b64url:decode(B64UrlBin),
        Dec == Bin
    end).


prop_decode_iolist() ->
    ?FORALL(IoList, shallow_b64_iolist(), begin
        A = couch_decode_base64url(iolist_to_binary(IoList)),
        B = b64url:decode(IoList),
        A == B
    end).


prop_decode_binary_error() ->
    ?FORALL({ErrBin, BlockPos}, bad_binary(), begin
        Dec = b64url:decode(ErrBin),
        Dec == {error, {bad_block, BlockPos}}
    end).


prop_decode_bad_length() ->
    ?FORALL(Bin, bad_len_binary(), begin
        try
            b64url:decode(Bin),
            false
        catch error:badarg ->
            true
        end
    end).


shallow_iolist() ->
    ?LET(Bin, binary(), to_iolist(Bin)).


shallow_b64_iolist() ->
    ?LET(Bin, binary(), to_iolist(couch_encode_base64url(Bin))).


bad_binary() ->
    ?LET(Bin, binary(), insert_error(Bin)).


bad_len_binary() ->
    ?LET(Bin, binary(), make_bad_len(Bin)).


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


insert_error(B) when is_binary(B), size(B) < 2 ->
    case random:uniform(2) of
        1 -> {<<122, 255>>, 0};
        2 -> {<<122, 122, 255>>, 0}
    end;
insert_error(B) when is_binary(B) ->
    B64 = couch_encode_base64url(B),
    S = random:uniform(size(B64)-1),
    <<First:S/binary, _:1/binary, Second/binary>> = B64,
    {<<First:S/binary, 255, Second/binary>>, 4 * (S div 4)}.


make_bad_len(Bin) when size(Bin) rem 4 == 1 ->
    Bin;
make_bad_len(Bin) when size(Bin) rem 4 == 2 ->
    <<"AAA", Bin/binary>>;
make_bad_len(Bin) when size(Bin) rem 4 == 3 ->
    <<"AA", Bin/binary>>;
make_bad_len(Bin) when size(Bin) rem 4 == 0 ->
    <<"A", Bin/binary>>.


% These functions are copy/pasted from couch_util to avoid
% the direct dependency. The goal of this project is to replace
% these in couch_util anyway so when that happens they'll only
% exist here for these tests.


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
