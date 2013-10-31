-module(couch_seqs_b64url).
-on_load(init/0).


-export([
    encode/1,
    decode/1
]).


-define(NOT_LOADED, not_loaded(?LINE)).


-spec encode(iodata()) -> binary().
encode(IoData) ->
    case encode_init(IoData) of
        {ok, Bin} ->
            Bin;
        {partial, St} ->
            encode_loop(IoData, St)
    end.


-spec decode(iodata()) -> binary() | {error, any()}.
decode(IoData) ->
    case decode_init(IoData) of
        {ok, Bin} ->
            Bin;
        {partial, St} ->
            decode_loop(IoData, St)
    end.


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "couch_seqs_b64"), 0).


encode_loop(IoData, St) ->
    case encode_cont(IoData, St) of
        {ok, Bin} ->
            Bin;
        {partial, St} ->
            encode_loop(IoData, St)
    end.


decode_loop(IoData, St) ->
    case decode_cont(IoData, St) of
        {ok, Bin} ->
            Bin;
        {partial, St} ->
            decode_loop(IoData, St)
    end.


encode_init(_) -> ?NOT_LOADED.
encode_cont(_, _) -> ?NOT_LOADED.
decode_init(_) -> ?NOT_LOADED.
decode_cont(_, _) -> ?NOT_LOADED.


not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

