
#include <assert.h>
#include <string.h>

#include "erl_nif.h"


typedef ERL_NIF_TERM ENTERM;


typedef struct
{
    ENTERM atom_ok;
    ENTERM atom_error;
    ENTERM atom_partial;

    ENTERM atom_nomem;
    ENTERM atom_bad_block;

    ErlNifResourceType* res_st;
} cseq_priv;


typedef struct
{
    ErlNifPid     pid;
    ErlNifBinary* tgt;
    size_t        len;
    size_t        si;
    size_t        ti;
} cseq_st;


typedef enum
{
    ST_OK,
    ST_ERROR,
    ST_PARTIAL
} cseq_status;


const unsigned char B64URL_B2A[256] = {
   65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, //   0 -  15
   81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99,100,101,102, //  16 -  31
  103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118, //  32 -  47
  119,120,121,122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 45, 95, //  48 -  63
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, //  64 -  79
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, //  80 -  95
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, //  96 - 111
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 112 - 127
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 128 - 143
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 144 - 159
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 160 - 175
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 176 - 191
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 192 - 207
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 208 - 223
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 224 - 239
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255  // 240 - 255
};

const unsigned char B64URL_A2B[256] = {
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, //   0 -  15
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, //  16 -  31
  255,255,255,255,255,255,255,255,255,255,255,255,255, 62,255,255, //  32 -  47
   52, 53, 54, 55, 56, 57, 58, 59, 60, 61,255,255,255,255,255,255, //  48 -  63
  255,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, //  64 -  79
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,255,255,255,255, 63, //  80 -  95
  255, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, //  96 - 111
   41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,255,255,255,255,255, // 112 - 127
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 128 - 143
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 144 - 159
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 160 - 175
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 176 - 191
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 192 - 207
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 208 - 223
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, // 224 - 239
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255  // 240 - 255
};


#define BYTES_PER_PERCENT 64

static inline int
do_consume_timeslice(ErlNifEnv* env) {
#if(ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 4)
    return enif_consume_timeslice(env, 1);
#else
    return 0;
#endif
}


static inline ENTERM
make_atom(ErlNifEnv* env, const char* name)
{
    ENTERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}


static inline ENTERM
make_ok(ErlNifEnv* env, cseq_priv* priv, ENTERM value)
{
    return enif_make_tuple2(env, priv->atom_ok, value);
}


static inline ENTERM
make_error(ErlNifEnv* env, cseq_priv* priv, ENTERM value)
{
    return enif_make_tuple2(env, priv->atom_error, value);
}


static inline ENTERM
make_bad_block(ErlNifEnv* env, cseq_priv* priv, size_t pos)
{
    ENTERM pterm = enif_make_uint64(env, pos);
    return enif_make_tuple2(env, priv->atom_bad_block, pterm);
}


static inline ENTERM
make_partial(ErlNifEnv* env, cseq_priv* priv, ENTERM value)
{
    return enif_make_tuple2(env, priv->atom_partial, value);
}


static inline int
check_pid(ErlNifEnv* env, cseq_st* st)
{
    ErlNifPid self_pid;
    ENTERM self;
    ENTERM orig;

    enif_self(env, &self_pid);
    self = enif_make_pid(env, &self_pid);
    orig = enif_make_pid(env, &(st->pid));

    if(enif_compare(self, orig) == 0) {
        return 1;
    }

    return 0;
}


static cseq_st*
cseq_st_alloc(ErlNifEnv* env, cseq_priv* priv, ErlNifBinary* src, size_t tlen)
{
    cseq_st* st = enif_alloc_resource(priv->res_st, sizeof(cseq_st));
    if(st == NULL) {
        goto error;
    }

    memset(st, '\0', sizeof(cseq_st));
    enif_self(env, &(st->pid));
    st->len = src->size;
    st->si = 0;
    st->ti = 0;
    st->tgt = (ErlNifBinary*) enif_alloc(sizeof(ErlNifBinary));
    if(st->tgt == NULL) {
        goto error;
    }

    if(!enif_alloc_binary(tlen, st->tgt)) {
        goto error;
    }

    return st;

error:
    if(st != NULL) {
        enif_release_resource(st);
    }
    return NULL;
}


static void
cseq_st_free(ErlNifEnv* env, void* obj)
{
    cseq_st* st = (cseq_st*) obj;

    if(st->tgt != NULL) {
        enif_release_binary(st->tgt);
        enif_free(st->tgt);
    }
}

static ENTERM
cseq_st_enc_ret(ErlNifEnv* env, cseq_st* st, int status)
{
    cseq_priv* priv = (cseq_priv*) enif_priv_data(env);
    ENTERM ret;

    if(status == ST_OK) {
        ret = make_ok(env, priv, enif_make_binary(env, st->tgt));
        enif_free(st->tgt);
        st->tgt = NULL;
    } else if(status == ST_PARTIAL) {
        ret = make_partial(env, priv, enif_make_resource(env, st));
    } else {
        assert(0 == 1 && "invalid status encoder status");
        ret = enif_make_badarg(env);
    }

    enif_release_resource(st);
    return ret;
}

static ENTERM
cseq_st_dec_ret(ErlNifEnv* env, cseq_st* st, int status, ENTERM ret)
{
    cseq_priv* priv = (cseq_priv*) enif_priv_data(env);

    if(status == ST_OK) {
        ret = make_ok(env, priv, enif_make_binary(env, st->tgt));
        enif_free(st->tgt);
        st->tgt = NULL;
    } else if(status == ST_ERROR) {
        ret = make_error(env, priv, ret);
    } else if(status == ST_PARTIAL) {
        ret = make_partial(env, priv, enif_make_resource(env, st));
    } else {
        assert(0 == 1 && "invalid status decoder status");
        ret = enif_make_badarg(env);
    }

    enif_release_resource(st);
    return ret;
}

static int
load(ErlNifEnv* env, void** priv, ENTERM info)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* res;

    cseq_priv* new_priv = (cseq_priv*) enif_alloc(sizeof(cseq_priv));
    if(new_priv == NULL) {
        return 1;
    }

    res = enif_open_resource_type(
            env, NULL, "couch_seq", cseq_st_free, flags, NULL);
    if(res == NULL) {
        return 1;
    }
    new_priv->res_st = res;

    new_priv->atom_ok = make_atom(env, "ok");
    new_priv->atom_error = make_atom(env, "error");
    new_priv->atom_partial = make_atom(env, "partial");

    new_priv->atom_nomem = make_atom(env, "enomem");
    new_priv->atom_bad_block = make_atom(env, "bad_block");

    *priv = (void*) new_priv;

    return 0;
}


static int
reload(ErlNifEnv* env, void** priv, ENTERM info)
{
    return 0;
}


static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ENTERM info)
{
    return 0;
}


static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}


static inline cseq_status
cseq_b64url_encode(ErlNifEnv* env, ErlNifBinary* src, cseq_st* st)
{
    size_t chunk_start = st->si;
    unsigned char c1;
    unsigned char c2;
    unsigned char c3;

    assert(st->si % 3 == 0 && "invalid source index");
    assert(st->ti % 4 == 0 && "invalid target index");

    while(st->si + 3 <= src->size) {
        c1 = src->data[st->si++];
        c2 = src->data[st->si++];
        c3 = src->data[st->si++];

        st->tgt->data[st->ti++] = B64URL_B2A[(c1 >> 2) & 0x3F];
        st->tgt->data[st->ti++] = B64URL_B2A[((c1 << 4) | (c2 >> 4)) & 0x3F];
        st->tgt->data[st->ti++] = B64URL_B2A[((c2 << 2) | (c3 >> 6)) & 0x3F];
        st->tgt->data[st->ti++] = B64URL_B2A[c3 & 0x3F];

        if(st->si - chunk_start > BYTES_PER_PERCENT) {
            if(do_consume_timeslice(env)) {
                return ST_PARTIAL;
            } else {
                chunk_start = st->si;
            }
        }
    }

    if(src->size % 3 == 1) {
        c1 = src->data[st->si];
        st->tgt->data[st->ti++] = B64URL_B2A[(c1 >> 2) & 0x3F];
        st->tgt->data[st->ti++] = B64URL_B2A[(c1 << 4) & 0x3F];
    } else if(src->size % 3 == 2) {
        c1 = src->data[st->si];
        c2 = src->data[st->si+1];
        st->tgt->data[st->ti++] = B64URL_B2A[(c1 >> 2) & 0x3F];
        st->tgt->data[st->ti++] = B64URL_B2A[((c1 << 4) | (c2 >> 4)) & 0x3F];
        st->tgt->data[st->ti++] = B64URL_B2A[(c2 << 2) & 0x3F];
    } else {
        assert(0 == 1 && "Inavlid length calculation");
    }

    return ST_OK;
}


static ENTERM
cseq_b64url_encode_init(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    ErlNifBinary src;
    cseq_priv* priv = (cseq_priv*) enif_priv_data(env);
    cseq_st* st = NULL;
    size_t tlen;
    size_t rem;
    int status;
    ENTERM ret;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_inspect_iolist_as_binary(env, argv[0], &src)) {
        ret = enif_make_badarg(env);
        goto error;
    }

    // The target length is defined as 4 * ceil(src_len/3) but we
    // don't use '=' padding for URLs so we only add exactly the
    // extra bytes we need.
    tlen = (src.size / 3) * 4;
    rem = src.size % 3;
    if(rem == 1) {
        tlen += 2;
    } else if(rem == 2) {
        tlen += 3;
    }

    st = cseq_st_alloc(env, priv, &src, tlen);
    if(st == NULL) {
        ret = make_error(env, priv, priv->atom_nomem);
        goto error;
    }

    status = cseq_b64url_encode(env, &src, st);

    return cseq_st_enc_ret(env, st, status);

error:
    if(st != NULL) {
        enif_release_resource(st);
    }

    return ret;
}


static ENTERM
cseq_b64url_encode_cont(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    ErlNifBinary src;
    cseq_priv* priv = (cseq_priv*) enif_priv_data(env);
    cseq_st* st = NULL;
    int status;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_inspect_iolist_as_binary(env, argv[0], &src)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], priv->res_st, (void**) &st)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, st)) {
        return enif_make_badarg(env);
    }

    if(src.size != st->len) {
        return enif_make_badarg(env);
    }

    status = cseq_b64url_encode(env, &src, st);

    return cseq_st_enc_ret(env, st, status);
}


static inline cseq_status
cseq_b64url_decode(ErlNifEnv* env, ErlNifBinary* src, cseq_st* st, ENTERM* ret)
{
    cseq_priv* priv = (cseq_priv*) enif_priv_data(env);
    size_t chunk_start = st->si;
    unsigned char c1;
    unsigned char c2;
    unsigned char c3;
    unsigned char c4;

    assert(st->si % 4 == 0 && "invalid source index");
    assert(st->ti % 3 == 0 && "invalid target index");

    while(st->si + 4 <= src->size) {
        c1 = B64URL_A2B[src->data[st->si++]];
        c2 = B64URL_A2B[src->data[st->si++]];
        c3 = B64URL_A2B[src->data[st->si++]];
        c4 = B64URL_A2B[src->data[st->si++]];

        if(c1 == 255 || c2 == 255 || c3 == 255 || c4 == 255) {
            *ret = make_bad_block(env, priv, st->si-4);
            return ST_ERROR;
        }

        st->tgt->data[st->ti++] = (c1 << 2) | (c2 >> 4);
        st->tgt->data[st->ti++] = (c2 << 4) | (c3 >> 2);
        st->tgt->data[st->ti++] = (c3 << 6) | c4;

        if(st->si - chunk_start > BYTES_PER_PERCENT) {
            if(do_consume_timeslice(env)) {
                return ST_PARTIAL;
            } else {
                chunk_start = st->si;
            }
        }
    }

    if(src->size % 4 == 2) {
        c1 = B64URL_A2B[src->data[st->si]];
        c2 = B64URL_A2B[src->data[st->si+1]];

        if(c1 == 255 || c2 == 255) {
            *ret = make_bad_block(env, priv, st->si);
            return ST_ERROR;
        }

        st->tgt->data[st->ti++] = (c1 << 2) | (c2 >> 4);
    } else if(src->size % 4 == 3) {
        c1 = B64URL_A2B[src->data[st->si]];
        c2 = B64URL_A2B[src->data[st->si+1]];
        c3 = B64URL_A2B[src->data[st->si+2]];

        if(c1 == 255 || c2 == 255 || c3 == 255) {
            *ret = make_bad_block(env, priv, st->si);
            return ST_ERROR;
        }

        st->tgt->data[st->ti++] = (c1 << 2) | (c2 >> 4);
        st->tgt->data[st->ti++] = (c2 << 4) | (c3 >> 2);
    } else {
        assert(0 == 1 && "invalid source length");
    }

    return ST_OK;
}


static ENTERM
cseq_b64url_decode_init(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    ErlNifBinary src;
    cseq_priv* priv = (cseq_priv*) enif_priv_data(env);
    cseq_st* st = NULL;
    size_t tlen;
    int status;
    ENTERM ret;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_inspect_iolist_as_binary(env, argv[0], &src)) {
        return enif_make_badarg(env);
    }

    // We don't do use '=' padding for URLs so our target length
    // will be the number of blocks of 4 bytes times 3, plus 1 or 2
    // depending on the number of bytes in the last block.
    tlen = (src.size / 4) * 3;
    if(src.size % 4 == 1) {
        ret = enif_make_badarg(env);
        goto error;
    } else if(src.size % 4 == 2) {
        tlen += 1;
    } else if(src.size % 4 == 3) {
        tlen += 2;
    }

    st = cseq_st_alloc(env, priv, &src, tlen);
    if(st == NULL) {
        ret = make_error(env, priv, priv->atom_nomem);
        goto error;
    }

    status = cseq_b64url_decode(env, &src, st, &ret);

    return cseq_st_dec_ret(env, st, status, ret);

error:
    if(st != NULL) {
        enif_release_resource(st);
    }

    return ret;
}


static ENTERM
cseq_b64url_decode_cont(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    ErlNifBinary src;
    cseq_priv* priv = (cseq_priv*) enif_priv_data(env);
    cseq_st* st = NULL;
    ENTERM ret;
    int status;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_inspect_iolist_as_binary(env, argv[0], &src)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], priv->res_st, (void**) &st)) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, st)) {
        return enif_make_badarg(env);
    }

    if(src.size != st->len) {
        return enif_make_badarg(env);
    }

    status = cseq_b64url_decode(env, &src, st, &ret);

    return cseq_st_dec_ret(env, st, status, ret);
}


static unsigned char CHECK_A2B[64] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";


static ENTERM
cseq_b64url_check_tables(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    cseq_priv* priv = (cseq_priv*) enif_priv_data(env);
    ENTERM pos;
    int i;

    for(i = 0; i < 64; i++) {
        if(B64URL_B2A[i] != CHECK_A2B[i]) {
            pos = enif_make_int(env, i);
            return enif_make_tuple2(env, priv->atom_error, pos);
        }
    }

    for(i = 64; i < 256; i++) {
        if(B64URL_B2A[i] != 255) {
            pos = enif_make_int(env, i);
            return enif_make_tuple2(env, priv->atom_error, pos);
        }
    }

    for(i = 0; i < 64; i++) {
        if(B64URL_A2B[CHECK_A2B[i]] != i) {
            pos = enif_make_int(env, i);
            return enif_make_tuple2(env, priv->atom_error, pos);
        }
    }

    for(i = 0; i < 256; i++) {
        if(B64URL_A2B[i] == 255) {
            continue;
        }
        if(CHECK_A2B[B64URL_A2B[i]] != i) {
            pos = enif_make_int(env, i);
            return enif_make_tuple2(env, priv->atom_error, pos);
        }
    }

    return priv->atom_ok;
}

static ErlNifFunc funcs[] = {
    {"encode_init", 1, cseq_b64url_encode_init},
    {"encode_cont", 2, cseq_b64url_encode_cont},
    {"decode_init", 1, cseq_b64url_decode_init},
    {"decode_cont", 2, cseq_b64url_decode_cont},
    {"check_tables", 0, cseq_b64url_check_tables}
};


ERL_NIF_INIT(couch_seqs_b64url, funcs, &load, &reload, &upgrade, &unload);


