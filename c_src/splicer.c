/*
 * Copyright 2018 Helium Systems Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "erl_nif.h"
#include <fcntl.h>
#include <sys/ioctl.h>
#include <unistd.h>

static ErlNifResourceType *SPLICER_RESOURCE;

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;

struct splicer_pipe {
    int pipefd[2];
};

static ERL_NIF_TERM
splice2(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int fd1, fd2;
    if (!enif_get_int(env, argv[0], &fd1) || !enif_get_int(env, argv[1], &fd2)) {
        return enif_make_badarg(env);
    }

    // allocate a resource to hold our pipe FDs
    struct splicer_pipe* mypipe = enif_alloc_resource(SPLICER_RESOURCE, sizeof(struct splicer_pipe));
    int res = pipe(mypipe->pipefd);
    if (res != 0) {
        enif_release_resource(mypipe);
        return ATOM_ERROR;
    }
    ERL_NIF_TERM term = enif_make_resource(env, mypipe);
    return enif_make_tuple2(env, ATOM_OK, term);
}

static ERL_NIF_TERM
splice3(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int fd1, fd2, infd, outfd;
    struct splicer_pipe *pipe;
    if (!enif_get_int(env, argv[0], &fd1) || !enif_get_int(env, argv[1], &fd2)) {
        return enif_make_badarg(env);
    }

    int bytes;
    infd = fd1;
    outfd = fd2;
    ioctl(fd1, FIONREAD, &bytes);
    if (bytes == 0) {
        ioctl(fd2, FIONREAD, &bytes);
        if (bytes == 0) {
            return enif_make_tuple2(env, ATOM_OK, argv[2]);
        }
        infd = fd2;
        outfd = fd1;
    }

    if (!enif_get_resource(env, argv[2], SPLICER_RESOURCE, (void**)&pipe)) {
        return enif_make_badarg(env);
    }

    ssize_t res = splice(infd, NULL, pipe->pipefd[1], NULL, bytes, SPLICE_F_MOVE | SPLICE_F_MORE);
    if (res == -1) {
        return ATOM_ERROR;
    }
    res = splice(pipe->pipefd[0], NULL, outfd, 0, bytes, SPLICE_F_MOVE | SPLICE_F_MORE);
    if (res == -1) {
        return ATOM_ERROR;
    }

    return enif_make_tuple2(env, ATOM_OK, argv[2]);
}


static ErlNifFunc nif_funcs[] =
    {{"splice_int", 2, splice2, 0},
     {"splice_int", 3, splice3, 0}};

#define ATOM(Id, Value)                                                        \
    {                                                                          \
        Id = enif_make_atom(env, Value);                                       \
    }

static int
load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;

    SPLICER_RESOURCE = enif_open_resource_type(env, NULL, "splicer_pipe", NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    ATOM(ATOM_OK, "ok");
    ATOM(ATOM_ERROR, "error");

    return 0;
}

ERL_NIF_INIT(splicer, nif_funcs, load, NULL, NULL, NULL);
