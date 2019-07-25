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
#include <sys/epoll.h>

#define PIPE_SIZE 4096

static ErlNifResourceType *SPLICER_RESOURCE;

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;

struct splicer_pipe {
    int timeout;
    int fd1;
    int fd2;
    int pipefd[2];
    ErlNifTid tid;
    ErlNifPid dst;
    ErlNifEnv* env;
    ErlNifThreadOpts* opts;
    ERL_NIF_TERM ref;
};

void* splicer_run(void*);

void* splicer_run(void *obj)
{

    struct splicer_pipe *mypipe = (struct splicer_pipe*)obj;

    int epfd = epoll_create(2);
    if (epfd < 0) {
        goto done;
    }

    struct epoll_event event, events[2];
    event.data.fd = mypipe->fd1;
    event.events = EPOLLIN;
    int ret = epoll_ctl(epfd, EPOLL_CTL_ADD, mypipe->fd1, &event);
    if (ret) {
        goto done;
    }

    event.data.fd = mypipe->fd2;
    ret = epoll_ctl(epfd, EPOLL_CTL_ADD, mypipe->fd2, &event);
    if (ret) {
        goto done;
    }

    int nfds;
    while(1) {
        nfds = epoll_wait(epfd, events, 2, mypipe->timeout);
        if (nfds < 1) {
            // timeout or error
            goto done;
        }
        for (int i = 0; i < nfds; ++i) {
            int rfd = events[i].data.fd;
            int wfd = (events[i].data.fd == mypipe->fd1) ? mypipe->fd2 : mypipe->fd1;
            int bytes;
            ioctl(rfd, FIONREAD, &bytes);
            if (bytes == 0) {
                goto done;
            }
            for (int i = bytes; i > 0; i-=PIPE_SIZE) {
                ssize_t res;
                if (i <= PIPE_SIZE) {
                    res = splice(rfd, NULL, mypipe->pipefd[1], NULL, i, SPLICE_F_MOVE);
                } else {
                    res = splice(rfd, NULL, mypipe->pipefd[1], NULL, PIPE_SIZE, SPLICE_F_MOVE);
                }
                if (res == -1) {
                    goto done;
                }
                if (i <= PIPE_SIZE) {
                    res = splice(mypipe->pipefd[0], NULL, wfd, NULL, res, SPLICE_F_MOVE);
                } else {
                    // tell splice there's more data coming
                    res = splice(mypipe->pipefd[0], NULL, wfd, NULL, res, SPLICE_F_MOVE | SPLICE_F_MORE);
                }
                if (res == -1) {
                    goto done;
                }
            }
        }
    }
done:
    // clean up all our descriptors
    if (epfd > 0) {
        close(epfd);
    }
    close(mypipe->pipefd[0]);
    close(mypipe->pipefd[1]);
    // signal the caller we're done
    enif_send(NULL, &(mypipe->dst), mypipe->env, mypipe->ref);
    enif_thread_exit(NULL);
    return NULL;
}

static ERL_NIF_TERM
splice2(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int fd1, fd2, timeout;
    char atm_infinity[10];
    if (!enif_get_int(env, argv[0], &fd1) || !enif_get_int(env, argv[1], &fd2)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &timeout)) {
        if (enif_get_atom(env, argv[2], atm_infinity, 10, ERL_NIF_LATIN1) && strncmp("infinity", atm_infinity, 9) == 0) {
            timeout = -1;
        } else {
            return enif_make_badarg(env);
        }
    }

    // allocate a resource to hold our pipe FDs
    struct splicer_pipe* mypipe = enif_alloc_resource(SPLICER_RESOURCE, sizeof(struct splicer_pipe));
    int res = pipe(mypipe->pipefd);
    if (res != 0) {
        enif_release_resource(mypipe);
        return ATOM_ERROR;
    }
    mypipe->opts = enif_thread_opts_create("splicer");
    mypipe->env = enif_alloc_env();
    mypipe->fd1 = fd1;
    mypipe->fd2 = fd2;
    mypipe->timeout = timeout;
    enif_self(env, &mypipe->dst);
    ERL_NIF_TERM term = enif_make_resource(env, mypipe);
    mypipe->ref = enif_make_copy(mypipe->env, term);
    int status = enif_thread_create("splicer", &(mypipe->tid), splicer_run, mypipe, mypipe->opts);
    if(status != 0) {
        enif_release_resource(mypipe);
        return enif_make_badarg(env);
    }
    // return {ok, Ref} to the caller
    // The calling process will get sent the Ref when the splice breaks
    // to tell it that it is finished
    return enif_make_tuple2(env, ATOM_OK, term);
}


static ErlNifFunc nif_funcs[] =
    {{"splice_int", 3, splice2, 0}};

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
