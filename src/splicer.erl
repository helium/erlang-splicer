-module(splicer).
-on_load(init/0).
-define(APPNAME, splicer).
-define(LIBNAME, splicer).

%% API exports
-export([splice/2, splice/3]).

%%====================================================================
%% API functions
%%====================================================================

-spec splice(non_neg_integer(), non_neg_integer()) -> ok | {error, any()}.
splice(FD1, FD2) ->
    splice(FD1, FD2, infinity).


-spec splice(non_neg_integer(), non_neg_integer(), pos_integer() | infinity) -> ok | {error, any()}.
splice(FD1, FD2, Timeout) ->
    inert:fdset(FD1, read),
    inert:fdset(FD2, read),
    case splice_int(FD1, FD2, Timeout) of
        ok ->
            splice2(FD1, FD2, Timeout);
        {ok, Ref} ->
            inert:fdclr(FD1),
            inert:fdclr(FD2),
            %% Linux NIF version, returns a Ref
            %% block forever and ever
            receive
                Ref -> ok
            end;
        Other ->
            Other
    end.

splice2(FD1, FD2, Timeout) ->
    case splice_int(FD1, FD2, Timeout) of
        ok ->
            splice2(FD1, FD2, Timeout);
        Other ->
            Other
    end.

%% this function *may* be replaced by a NIF
splice_int(FD1, FD2, Timeout) ->
    %% wait for one of the FDs to wake us up
    receive
        {inert_read, _, FD} ->
            [OtherFD] = [FD1, FD2] -- [FD],
            BitSize = erlang:system_info(wordsize)*8,
            {ok, Code, []} = procket:alloc([<<0:BitSize/integer>>]),
            case procket:ioctl(FD, fionread(), Code) of
                {ok, <<BytesToRead:BitSize/integer-unsigned-native>>} when BytesToRead == 0 ->
                    inert:fdclr(OtherFD),
                    done;
                {ok, <<BytesToRead:BitSize/integer-unsigned-native>>} ->
                    case procket:read(FD, BytesToRead) of
                        {ok, Buf} ->
                            case write_exact(OtherFD, Buf) of
                                ok ->
                                    inert:fdset(FD, read),
                                    ok;
                                Other ->
                                    inert:fdclr(OtherFD),
                                    Other
                            end;
                        Other ->
                            inert:fdclr(OtherFD),
                            Other
                    end;
                Other ->
                    inert:fdclr(OtherFD),
                    Other
            end
    after Timeout ->
              {error, timeout}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

fionread() ->
    case os:type() of
        {unix, linux} ->
            16#541B;
        {unix, darwin} ->
            16#4004667f;
        _ ->
            erlang:error({unsupported_os, "add a definition for fionread's value for this operating system"})
    end.

write_exact(FD, Buf) ->
    case procket:write(FD, Buf) of
        ok ->
            ok;
        {ok, N} ->
            <<_:N/bytes, Rest/binary>> = Buf,
            write_exact(FD, Rest);
        Error ->
            Error
    end.

init() ->
    case os:type() of
        {unix, linux} ->
            SoName = case code:priv_dir(?APPNAME) of
                         {error, bad_name} ->
                             case filelib:is_dir(filename:join(["..", priv])) of
                                 true ->
                                     filename:join(["..", priv, ?LIBNAME]);
                                 _ ->
                                     filename:join([priv, ?LIBNAME])
                             end;
                         Dir ->
                             filename:join(Dir, ?LIBNAME)
                     end,
            erlang:load_nif(SoName, 0);
        _ -> ok
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
splice_test() ->
    inert:start(),
    {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(ListenSock),
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
                  {ok, SA} = gen_tcp:accept(ListenSock),
                  {ok, SB} = gen_tcp:accept(ListenSock),
                  gen_tcp:controlling_process(SA, Parent),
                  gen_tcp:controlling_process(SB, Parent),
                  Parent ! {Ref, SA, SB}
          end),
    {ok, SocketC} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    {ok, SocketD} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    receive
        {Ref, SocketA, SocketB} -> ok
    end,

    Pid = spawn(fun() ->
                        receive
                            {A, B} ->
                                splice(element(2, inet:getfd(A)), element(2, inet:getfd(B)))
                        end
                end),

    gen_tcp:controlling_process(SocketA, Pid),
    gen_tcp:controlling_process(SocketB, Pid),

    ok = inet:setopts(SocketD, [{active, true}]),
    ok = inet:setopts(SocketC, [{active, true}]),
    Pid ! {SocketA, SocketB},


    ok = gen_tcp:send(SocketC, <<"Hello">>),
    Result = receive
                 {tcp, SocketD, <<"Hello">>} ->
                     true
             after 1000 ->
                       false
             end,
    ?assert(Result),

    ok = gen_tcp:send(SocketD, <<"Goodbye">>),
    Result2 = receive
                 {tcp, SocketC, <<"Goodbye">>} ->
                     true
             after 1000 ->
                       false
             end,
    ?assert(Result2),

    %% check infinity timeout works
    %timer:sleep(1000),
    %?assert(is_process_alive(Pid)),

    ok.

timeout_test() ->
    inert:start(),
    {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(ListenSock),
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
                  {ok, SA} = gen_tcp:accept(ListenSock),
                  {ok, SB} = gen_tcp:accept(ListenSock),
                  gen_tcp:controlling_process(SA, Parent),
                  gen_tcp:controlling_process(SB, Parent),
                  Parent ! {Ref, SA, SB}
          end),
    {ok, SocketC} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    {ok, SocketD} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    receive
        {Ref, SocketA, SocketB} -> ok
    end,

    Pid = spawn(fun() ->
                        receive
                            {A, B} ->
                                splice(element(2, inet:getfd(A)), element(2, inet:getfd(B)), 500)
                        end
                end),

    gen_tcp:controlling_process(SocketA, Pid),
    gen_tcp:controlling_process(SocketB, Pid),

    ok = inet:setopts(SocketD, [{active, true}]),
    ok = inet:setopts(SocketC, [{active, true}]),
    Pid ! {SocketA, SocketB},


    ok = gen_tcp:send(SocketC, <<"Hello">>),
    Result = receive
                 {tcp, SocketD, <<"Hello">>} ->
                     true
             after 1000 ->
                       false
             end,
    ?assert(Result),

    ok = gen_tcp:send(SocketD, <<"Goodbye">>),
    Result2 = receive
                 {tcp, SocketC, <<"Goodbye">>} ->
                     true
             after 1000 ->
                       false
             end,
    ?assert(Result2),

    %% check timeout
    timer:sleep(500),
    ?assertEqual(false, is_process_alive(Pid)),

    ok.


large_splice_test() ->
    inert:start(),
    {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(ListenSock),
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
                  {ok, SA} = gen_tcp:accept(ListenSock),
                  {ok, SB} = gen_tcp:accept(ListenSock),
                  gen_tcp:controlling_process(SA, Parent),
                  gen_tcp:controlling_process(SB, Parent),
                  Parent ! {Ref, SA, SB}
          end),
    {ok, SocketC} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    {ok, SocketD} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    receive
        {Ref, SocketA, SocketB} -> ok
    end,

    Pid = spawn(fun() ->
                        receive
                            {A, B} ->
                                splice(element(2, inet:getfd(A)), element(2, inet:getfd(B)))
                        end
                end),

    gen_tcp:controlling_process(SocketA, Pid),
    gen_tcp:controlling_process(SocketB, Pid),

    Pid ! {SocketA, SocketB},

    Pkt = crypto:strong_rand_bytes(16384),
    {Time, {ok, Pkt}} = timer:tc(fun() ->
                                         gen_tcp:send(SocketC, Pkt),
                                         gen_tcp:recv(SocketD, 16384)
                                 end),

    io:format(user, "Time ~f~n", [Time/1000000]),
    Pkt2 = crypto:strong_rand_bytes(4123),
    {Time2, {ok, Pkt2}} = timer:tc(fun() ->
                                         gen_tcp:send(SocketC, Pkt2),
                                         gen_tcp:recv(SocketD, 4123)
                                 end),
    io:format(user, "Time ~f~n", [Time2/1000000]),
    gen_tcp:close(SocketC),
    timer:sleep(2000),
    false = is_process_alive(Pid),
    ok.

-endif.
