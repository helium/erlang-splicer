-module(splicer).
-on_load(init/0).
-define(APPNAME, splicer).
-define(LIBNAME, splicer).

%% API exports
-export([splice/2]).

%%====================================================================
%% API functions
%%====================================================================

splice(FD1, FD2) ->
    case splice_int(FD1, FD2) of
        ok ->
            splice(FD1, FD2);
        {ok, Ref} ->
            splice(FD1, FD2, Ref);
        Other ->
            Other
    end.

splice(FD1, FD2, Ref) ->
    case splice_int(FD1, FD2, Ref) of
        {ok, Ref} ->
            splice(FD1, FD2, Ref);
        Other ->
            Other
    end.

%% this function is a NIF only
splice_int(_FD1, _FD2, _Ref) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

%% this function *may* be replaced by a NIF
splice_int(FD1, FD2) ->
    inert:fdset(FD1, read),
    inert:fdset(FD2, read),
    %% wait for one of the FDs to wake us up
    receive
        {inert_read, _, FD} ->
            [OtherFD] = [FD1, FD2] -- [FD],
            inert:fdclr(OtherFD),
            BitSize = erlang:system_info(wordsize)*8,
            {ok, Code, []} = procket:alloc([<<0:BitSize/integer>>]),
            case procket:ioctl(FD, fionread(), Code) of
                {ok, Res} ->
                    <<BytesToRead:BitSize/integer-unsigned-native>> = Res,
                    case procket:read(FD, BytesToRead) of
                        {ok, Buf} ->
                            write_exact(OtherFD, Buf);
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end
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
        {unix, linux2} ->
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
    spawn(fun() ->
                  {ok, SA} = gen_tcp:accept(ListenSock),
                  {ok, SB} = gen_tcp:accept(ListenSock),
                  gen_tcp:controlling_process(SA, Parent),
                  gen_tcp:controlling_process(SB, Parent),
                  Parent ! {SA, SB}
          end),
    {ok, SocketC} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    {ok, SocketD} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    receive
        {SocketA, SocketB} -> ok
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

    ok.

-endif.
