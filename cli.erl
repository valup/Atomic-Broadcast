-module(cli).

start(Host, Puerto) ->
    case gen_tcp:connect(Host, Puerto + uniform(3) -1,[binary, {packet, 0}]) of
        {ok , Socket} ->
            io:format("Conectado~n"),
            register(cerrar, spawn(?MODULE, wait_fin, [Socket])),
            register(sec, self()),
            secuencia(0);
        {error, Reason} -> io:format("ERROR: ~p, no se pudo conectar~n", [Reason])
    end.

secuencia(C, Socket) ->
    receive
        fin -> ok;
        {prox, Pid} ->
            Pid ! {prox, C, Socket},
            secuencia(C+1)
    end

wait_fin(Socket) ->
    receive
        fin -> gen_tcp:close(Socket), ok
    end.

fin() ->
    sec ! fin, ok.

get() ->
    sec ! {prox, self()},
    receive
        {prox, C, Socket} ->
            gen_tcp:send(Socket, {C, get}),
            case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
                {ok, {c, getRes, V}} -> io:format("~p~n", [V]);
                {error, _} ->
                    io:format("Servidor caido~n"),
                    sec ! fin,
                    ok
            end
    end

append(R) ->
    sec ! {prox, self()},
    receive
        {prox, C, Socket} ->
            gen_tcp:send(Socket, {C, append, R}),
            case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
                {ok, {c, appendRes, Res}} -> io:format("~p~n", [Res]);
                {error, _} ->
                    io:format("Servidor caido~n"),
                    sec ! fin,
                    ok
            end
    end