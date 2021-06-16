-module(dest).
-include("struct.hrl").
-export([start/1,stop/0]).
-export([destLoop/3, liderSender/1, nodoSender/1, deliver/0, numero/2]).
-export([send/1]).

start(Nodos) ->
    if
        Nodos == [] ->
            io:format("La lista debe tener todos los nodos, incluyendo el actual~n"),
            exit(error);
        true ->
            Lider = lists:nth(1, Nodos),
            if
                node() == Lider ->
                    [_|Otros] = Nodos,
                    connect(Otros),
                    register(sender, spawn(?MODULE, liderSender,[Otros]));
                true ->
                    register(sender, spawn(?MODULE, nodoSender,[lists:delete(node(), Nodos)]))
            end,
            register(dest, spawn(?MODULE, destLoop,[dict:new(), [], 0])),
            register(num, spawn(?MODULE, numero,[0, 0])),
            register(deliver, spawn(?MODULE, deliver,[]))
    end,
    ok.

% Conecta el nodo actual a los de la lista
connect([]) -> ok;
connect([Nodo | Nodos]) ->
  case net_kernel:connect_node(Nodo) of
    true ->
      io:format(user, "Conectado a ~p\n", [Nodo]),
      connect(Nodos);
    false ->
      io:format(user, "Error conectando a ~p\n", [Nodo]),
      exit(error);
    ignored ->
      io:format("Error, el nodo local no está vivo. ~n")
  end.

stop() ->
    dest ! fin,
    deliver ! fin,
    sender ! fin,
    num ! fin,
    unregister(deliver),
    unregister(dest),
    unregister(sender),
    unregister(num).

send(Msg) ->
    sender ! #msg{msg = Msg, sender = self()},
    ok.

liderSender(Nodos) ->
    net_kernel:monitor_nodes(false),
    avisar(Nodos),
    senderLoop(Nodos, length(Nodos), 1).

avisar([]) -> ok;
avisar([Nodo|Nodos]) ->
    {sender, Nodo} ! listo,
    avisar(Nodos).

nodoSender(Nodos) ->
    receive
        listo ->
            net_kernel:monitor_nodes(false),
            senderLoop(Nodos, length(Nodos), 1)
    end.

senderLoop(Nodos, NumNodos, N) ->
    receive
        fin -> ok;
        nodedown ->
            io:format("hey~n"),
            Nodes = nodes(),
            if
                length(Nodes)/NumNodos < 0.25 ->
                    io:format("Demasiados nodos caidos. El servidor se cerrara.~n"),
                    stop();
                true ->
                    senderLoop(Nodes, NumNodos, N)
            end;
        S when is_record(S, msg) ->
            Id = atom_to_list(node()) ++ integer_to_list(N),
            M = #msg{msg = S#msg.msg, sender = node()},
            lists:foreach(fun (X) -> {dest, X} ! {M, Id} end, Nodos),
            A = propuestas(Nodos),
            num ! {acordado, A},
            lists:foreach(fun (X) -> {dest, X} ! {acordado, A, Id} end, Nodos),
            senderLoop(Nodos, NumNodos, N+1)
    end.

propuestas([]) -> 0;
propuestas([_|Nodos]) ->
    receive
        {propuesta, N} -> max(N, propuestas(Nodos))
    end.

deliver() ->
    receive
        fin -> exit(normal);
        M ->
            io:format("Deliver : ~p ~n", [M]),
            deliver()
    end.


destLoop(Ids, Orden, TO) ->
    receive
        {Data, Id} when is_record(Data, msg) ->
            num ! proponer,
            receive
                {propongo, N} ->
                    {sender, Data#msg.sender} ! {propuesta, N},
                    destLoop(dict:append(Id, N, Ids), ubicar(Data#msg{sn = N}, Orden), 0)
            end;
        {acordado, A0, Id} ->
            num ! acordado,
            receive
                {acuerdo, A1} ->
                    A = max(A0, A1),
                    num ! {acordado, A},
                    {ok, [N]} = dict:find(Id, Ids),
                    if
                        A >= N ->
                            destLoop(Ids, reubicar(N, A, Orden), 0);
                        true -> destLoop(Ids, Orden, 0)
                    end
            end
    after TO ->
        case Orden of
            [] -> destLoop(Ids, Orden, infinity);
            _ ->
                [Prox | Resto] = Orden,
                if
                    Prox#msg.estado == aceptado ->
                        deliver ! Prox#msg.msg,
                        destLoop(Ids, Resto, 0);
                    true -> destLoop(Ids, Orden, infinity)
                end
        end
    end.

ubicar(M, []) -> [M];
ubicar(M, Msjs) ->
    [Msj | Msgs] = Msjs,
    if
        M#msg.sn =< Msj#msg.sn -> [M | Msjs];
        true -> [Msj | ubicar (M, Msgs)]
    end.

reubicar(N, A, Msjs) ->
    [Msj | Msgs] = Msjs,
    if
        N == Msj#msg.sn ->
            if
                A > N -> ubicar(Msj#msg{estado = aceptado, sn = A}, Msgs);
                true -> [Msj#msg{estado = aceptado} | Msgs]
            end;
        true -> [Msj | reubicar(N, A, Msgs)]
    end.

numero(A, P) ->
    receive
        fin -> ok;
        {acordado, N} -> numero(N, P);
        proponer ->
            N = max(A, P) + 1,
            dest ! {propongo, N},
            numero(A, N);
        acordado ->
            dest ! {acuerdo, A},
            numero(A, P)
    end.
