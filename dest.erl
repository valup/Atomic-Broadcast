-module(dest).
-include("struct.hrl").
-export([start/1,stop/0]).
-export([destLoop/3, senderLoop/1, deliver/0]).
-export([send/1]).

start(Seq) ->
    Nodos = nodos(),
    Otros = delete(node(), Nodos)
    if
        node() == lists:nth(1, Nodos) -> connect(Otros)
    end,
    register(dest, spawn(?MODULE, destLoop,[1, dict:new(), [], 0])),
    register(sender, spawn(?MODULE, senderLoop,[Otros, 1])),
    register(num, spawn(?MODULE, numero,[0, 0])),
    register(deliver, spawn(?MODULE, deliver,[])).

% Crea una lista de los nodos servidores en base a SNames
nodos() ->
  {ok, Hostname} = inet:gethostname(),
  [list_to_atom(Nodo++"@"++Hostname) || Nodo <- ?NODOS].

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
    unregister(deliver),
    unregister(dest).

send(Msg) ->
    sender ! #send{msg = Msg, sender = self()},
    ok.

senderLoop(Otros, N) ->
    receive
        S when is_record(S, send) ->
            Id = atom_to_list(node()) ++ integer_to_list(N),
            M = #msg{msg = S#send.msg, sender = node()},
            lists:foreach(fun (X) -> {dest, X} ! {M, Id} end, Otros),
            A = propuestas(Otros),
            num ! {acordado, A},
            lists:foreach(fun (X) -> {dest, X} ! {acordado, A, Id} end, Otros),
            senderLoop(Otros, N+1);
        _ ->
            io:format("Recv cualca ~n")
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
                    destLoop(dict:append(Id, N, Ids), ubicar(Data#msj{sn = N}, N, Orden), TO)
            end;
        {acordado, A0, Id} ->
            acordado ! num,
            receive
                {acuerdo, A1} ->
                    A = max(A0, A1),
                    num ! {acordado, A},
                    {ok, N} = dict:find(Id, Ids, TO),
                    if
                        A > N -> destLoop(Ids, reubicar(N, A, Orden), TO);
                        true -> destLoop(Ids, Orden, TO)
                    end
            end
    after TO ->
        [Prox | Resto] = lists:nth(1, Orden),
        if
            Prox#msg.estado == aceptado ->
                deliver ! Prox,
                destLoop(Ids, Resto, 0);
            true -> destLoop(Ids, Resto, infinity)
        end
    end.

ubicar(M, []) -> [M];
ubicar(M, Msjs) ->
    [Msj | Msgs] = Msjs,
    if
        M#msg.sn <= Msj#msg.sn -> [M | Msjs];
        true -> [Msj | ubicar (M, Msgs)]
    end.

reubicar(N, A, Msjs) ->
    [Msj | Msgs] = Msjs,
    if
        N == Msj#msg.sn -> ubicar(A, Msj#msg{estado = aceptado}, Msgs);
        true -> [Msj | reubicar (M, Msgs)]
    end.

numero(A, P) ->
    receive
        {acordado, N} -> numero(N, P);
        proponer ->
            N = max(A, P) + 1,
            dest ! {propongo, N},
            numero(A, N);
        acordado ->
            dest ! {acuerdo, A},
            numero(A, N)
    end.
