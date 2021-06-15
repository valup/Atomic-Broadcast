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
            num ! (acordado, A),
            lists:foreach(fun (X) -> {dest, X} ! {A, Id} end, Otros),
            senderLoop(Seq);
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


destLoop(NDel, Ids, Orden,  TO) ->
    receive
        {Data, Id} when is_record(Data,msg) ->
            num ! proponer,
            receive
                {propongo, N} ->
                    {sender, Data#msg.sender} ! {propuesta, N},
                    Msj = #send{msg = Data#msg.msg, sender = Data#msg.sender, sn = N},
                    destLoop(NDel, dict:append(Id, N, Ids), ubicar(Msj, N, Orden))

    after TO ->
            case dict:find(NDel, Pend) of
                {ok, Msg} ->
                    deliver ! Msg,
                    destLoop(NDel + 1, Pend, 0);
                error ->
                    destLoop(NDel, Pend, infinity)
            end
    end.

ubicar(M, Msjs) ->
    [Msj | Msgs] = Msjs
    if
        M#send.sn <= Msj#send.sn -> [M | Msjs];
        true -> [Msj | ubicar (M, Msgs)]
    end.

numero(A, P) ->
    receive
        {acordado, N} -> numero(N, P);
        proponer ->
            N = max(A, P) + 1,
            dest ! {propongo, N},
            numero(A, N)
    end.
