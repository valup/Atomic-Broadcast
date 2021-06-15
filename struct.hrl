%% Msg info Msg x Sender x Seq Num
-record(msg, {msg, sender, sn = 0, estado = pendiente}).

% se puede poner cualquier cantidad de nodos con cualquier nombre (sin hostname)
% pero deben estar todos abiertos al correr el codigo
-define(NODOS, ["a","b","c"]).
