%% Msg info Msg x Sender x Seq Num
-record(msg, {msg, sender, sn = 0, estado = pendiente}).
