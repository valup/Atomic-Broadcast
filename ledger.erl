-module{ledger}.

start() ->
    ledger([]).

ledger(S) ->
    receive
        {C, get, Pid} -> Pid ! {C, getRes, reverse(S)};
        {C, append, R, Pid} ->
            if
                lists:member(R, S) -> Pid ! {C, appendRes, exists};
                true ->
                    Pid ! {C, appendRes, ack},
                    ledger([R|S])
            end
    end.