-module(benchmark).
-export([parse/0, bench/2, bench/4, run/0]).

parse() ->
    http:parse_request("GET /foo HTTP/1.1\r\nUser-Agent: Test\r\nAccept: anything\r\n\r\nThis is the body").

run() ->
    lists:foreach(fun(N) ->
        bench(localhost, 8080, 4, N)
        end,
        [ 1, 2, 5, 10] %[ 50, 100, 150]  % 200, 250, 300, 350, 400, 450, 500, 750, 1000, 1250, 1500, 1750, 2000, 2500, 3000, 3500, 4000 ]
    ).

bench(Host, Port) ->
    bench(Host, Port, 4, 5000).

bench(Host, Port, C, N) ->
    Start = erlang:system_time(micro_seconds),
    parallel(C, Host, Port, N, self()),
    collect(C),
    Finish = erlang:system_time(micro_seconds),
    T = Finish - Start,
    io:format("~w\t~w~n", [N, (T div 1000)]).

parallel(0, _, _, _, _) ->
    ok;
parallel(C, Host, Port, N, Ctrl) ->
    spawn(fun() -> report(N, Host, Port, Ctrl) end),
    parallel(C-1, Host, Port, N, Ctrl).

report(N, Host, Port, Ctrl) ->
    run(N, Host, Port),
    Ctrl ! ok.

collect(0) ->
    ok;
collect(N) ->
    receive
        ok ->
            collect(N-1)
    end.

run(0, _, _) ->
    ok;
run(N, Host, Port) ->
    request(Host, Port),
    % dummy(Host, Port),
    run(N-1, Host, Port).

dummy(_, _) ->
    ok.

request(Host, Port) ->
    Resp = gen_tcp:connect(Host, Port, [list, {active, false}, {reuseaddr, true}]),
    case Resp of
        {ok, Server} ->
            gen_tcp:send(Server, http:get("foo")),
            Recv = gen_tcp:recv(Server, 0),
            case Recv of
                {ok, _} ->
                    ok;
                {error, Error} ->
                    io:format("test: error: ~w~n", [Error])
            end,
            gen_tcp:close(Server);
        {error, Reason} ->
            io:format("test: error: ~w~n", [Reason])
    end,
    ok.






