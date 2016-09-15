-module(rudy).
% -export([init/1, handler/1, request/1, reply/1]).
-export([start/1, start/2, stop/0]).

start(Port) ->
    start(Port, 1).

start(Port, N) ->
    register(rudy, spawn(fun() -> init(Port, N) end)).

stop() ->
    exit(whereis(rudy), "Forcing rudy web server to exit").

% Initializes server:
%       - Takes port number
%       - Opens listening socket and passes it to handler/1
%       - Closes socket when done
init(Port, N) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handlers(Listen, N),
            wait();
        {error, Error} ->
            io:format("rudy: initialization failed: ~w~n", [Error]),
            error
    end.

wait() ->
    receive
        stop ->
            ok
    end.

handlers(_, 0) ->
    ok;
handlers(Listen, N) ->
    spawn(fun() -> handler(Listen, N) end),
    handlers(Listen, N-1).

% Listens on socket for incoming connections
%       - When client has connected, passes connection to request/1
%       - When request is handled, close connection
handler(Listen, I) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            % io:format("[~p] {ok Client}\n", [I]),
            request(Client),
            % spawn(fun() -> handler(Listen, I) end);
            handler(Listen, I);
        {error, Error} ->
            io:format("rudy: error ~w~n", [Error]),
            error
    end.

% Reads request from client connection and parses it
%       - Parses incoming HTTP request and passes it to reply/1
%       - Reply is sent back to client
request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            % io:format("request {ok Str}"),
            % io:format("\n\nRequest: ~s\n\n", [Str]),
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: errr ~w~n", [Error])
    end,
    gen_tcp:close(Client).

% Decides what to reply and how to format into well-formed HTTP reply
reply({{get, URI, _}, _, _}) ->
    % timer:sleep(40),
    http:ok("<html><head><title>Rudy</title></head><body>Accessing Resource:<br/>" ++ URI ++ "</body></html>").
