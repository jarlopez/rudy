-module(rudy).
-export([init/1, handler/1, request/1, reply/1]).
-compile({no_auto_import,[get/1]}).

% Initializes server:
%       - Takes port number
%       - Opens listening socket and passes it to handler/1
%       - Closes socket when done
init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            io:format("init {ok Listen}"),
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            io:format("init {error Error}"),
            error
    end.

% Listens on socket for incoming connections
%       - When client has connected, passes connection to request/1
%       - When request is handles, close connection
handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            io:format("handler {ok Client}"),
            request(Client);
        {error, Error} ->
            io:format("handler {error Error"),
            error
    end.

% Reads request from client connection and parses it
%       - Parses incoming HTTP request and passes it to reply/1
%       - Reply is sent back to client
request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            io:format("request {ok Str}"),
            io:format("\n\nRequest: ~s\n\n", [Str]),
            Request = http:parse_request(Str),
            Response = reply(Request), %TODO PDF says reply(Request), where Request is unbound %
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: errr ~w~n", [Error])
    end,
    gen_tcp:close(Client).

% Decides what to reply and how to format into well-formed HTTP reply
reply({{get, URI, _}, _, _}) ->
    http:ok("Welcome to the next chapter.").