-module(rudy).
% -export([init/1, handler/1, request/1, reply/1]).
-export([start/1, start/2, stop/0]).

-define(home_page, "index.html").
-define(error_page, "error.html").
-define(not_found_template, "<html><head><title>Not Found</title></head><body>The requested resource cannot be found.</body></html>").

start(Port) ->
    start(Port, 1).

start(Port, N) ->
    register(rudy, spawn(fun() -> init(Port, N) end)).

stop() ->
    whereis(rudy) ! stop.

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
            % io:format("[PID ~p] Accepted Client\n", [self()]),
            % spawn(fun() -> request(Client) end),
            request(Client),
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
            reply(Client, Request);
        {error, Error} ->
            io:format("rudy: errr ~w~n", [Error])
    end.

determine_resource(ParsedUri) ->
    case ParsedUri of
        {ok, {_, _, _, _, [$/], _}} ->
            {resource, ?home_page};
        {ok, {_, _, _, _, [$/ | Resource], Queries}} ->
            % io:format("Resource: ~p\n", [Resource]),
            % io:format("Queries: ~p\n", [Queries]),
            {resource, Resource};
        {error, Reason} ->
            {error, ?error_page}
    end.

get_content_type(FileName) ->
    Ext  = filename:extension(FileName),
    case Ext of
        ".jpg" ->
            "image/jpg";
        ".png" ->
            "image/png";
        ".html" ->
            "text/html";
        ".css" ->
            % io:format("returning css\n"),
            "text/css";
        _ ->
            % No extension, assume plaintext
            % io:format("returning plain\n"),
            "text/plain"
    end.

% Decides what to reply and how to format into well-formed HTTP reply
reply(Client, {{get, URI, _}, _, _}) ->
    % timer:sleep(40),
    % io:format("Slept!\n"),
    FullPath = "http://localhost" ++ URI,
    Uri = http:parse_uri(FullPath),
    ResourceStatus = determine_resource(Uri),
    % case ResourceStatus of
    %     {resource, Resource} ->

    {_, Resource} = ResourceStatus,
    case filelib:file_size(Resource) of
        0 ->
            Response = http:not_found(),
            Data = ?not_found_template,
            ok;
        FileSize ->
            % Resource exists
            % io:format("FileSize; ~p\n", [FileSize]),
            % Response = http:gen_resource_headers(FileSize, "image/png"),
            MimeType = get_content_type(Resource),
            % io:format("mimetype: ~p\n", [MimeType]),
            Response = http:gen_resource_headers(FileSize, MimeType),
            % file:sendfile(Resource, Client)
            {ok, Data} = file:read_file(Resource)
    end,

    % io:format("Response: ~p\nResource: ~p\n", [Response, Resource]),
    gen_tcp:send(Client, Response),
    gen_tcp:send(Client, Data),
    gen_tcp:send(Client, "\r\n"),
    % file:sendfile(Resource,  Client),
    gen_tcp:close(Client).