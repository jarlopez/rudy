-module(rudy).
-export([init/1, handler/1, request/1, reply/1]).

% Initializes server:
%       - Takes port number
%       - Opens listening socket and passes it to handler/1
%       - Closes socket when done
init(Port) ->
   noop.

% Listens on socket for incoming connections
%       - When client has connected, passes connection to request/1
%       - When request is handles, close connection
handler(Listen) ->
    noop.

% Reads request from client connection and parses it
%       - Parses incoming HTTP request and passes it to reply/1
%       - Reply is sent back to client
request(Client) ->
    noop.

% Decides what to reply and how to format into well-formed HTTP reply
reply(Request) ->
    noop.
