-module(http2_frame_headers).

-include("http2.hrl").

-type payload() :: #headers{}.
-export_type([payload/0]).

-behaviour(http2_frame).
-export([
         format/1,
         read/2,
         validate/1,
         to_binary/1
        ]).

-spec format(payload()) -> iodata().
format(Payload) ->
    io_lib:format("[Headers: ~p]", [Payload]).
