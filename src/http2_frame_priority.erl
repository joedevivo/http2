-module(http2_frame_priority).

-include("http2.hrl").
-type payload() :: #priority{}.

-behaviour(http2_frame).
-export([
         format/1,
         read/2,
         to_binary/1,
         validate/1
        ]).

-spec format(payload()) -> iodata().
format(Payload) ->
    io_lib:format("[Priority: ~p]", [Payload]).

-spec read(http2_frame:header(), binary()) ->
    {ok, payload()} | {error, term()}.
read(#frame_header{}, <<Exclusive:1,StreamId:31,Weight:8/bits>>) ->
    {ok, #priority{
            exclusive = Exclusive,
            stream_id = StreamId,
            weight = Weight
           }};
read(_, _) ->
    {error, bad_frame_body}.

-spec to_binary(payload()) -> iodata().
to_binary(#priority{
             exclusive=E,
             stream_id=StreamId,
             weight=W
            }) ->
    <<E:1,StreamId:31,W:8>>.

validate(#frame_header{length=5}) ->
    ok;
validate(_) ->
    {error, ?FRAME_SIZE_ERROR}.
