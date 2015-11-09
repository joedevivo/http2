-module(http2_frame_data).

-include("http2.hrl").

-type payload() :: #data{}.
-export_type([payload/0]).

-export([
         to_frames/3,
         send/4
        ]).

-behaviour(http2_frame).
-export([
         format/1,
         read/2,
         validate/1,
         to_binary/1
        ]).

%% format/1 shows the first 32 bytes if the frame is 32 or larger
-spec format(payload()) -> iodata().
format(Payload) ->
    BinToShow = case size(Payload) > 31 of
        false ->
            Payload#data.data;
        true ->
            <<Start:32/binary,_/binary>> = Payload#data.data,
            Start
    end,
    io_lib:format("[Data: {data: ~p ...}]", [BinToShow]).

%% DATA Frames have no rules about what can be in the header
-spec validate(http2_frame:header()) -> ok.
validate(_) -> ok.

-spec read(http2_frame:header(), binary()) ->
    {ok, payload()} | {error, term()}.
read(_H=#frame_header{length=0}, <<>>) ->
    {ok, #data{data= <<>>}};
read(H=#frame_header{length=L}, Bin)
  when L =:= byte_size(Bin) ->
    Data = http2_padding:read_possibly_padded_payload(Bin, H),
    {ok, #data{data=Data}};
read(_, _) ->
    {error, binary_wrong_size}.

-spec to_binary(payload()) -> iodata().
to_binary(#data{data=D}) ->
    D.

-spec to_frames(http2_stream:id(), binary(), http2_frame_settings:payload()) -> [http2:frame()].
to_frames(StreamId, Data, S=#settings{max_frame_size=MFS}) ->
    L = byte_size(Data),
    case L >= MFS of
        false ->
            [{#frame_header{
                 length=L,
                 type=?DATA,
                 flags=?FLAG_END_STREAM,
                 stream_id=StreamId
                }, #data{data=Data}}];
        true ->
            <<ToSend:MFS/binary,Rest/binary>> = Data,
            [{#frame_header{
                 length=MFS,
                 type=?DATA,
                 stream_id=StreamId
                },
              #data{data=ToSend}} | to_frames(StreamId, Rest, S)]
    end.

send({Transport, Socket}, StreamId, Data, Settings) ->
    Frames = to_frames(StreamId, Data, Settings),
    [ Transport:send(Socket, F) || F <- Frames].
