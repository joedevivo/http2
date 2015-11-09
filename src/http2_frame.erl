-module(http2_frame).

-include("http2.hrl").

-type type() :: ?DATA
              | ?HEADERS
              | ?PRIORITY
              | ?RST_STREAM
              | ?SETTINGS
              | ?PUSH_PROMISE
              | ?PING
              | ?GOAWAY
              | ?WINDOW_UPDATE
              | ?CONTINUATION.
-type header() :: #frame_header{}.

-type payload() :: http2_frame_data:payload()
                 | http2_frame_headers:payload().

-export_type([type/0, header/0, payload/0]).

%% Callbacks for each type of frame.

%% Turn the payload into something we can display
-callback format(payload()) -> iodata().

%% Each module will define how to read the frame payload from this
%% header and the binary that follows.
-callback read(Header::header(),
               Payload::binary()) ->
    {ok, payload()} | {error, term()}.

%% Sometimes a frame header is invalid, and there are specific rules
%% per frame type. Rather than bog down a http2_frame:validate/1
%% function, we'll store the logic for each frame in that frame's
%% module.
-callback validate(FrameHeader::header()) ->
    ok | {error, term()}.

-callback to_binary(Payload::payload()) ->
    iodata().
