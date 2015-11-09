
%% TODO: There's too much going on during this refactor to split this
%% file up, but I'd really like to allow an
%% -include_lib("http2/src/http2.hrl"). to include several files. I'll
%% try and put separators in this file where I think we could split

%% -include_lib("http2/src/http2_flags.hrl").
-define(FLAG_ACK,         16#1 ).
-define(FLAG_END_STREAM,  16#1 ).
-define(FLAG_END_HEADERS, 16#4 ).
-define(FLAG_PADDED,      16#8 ).
-define(FLAG_PRIORITY,    16#20).

-define(IS_FLAG(Flags, Flag), Flags band Flag =:= Flag).
-define(NOT_FLAG(Flags, Flag), Flags band Flag =/= Flag).

%% -include_lib("http2/src/http2_error.hrl").
-define(NO_ERROR,           16#0).
-define(PROTOCOL_ERROR,     16#1).
-define(INTERNAL_ERROR,     16#2).
-define(FLOW_CONTROL_ERROR, 16#3).
-define(SETTINGS_TIMEOUT,   16#4).
-define(STREAM_CLOSED,      16#5).
-define(FRAME_SIZE_ERROR,   16#6).
-define(REFUSED_STREAM,     16#7).
-define(CANCEL,             16#8).
-define(COMPRESSION_ERROR,  16#9).
-define(CONNECT_ERROR,      16#a).
-define(ENHANCE_YOUR_CALM,  16#b).
-define(INADEQUATE_SECURITY,16#c).
-define(HTTP_1_1_REQUIRED,  16#d).

%% -include_lib("http2/src/http2_settings.hrl").
-define(SETTINGS_HEADER_TABLE_SIZE,         <<16#1>>).
-define(SETTINGS_ENABLE_PUSH,               <<16#2>>).
-define(SETTINGS_MAX_CONCURRENT_STREAMS,    <<16#3>>).
-define(SETTINGS_INITIAL_WINDOW_SIZE,       <<16#4>>).
-define(SETTINGS_MAX_FRAME_SIZE,            <<16#5>>).
-define(SETTINGS_MAX_HEADER_LIST_SIZE,      <<16#6>>).

%% -include_lib("http2/src/http2_frame.hrl").
%% FRAME TYPES
-define(DATA            , 16#0).
-define(HEADERS         , 16#1).
-define(PRIORITY        , 16#2).
-define(RST_STREAM      , 16#3).
-define(SETTINGS        , 16#4).
-define(PUSH_PROMISE    , 16#5).
-define(PING            , 16#6).
-define(GOAWAY          , 16#7).
-define(WINDOW_UPDATE   , 16#8).
-define(CONTINUATION    , 16#9).

%% frame_header record for the first 9 bytes in any http/2 frame.
-record(frame_header, {
    length      :: non_neg_integer(),
    type        :: http2_frame:type(),
    flags = 0   :: non_neg_integer(),
    stream_id   :: http_stream:id()
    }).

%% There are ten types of frame, and each has their own payload type:

%% DATA Frame
-record(data, {
          data :: binary()
         }).

%% HEADERS Frame
-record(headers, {
          priority = undefined :: http2_frame_priority:payload() | undefined,
          block_fragment :: binary()
         }).

%% PRIORITY Frame
-record(priority, {
          exclusive :: 0 | 1,
          stream_id :: http2_stream:id(),
          weight :: pos_integer()
         }).

%% RST_STREAM Frame
-record(rst_stream, {
          error_code :: http2:error_code()
         }).

%% SETTINGS Frame, these default values come straight from the HTTP/2 spec
-record(settings, {
          header_table_size        = 4096,
          enable_push              = 1,
          max_concurrent_streams   = unlimited,
          initial_window_size      = 65535,
          max_frame_size           = 16384,
          max_header_list_size     = unlimited
         }
       ).

%% PUSH_PROMISE
-record(push_promise, {
          promised_stream_id :: http2_stream:id(),
          block_fragment :: binary()
         }).

%% PING
-record(ping, {
          opaque_data :: binary()
         }).

%% GOAWAY
-record(goaway, {
          last_stream_id :: http2_stream:id(),
          error_code :: http2:error_code(),
          additional_debug_data = <<>> :: binary()
         }).

%% WINDOW_UPDATE
-record(window_update, {
          window_size_increment :: non_neg_integer()
         }).

%% CONTINUATION
-record(continuation, {
          block_fragment :: binary()
         }).
