-module(http2).

-include("http2.hrl").

-type error_code() :: ?NO_ERROR
                    | ?PROTOCOL_ERROR
                    | ?INTERNAL_ERROR
                    | ?FLOW_CONTROL_ERROR
                    | ?SETTINGS_TIMEOUT
                    | ?STREAM_CLOSED
                    | ?FRAME_SIZE_ERROR
                    | ?REFUSED_STREAM
                    | ?CANCEL
                    | ?COMPRESSION_ERROR
                    | ?CONNECT_ERROR
                    | ?ENHANCE_YOUR_CALM
                    | ?INADEQUATE_SECURITY
                    | ?HTTP_1_1_REQUIRED.


-type frame() :: {http2_frame:header(), http2_frame:payload()}.

-export_type([error_code/0, frame/0]).


-type transport() :: gen_tcp | ssl.
-type socket() :: {gen_tcp, gen_tcp:socket()|undefined} | {ssl, ssl:sslsocket()|undefined}.
-export_type([transport/0, socket/0]).
