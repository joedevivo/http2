-module(http2_frame_settings).

-include("http2.hrl").
-type payload() :: #settings{}.
-export_type([payload/0]).

-define(SETTING_NAMES, [?SETTINGS_HEADER_TABLE_SIZE,
                        ?SETTINGS_ENABLE_PUSH,
                        ?SETTINGS_MAX_CONCURRENT_STREAMS,
                        ?SETTINGS_INITIAL_WINDOW_SIZE,
                        ?SETTINGS_MAX_FRAME_SIZE,
                        ?SETTINGS_MAX_HEADER_LIST_SIZE]).

-behaviour(http2_frame).
-export([
         format/1,
         read/2,
         validate/1,
         send/3,
         ack/1,
         to_binary/1,
         overlay/2
        ]).

-spec format(binary()
            |payload()
            |http2_settings:properties()) -> iodata().
format(<<>>) -> "Ack!";
format(#settings{
        header_table_size        = HTS,
        enable_push              = EP,
        max_concurrent_streams   = MCS,
        initial_window_size      = IWS,
        max_frame_size           = MFS,
        max_header_list_size     = MHLS
    }) ->
    lists:flatten(
        io_lib:format("[Settings: "
        " header_table_size        = ~p,"
        " enable_push              = ~p,"
        " max_concurrent_streams   = ~p,"
        " initial_window_size      = ~p,"
        " max_frame_size           = ~p,"
        " max_header_list_size     = ~p~n]", [HTS,EP,MCS,IWS,MFS,MHLS]));
format({settings, PList}) ->
    L = lists:map(fun({?SETTINGS_HEADER_TABLE_SIZE,V}) ->
                      {header_table_size,V};
                 ({?SETTINGS_ENABLE_PUSH,V}) ->
                      {enable_push,V};
                 ({?SETTINGS_MAX_CONCURRENT_STREAMS,V}) ->
                      {max_concurrent_streams,V};
                 ({?SETTINGS_INITIAL_WINDOW_SIZE,V}) ->
                      {initial_window_size, V};
                 ({?SETTINGS_MAX_FRAME_SIZE,V}) ->
                      {max_frame_size,V};
                 ({?SETTINGS_MAX_HEADER_LIST_SIZE,V}) ->
                      {max_header_list_size,V}
              end,
              PList),
    io_lib:format("~p", [L]).

-spec validate(http2_frame:header()) -> ok.
validate(_) ->
    ok.

-spec read(http2_frame:header(), binary()) ->
    {ok, payload()} |
    {error, term()}.
read(_Header = #frame_header{length=0}, <<>>) ->
    {ok, {settings, []}};
read(_Header = #frame_header{}, Bin) ->
    Settings = parse_settings(Bin),
    {ok, {settings, Settings}}.

-spec parse_settings(binary()) -> [proplists:property()].
parse_settings(Bin) ->
    lists:reverse(parse_settings(Bin, [])).

-spec parse_settings(binary(), [proplists:property()]) ->  [proplists:property()].
parse_settings(<<0,1,Val:4/binary,T/binary>>, S) ->
    parse_settings(T, [{?SETTINGS_HEADER_TABLE_SIZE, binary:decode_unsigned(Val)}|S]);
parse_settings(<<0,2,Val:4/binary,T/binary>>, S) ->
    parse_settings(T, [{?SETTINGS_ENABLE_PUSH, binary:decode_unsigned(Val)}|S]);
parse_settings(<<0,3,Val:4/binary,T/binary>>, S) ->
    parse_settings(T, [{?SETTINGS_MAX_CONCURRENT_STREAMS, binary:decode_unsigned(Val)}|S]);
parse_settings(<<0,4,Val:4/binary,T/binary>>, S) ->
    parse_settings(T, [{?SETTINGS_INITIAL_WINDOW_SIZE, binary:decode_unsigned(Val)}|S]);
parse_settings(<<0,5,Val:4/binary,T/binary>>, S) ->
    parse_settings(T, [{?SETTINGS_MAX_FRAME_SIZE, binary:decode_unsigned(Val)}|S]);
parse_settings(<<0,6,Val:4/binary,T/binary>>, S)->
    parse_settings(T, [{?SETTINGS_MAX_HEADER_LIST_SIZE, binary:decode_unsigned(Val)}|S]);
parse_settings(<<>>, Settings) ->
    Settings.

-spec overlay(#settings{}, http2_settings:proplist()) -> #settings{}.
overlay(S, {settings, [{?SETTINGS_HEADER_TABLE_SIZE, Val}|PList]}) ->
    overlay(S#settings{header_table_size=Val}, {settings, PList});
overlay(S, {settings, [{?SETTINGS_ENABLE_PUSH, Val}|PList]}) ->
    overlay(S#settings{enable_push=Val}, {settings, PList});
overlay(S, {settings, [{?SETTINGS_MAX_CONCURRENT_STREAMS, Val}|PList]}) ->
    overlay(S#settings{max_concurrent_streams=Val}, {settings, PList});
overlay(S, {settings, [{?SETTINGS_INITIAL_WINDOW_SIZE, Val}|PList]}) ->
    overlay(S#settings{initial_window_size=Val}, {settings, PList});
overlay(S, {settings, [{?SETTINGS_MAX_FRAME_SIZE, Val}|PList]}) ->
    overlay(S#settings{max_frame_size=Val}, {settings, PList});
overlay(S, {settings, [{?SETTINGS_MAX_HEADER_LIST_SIZE, Val}|PList]}) ->
    overlay(S#settings{max_header_list_size=Val}, {settings, PList});
overlay(S, {settings, []}) ->
    S.

-spec send(http2:socket(), payload(), payload()) -> ok | {error, term()}.
send({Transport, Socket}, PrevSettings, NewSettings) ->
    Diff = http2_settings:diff(PrevSettings, NewSettings),
    Payload = make_payload(Diff),
    L = size(Payload),
    Header = <<L:24,?SETTINGS:8,16#0:8,0:1,0:31>>,

    Frame = [Header, Payload],
    lager:debug("sending settings ~p", [Frame]),
    Transport:send(Socket, Frame).

-spec make_payload(http2_settings:properties()) -> binary().
make_payload(Diff) ->
    make_payload_(lists:reverse(Diff), <<>>).

make_payload_([], BinAcc) ->
    BinAcc;
make_payload_([{<<Setting>>, Value}|Tail], BinAcc) ->
    make_payload_(Tail, <<Setting:16,Value:32,BinAcc/binary>>).

-spec ack(http2:socket()) -> ok | {error, term()}.
ack({Transport,Socket}) ->
    Transport:send(Socket, <<0:24,4:8,1:8,0:1,0:31>>).

-spec to_binary(payload()) -> iodata().
to_binary(#settings{}=Settings) ->
    [to_binary(S, Settings) || S <- ?SETTING_NAMES].

-spec to_binary(binary(), payload()) -> binary().
to_binary(?SETTINGS_HEADER_TABLE_SIZE, #settings{header_table_size=undefined}) ->
    <<>>;
to_binary(?SETTINGS_HEADER_TABLE_SIZE, #settings{header_table_size=HTS}) ->
    <<16#1:16,HTS:32>>;
to_binary(?SETTINGS_ENABLE_PUSH, #settings{enable_push=undefined}) ->
    <<>>;
to_binary(?SETTINGS_ENABLE_PUSH, #settings{enable_push=EP}) ->
    <<16#2:16,EP:32>>;
to_binary(?SETTINGS_MAX_CONCURRENT_STREAMS, #settings{max_concurrent_streams=undefined}) ->
    <<>>;
to_binary(?SETTINGS_MAX_CONCURRENT_STREAMS, #settings{max_concurrent_streams=MCS}) ->
    <<16#3:16,MCS:32>>;
to_binary(?SETTINGS_INITIAL_WINDOW_SIZE, #settings{initial_window_size=undefined}) ->
    <<>>;
to_binary(?SETTINGS_INITIAL_WINDOW_SIZE, #settings{initial_window_size=IWS}) ->
    <<16#4:16,IWS:32>>;
to_binary(?SETTINGS_MAX_FRAME_SIZE, #settings{max_frame_size=undefined}) ->
    <<>>;
to_binary(?SETTINGS_MAX_FRAME_SIZE, #settings{max_frame_size=MFS}) ->
    <<16#5:16,MFS:32>>;
to_binary(?SETTINGS_MAX_HEADER_LIST_SIZE, #settings{max_header_list_size=undefined}) ->
    <<>>;
to_binary(?SETTINGS_MAX_HEADER_LIST_SIZE, #settings{max_header_list_size=MHLS}) ->
    <<16#6:16,MHLS:32>>.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_payload_test() ->
    Diff = [
            {?SETTINGS_MAX_CONCURRENT_STREAMS, 2},
            {?SETTINGS_MAX_FRAME_SIZE, 2048}
           ],
    Bin = make_payload(Diff),
    <<MCSIndex>> = ?SETTINGS_MAX_CONCURRENT_STREAMS,
    <<MFSIndex>> = ?SETTINGS_MAX_FRAME_SIZE,
    ?assertEqual(<<MCSIndex:16,2:32,MFSIndex:16,2048:32>>, Bin),
    ok.

-endif.
