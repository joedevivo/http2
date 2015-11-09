-module(http2_settings).

-include("http2.hrl").
-type name() :: binary().
-type property() :: {name(), any()}.
-type properties() :: [property()].
-type proplist() :: {settings, properties()}.
-export_type([proplist/0, name/0, property/0, properties/0]).

-export([diff/2]).

-spec diff(http2_frame_settings:payload(), http2_frame_settings:payload()) -> properties().
diff(OldSettings, NewSettings) ->
    OldPl = to_proplist(OldSettings),
    NewPl = to_proplist(NewSettings),
    diff_(OldPl, NewPl, []).

diff_([],[],Acc) ->
    lists:reverse(Acc);
diff_([OldH|OldT],[OldH|NewT],Acc) ->
    diff_(OldT, NewT, Acc);
diff_([_OldH|OldT],[NewH|NewT],Acc) ->
    diff_(OldT, NewT, [NewH|Acc]).


-spec to_proplist(http2_frame_settings:payload()) -> properties().
to_proplist(Settings) ->
    [
     {?SETTINGS_HEADER_TABLE_SIZE,      Settings#settings.header_table_size     },
     {?SETTINGS_ENABLE_PUSH,            Settings#settings.enable_push           },
     {?SETTINGS_MAX_CONCURRENT_STREAMS, Settings#settings.max_concurrent_streams},
     {?SETTINGS_INITIAL_WINDOW_SIZE,    Settings#settings.initial_window_size   },
     {?SETTINGS_MAX_FRAME_SIZE,         Settings#settings.max_frame_size        },
     {?SETTINGS_MAX_HEADER_LIST_SIZE,   Settings#settings.max_header_list_size  }
    ].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

diff_test() ->
    Old = #settings{},
    New = #settings{
             max_frame_size=2048
            },
    Diff = diff(Old, New),
    ?assertEqual([{?SETTINGS_MAX_FRAME_SIZE, 2048}], Diff),
    ok.

diff_order_test() ->
    Old = #settings{},
    New = #settings{
             max_frame_size = 2048,
             max_concurrent_streams = 2
            },
    Diff = diff(Old, New),
    ?assertEqual(
       [{?SETTINGS_MAX_CONCURRENT_STREAMS, 2},
        {?SETTINGS_MAX_FRAME_SIZE, 2048}],
       Diff
      ),
    ?assertNotEqual(
       [
        {?SETTINGS_MAX_FRAME_SIZE, 2048},
        {?SETTINGS_MAX_CONCURRENT_STREAMS, 2}
       ],
       Diff
      ),

    ok.

-endif.
