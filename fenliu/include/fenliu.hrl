-ifdef(TEST).
-ifdef(USE_FENLIU_CALL).
fl_call(Server, Request) -> gen_server:call(Server, Request).
-endif.
-ifdef(USE_FENLIU_CAST).
fl_cast(Server, Msg) -> gen_server:cast(Server, Msg).
-endif.
-else.
-ifdef(USE_FENLIU_CALL).
fl_call(Server, Request) -> (fenliu:resolve_server(Server)):call(Server, Request).
-endif.
-ifdef(USE_FENLIU_CAST).
fl_cast(Server, Msg) -> (fenliu:resolve_server(Server)):cast(Server, Msg).
-endif.
-endif.
