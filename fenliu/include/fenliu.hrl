-ifdef(TEST).
fl_call(Server, Request) -> gen_server:call(Server, Request).
fl_cast(Server, Msg) -> gen_server:cast(Server, Msg).
-else.
fl_call(Server, Request) -> (fenliu:resolve_server(Server)):call(Server, Request).
fl_cast(Server, Msg) -> (fenliu:resolve_server(Server)):cast(Server, Msg).
-endif.
