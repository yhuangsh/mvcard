-module(qb_tab).
-compile(export_all).

init() ->
    qb_tab_city:init(),
    qb_tab_merchant:init(),
    qb_tab_user:init(),
    qb_tab_session:init(),
    qb_tab_trans:init(),
    qb_tab_promo:init().
