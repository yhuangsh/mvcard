-module(qb_html).
-compile(export_all).

-include("../../deps/yaws/include/yaws_api.hrl").

is_ajax_req(A) ->
    H = A#arg.headers,
    OH = H#headers.other,
    lists:any(fun({http_header, _, "X-Requested-With", _, "XMLHttpRequest"}) -> true;
		 (_) -> false
	      end, OH).

li_divider(Text) -> {li, [{'data-role', "list_divider"}], Text}.
a_home() -> {a, [{href, qb_url:home()}, {'data-role', "button"}, {'data-icon', "home"}, {'data-iconpos', "notext"}], "Home"}.
a_home(City) -> {a, [{href, qb_url:home(City)}, {'data-role', "button"}, {'data-icon', "home"}, {'data-iconpos', "notext"}], "Home"}.
    
css(CssPath) -> {link, [{rel, "stylesheet"}, {href, CssPath}]}.
js(JsPath) -> {script, [{src, JsPath}, {type, "text/javascript"}, {charset, "utf-8"}]}.

