-module(qb_url).
-compile(export_all).

-include("../../deps/yaws/include/yaws_api.hrl").

this(A) -> lists:flatten(yaws_api:format_url(yaws_api:request_url(A))).
    
index() -> prefix() ++ "index.yaws".
index(Url) -> prefix() ++ "index.yaws?g=" ++ yaws_api:url_encode(Url).
    
home() -> prefix() ++ "home.yaws".
home(City) ->prefix() ++ "home.yaws?city=" ++ yaws_api:url_encode(City).

login() -> login(home()).
login(CallbackUrl) -> prefix() ++ "login.yaws?cb=" ++ yaws_api:url_encode(CallbackUrl).
account(UserId) -> prefix() ++ "account.yaws?uid=" ++ yaws_api:url_encode(UserId).

select_city(City) -> prefix() ++ "selcity.yaws?city=" ++ yaws_api:url_encode(City).
change_city(City) -> prefix() ++ "chgcity.yaws?city=" ++ yaws_api:url_encode(City).

auth(UserName, Password, CallbackUrl) ->
    prefix() ++ 
	"auth.yaws?u=" ++ yaws_api:url_encode(UserName) ++ 
	"&p=" ++ yaws_api:url_encode(Password) ++ 
	"&cb=" ++ yaws_api:url_encode(CallbackUrl).
register(CallbackUrl) -> prefix() ++ "register.yaws?cb=" ++ yaws_api:url_encode(CallbackUrl).
barcode_scan(CallbackUrl) -> prefix_qb() ++ "barcode/scan?cb=" ++ yaws_api:url_encode(CallbackUrl).

error(Msg, Cb) -> error(Msg, "返回", Cb).
error(Msg, BtnTxt, Cb) -> error("出错了", "原因", Msg, BtnTxt, Cb).
error(Title1, Title2, Msg, BtnTxt, Cb) ->
    lists:flatten([prefix(), 
		   "error.yaws?t1=", yaws_api:url_encode(Title1), 
		   "&t2=", yaws_api:url_encode(Title2), 
		   "&m=", yaws_api:url_encode(Msg),
		   "&b=", yaws_api:url_encode(BtnTxt),
		   "&cb=", yaws_api:url_encode(Cb)]).

%prefix() -> "http://admin.weilink.me:8000/qianbao/".
%prefix() -> "http://localhost:8080/".
prefix() -> "/".
prefix_qb() -> "qianbao://".
   
