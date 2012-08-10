-module(qb_url).
-compile(export_all).

-include("../../deps/yaws/include/yaws_api.hrl").

e(Url) -> yaws_api:url_encode(Url).
f(Url) -> lists:flatten(Url).

this(A) -> this_1((A#arg.req)#http_request.path).
this_1({abs_path, Path}) -> Path.
	    
index() -> f([prefix(), "index.yaws"]).
index(Url) -> f([prefix(), "index.yaws?g=", e(Url)]).
    
home() -> f([prefix(), "home.yaws"]).
home(City) -> f([prefix(), "home.yaws?city=", e(City)]).

login() -> login(home()).
login(CallbackUrl) -> f([prefix(), "login.yaws?cb=", e(CallbackUrl)]).

account(UserId) -> f([prefix(), "account.yaws?uid=", e(UserId)]).

select_city(City) -> f([prefix(), "selcity.yaws?city=", e(City)]).

change_city(City) -> f([prefix(), "chgcity.yaws?city=", e(City)]).

auth(UserName, Password, CallbackUrl) -> f([prefix(), "auth.yaws?u=", e(UserName), "&p=", e(Password), "&cb=", e(CallbackUrl)]).

register(CallbackUrl) -> f([prefix(), "register.yaws?cb=", e(CallbackUrl)]).

error(Msg, Cb) -> error(Msg, "返回", Cb).
error(Msg, BtnTxt, Cb) -> error("出错了", "原因", Msg, BtnTxt, Cb).
error(Title1, Title2, Msg, BtnTxt, Cb) -> f([prefix(), "error.yaws?t1=", e(Title1), "&t2=", e(Title2), "&m=", e(Msg), "&b=", e(BtnTxt), "&cb=", e(Cb)]).

%prefix() -> "http://admin.weilink.me:8000/qianbao/".
%prefix() -> "http://localhost:8080/".
prefix() -> "/".
   
