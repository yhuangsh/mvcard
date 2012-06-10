-module(nihao).

-export([get_captcha/0,
	 verify_captcha/1,

	 login/2,
	 login/3,

	 logout/1,
	 change_password/3,
	 get_opaque/1,
	 set_opaque/2,

	 register/2,
	 reset_password/2,

	 start_captcha_sweeper/2,
	 stop_captcha_sweeper/0,

	 start_captcha_generator/4,
	 stop_captcha_generator/0]).

-define(USE_FENLIU_CALL, true).
-define(USE_FENLIU_CAST, true).
-include("../../fenliu/include/fenliu.hrl").

%% Captcha 
get_captcha() -> fl_call(nihao_captcha_server, get).
verify_captcha(CaptchaInfo) -> fl_call(nihao_captcha_server, {verify, CaptchaInfo}).

%% Login
login(UserInfo, ServiceInfo) -> login(UserInfo, undefined, ServiceInfo).
login(UserInfo, CaptchaInfo, ServiceInfo) when is_tuple(UserInfo), is_tuple(CaptchaInfo), is_tuple(ServiceInfo) ->
    fl_call(nihao_login_server, {login, {UserInfo, CaptchaInfo, ServiceInfo}}).

%% Session
logout(SessionInfo) ->
    fl_cast(nihao_session_server, {logout, SessionInfo}).

change_password(SessionInfo, CaptchaInfo, PasswordInfo) when is_tuple(SessionInfo), is_tuple(CaptchaInfo), is_tuple(PasswordInfo) ->
    fl_call(nihao_session_server, {change_password, SessionInfo, CaptchaInfo, PasswordInfo}).

get_opaque(SessionInfo) ->
    fl_call(nihao_session_server, {get_opaque, SessionInfo}).

set_opaque(SessionInfo, Opaque) ->
    fl_call(nihao_session_server, {set_opaque, SessionInfo, Opaque}).

%% Registration
register(UserInfo, CaptchaInfo) when is_tuple(UserInfo), is_tuple(CaptchaInfo) ->
    fl_call(nihao_register_server, {register, {UserInfo, CaptchaInfo}}).
reset_password(UserInfo, CaptchaInfo) when is_tuple(UserInfo), is_tuple(CaptchaInfo) ->
    fl_call(nihao_register_server, {reset_password, UserInfo, CaptchaInfo}).

%% Scheduled Activity
start_captcha_sweeper(TimeToStart, Interval) -> fl_call(nihao_scheduled_activity, {start_captcha_cache_sweeper, TimeToStart, Interval}).
stop_captcha_sweeper() -> fl_call(nihao_scheduled_activity, stop_captcha_ache_sweeper).

start_captcha_generator(TimeToStart, Interval, CodeLen, Count) -> fl_call(nihao_scheduled_activity, {start_captcha_generator, TimeToStart, Interval, CodeLen, Count}).
stop_captcha_generator() -> fl_call(nihao_scheduled_activity, stop_captchA_generator).


