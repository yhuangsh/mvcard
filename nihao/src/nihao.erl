-module(nihao).

-export([auth/2, 
	 register/4,
	 update/4,
	 delete/2]).

-include("../../fenliu/include/fenliu.hrl").

auth(Id, Password) -> fl_call(nihao_auth_server, {auth, Id, Password}).
register(Id, Email, Mobile, Password) -> fl_call(nihao_register_server, {register, Id, Email, Mobile, Password}).
update(Id, Email, Mobile, Password) -> fl_call(nihao_profile_server, {update, Id, Email, Mobile, Password}).
delete(Id, Password) -> fl_call(nihao_profile_server, {delete, Id, Password}).

