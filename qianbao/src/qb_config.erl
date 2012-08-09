-module(qb_config).
-compile(export_all).

default_city() -> "上海".
    
is_valid_city(City) -> is_valid_city_1(qb_tab_city:read(City)).
is_valid_city_1({ok, _}) -> true;
is_valid_city_1({nok, _}) -> false.
