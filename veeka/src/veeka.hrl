

-define(SESSION_TABLE, veeka_session_table).
-record(session, {id, sales_id}).

-define(SALES_TABLE, veeka_sales_table).
-record(sales, {id, name, pin}).
