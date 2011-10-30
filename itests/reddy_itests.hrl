-define(CONNECT(), reddy_conn:connect(os:getenv("REDIS_HOST"), list_to_integer(os:getenv("REDIS_PORT")))).
-define(POOL(Name, Count), reddy_pool:new_pool(Name, [{ip, os:getenv("REDIS_HOST")}, {port, list_to_integer(os:getenv("REDIS_PORT"))},
                                                      {count, Count}])).
-define(WAIT(X), fun() -> timer:sleep(X) end()).
