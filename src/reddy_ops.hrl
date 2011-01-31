%% List ops
-define(BLPOP, "BLPOP").
-define(BRPOP, "BRPOP").
-define(BRPOPLPUSH, "BRPOPLPUSH").
-define(LINDEX, "LINDEX").
-define(LINSERT, "LINSERT").
-define(LLEN, "LLEN").
-define(LPOP, "LPOP").
-define(LPUSH, "LPUSH").
-define(LPUSHX, "LPUSHX").
-define(LRANGE, "LRANGE").
-define(LREM, "LREM").
-define(LSET, "LSET").
-define(LTRIM, "LTRIM").
-define(RPOP, "RPOP").
-define(RPOPLPUSH, "RPOPLPUSH").
-define(RPUSH, "RPUSH").
-define(RPUSHX, "RPUSHX").

%% Keys ops
-define(DEL, "DEL").
-define(EXISTS, "EXISTS").
-define(EXPIRE, "EXPIRE").
-define(EXPIREAT, "EXPIREAT").
-define(KEYS, "KEYS").
-define(MOVE, "MOVE").
-define(PERSIST, "PERSIST").
-define(RANDOMKEY, "RANDOMKEY").
-define(RENAME, "RENAME").
-define(RENAMENX, "RENAMENX").
-define(TTL, "TTL").
-define(TYPE, "TYPE").


%% Set ops
-define(SADD, "SADD").
-define(SCARD, "SCARD").
-define(SDIFF, "SDIFF").
-define(SDIFFSTORE, "SDIFFSTORE").
-define(SINTER, "SINTER").
-define(SINTERSTORE, "SINTERSTORE").
-define(SISMEMBER, "SISMEMBER").
-define(SMEMBERS, "SMEMBERS").
-define(SMOVE, "SMOVE").
-define(SPOP, "SPOP").
-define(SRANDMEMBER, "SRANDMEMBER").
-define(SREM, "SREM").
-define(SUNION, "SUNION").
-define(SUNIONSTORE, "SUNIONSTORE").

%% Server ops
-define(INFO, "INFO").
-define(AUTH, "AUTH").

%% Health ops
-define(PING, "PING").
-define(ECHO, "ECHO").
