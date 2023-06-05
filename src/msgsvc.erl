-module(msgsvc).

-export([conf/1]).

conf(http_bind_addr) -> os:getenv("MSGSVC_HTTP_BIND_ADDRESS", "127.0.0.1");
conf(http_bind_port) -> os:getenv("MSGSVC_HTTP_BIND_PORT", "8080").
