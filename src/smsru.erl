-module(smsru).

%% API
-export([
	start/0, start/1,
	send/2, send/3,
	mail/2, mail/3,
	status/1,
	cost/1, cost/2,
	balance/0,
	limit/0,
	senders/0,
	get_token/0,
	check/0,
	add/2,
	del/1,
	get/0,
	config/1
]).

-define(SERVER, smsru_srv).

%% ===================================================================
%% API
%% ===================================================================

start() ->
	Config = case file:consult("priv/smsru.config") of
		{ok, [Result]} -> Result;
		_ -> []
	end,
	start(Config).

start(Config) ->
	ok = ensure_started([crypto, inets, smsru]),
	config(Config).

send(To, Text) ->
	send(To, Text, []).

send(To, Text, Params) ->
	gen_server:call(?SERVER, {send, [{to, To}, {text, Text}] ++ Params}).

mail(To, Text) ->
	mail(nil, To, Text).

mail(From, To, Text) ->
	gen_server:cast(?SERVER, {mail, [{from, From}, {to, To}, {text, Text}]}).

status(Id) ->
	gen_server:call(?SERVER, {status, Id}).

cost(To) ->
	cost(To, []).

cost(To, Params) ->
	gen_server:call(?SERVER, {cost, [{to, To}] ++ Params}).

balance() ->
	gen_server:call(?SERVER, balance).

limit() ->
	gen_server:call(?SERVER, limit).

senders() ->
	gen_server:call(?SERVER, senders).

get_token() ->
	gen_server:call(?SERVER, get_token).

check() ->
	gen_server:call(?SERVER, check).

add(Stoplist_phone, Stoplist_text) ->
	gen_server:call(?SERVER, {add, [{stoplist_phone, Stoplist_phone}, {stoplist_text, Stoplist_text}]}).

del(Stoplist_phone) ->
	gen_server:call(?SERVER, {del, Stoplist_phone}).

get() ->
	gen_server:call(?SERVER, get).

config(Config) ->
	gen_server:cast(?SERVER, {config, Config}).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok -> ensure_started(Apps);
		{error, {already_started, App}} -> ensure_started(Apps)
	end.