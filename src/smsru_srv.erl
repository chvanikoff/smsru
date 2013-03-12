-module(smsru_srv).
-author('chvanikoff <chvanikoff@gmail.com>').

-behaviour(gen_server).

%% Gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% API
-export([start_link/0]).

-record(state, {
    login = nil,
    password = nil
}).

-define(STATE_MATCH, (State = #state{login = Login, password = Password})).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #state{}}.


handle_call({send, Proplist}, _From, ?STATE_MATCH) ->
    URL = "http://sms.ru/sms/send",
    To = proplists:get_value(to, Proplist, ""),
    Text = proplists:get_value(text, Proplist, ""),
    From = proplists:get_value(from, Proplist, ""),
    Time = proplists:get_value(time, Proplist, ""),
    Translit = proplists:get_value(translit, Proplist, "0"),
    Test = proplists:get_value(test, Proplist, "0"),
    Partner_id = proplists:get_value(partner_id, Proplist, "0"),
    [{token, Token}, {sha512, Sha512}] = auth_params(Password),
    Data = "to=" ++ To
        ++ "&text=" ++ edoc_lib:escape_uri(Text)
        ++ "&from=" ++ From
        ++ "&time=" ++ Time
        ++ "&translit=" ++ Translit
        ++ "&test=" ++ Test
        ++ "&partner_id=" ++ Partner_id
        ++ "&login=" ++ Login
        ++ "&token=" ++ Token
        ++ "&sha512=" ++ Sha512,
    {ok, {{"HTTP/1.1", 200, "OK"}, _, Response}}
        = httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Data}, [], []),
    {reply, Response, State};

handle_call({status, Id}, _From, ?STATE_MATCH) ->
    {reply, ok, State};

handle_call({cost, Proplist}, _From, ?STATE_MATCH) ->
    {reply, ok, State};

handle_call(balance, _From, ?STATE_MATCH) ->
    {reply, ok, State};

handle_call(limit, _From, ?STATE_MATCH) ->
    {reply, ok, State};

handle_call(senders, _From, ?STATE_MATCH) ->
    {reply, ok, State};

handle_call(get_token, _From, State) ->
    Token = get_token(),
    {reply, Token, State};

handle_call(check, _From, ?STATE_MATCH) ->
    {reply, ok, State};

handle_call({add, Proplist}, _From, ?STATE_MATCH) ->
    {reply, ok, State};

handle_call({del, Stoplist_phone}, _From, ?STATE_MATCH) ->
    {reply, ok, State};

handle_call(get, _From, ?STATE_MATCH) ->
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast({config, Config}, _State) ->
    {noreply, #state{
        login = proplists:get_value(login, Config, nil),
        password = proplists:get_value(password, Config, nil)
    }};

handle_cast({mail, Proplist}, ?STATE_MATCH) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

get_token() ->
    {ok, {{"HTTP/1.1", 200, "OK"}, _, Token}} = httpc:request("http://sms.ru/auth/get_token"),
    Token.

http_get(Url) ->
    ok.

auth_params(Password) ->
    Token = get_token(),
    Sha512 = sha512(Password ++ Token),
    [{token, Token}, {sha512, Sha512}].

sha512(String) ->
    <<Binary:512/big-unsigned-integer>> = crypto:hash(sha512, String),
    lists:flatten(io_lib:format("~128.16.0b", [Binary])).
