%%% File    : xmpp_erlang_example.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : illustrate Erlang to Erlang communications
%%% Created : 18 Oct 2004 by Mickael Remond <mickael.remond@erlang-fr.org>
%%% Start example:
%%% {ok, XMPP}  = xmpp_erlang_example:start(ping).
%%% {ok, XMPP2} = xmpp_erlang_example:start(ping).
%%% xmpp_erlang_example:initialize_counter_pong(XMPP, 1).


-module(xmpp_erlang_example).
-behaviour(gen_xmpp_client).

%% XMPP configuration attributes
-host("localhost").
-username("jabberlang").
-authentication({password,"jabberlang"}).

%% Optional:
-include("xmpp.hrl").

%% Behaviour callbacks
-export([init/2,
	 presence/5,
	 message/7,
	 iq/7]).

-export([start/1,
	 initialize_counter_pong/2]).

%% Module API
start(Resource) ->
    gen_xmpp_client:start_link(?MODULE, [{resource,Resource}], []).

initialize_counter_pong(XMPP, Counter) ->
    xmpp:message(XMPP, "mremond@localhost/pong", "chat", "", encode(Counter)).

%% gen_xmpp_client callbacks
init(_Args, State) ->
    {ok, State}.

%% Ignore presence packets
presence(_XMPP, _Type, _From, _Attrs, _Elts) ->
    ok.

%% Reply to increment
message(XMPP, Type, From, Subject, Body, _Attrs, _Elts) ->
    Value = decode(Body),
    io:format("Value: ~p~n", [Value]),
    xmpp:message(XMPP, From, Type, Subject, encode(Value+1)).

%% Ignore IQ query
iq(_XMPP, _Type, _From, _QueryNS, _PacketID, _Attrs, _SubElts) ->
    ok.

%% Take term and return encoded string
encode(Term) ->
    httpd_util:encode_base64(binary_to_list(term_to_binary(Term))).

%% Take String and return term
decode(String) ->
    binary_to_term(list_to_binary(httpd_util:decode_base64(String))).
