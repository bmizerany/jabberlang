%%% File    : xmpp_echo_behaviour.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : This is a simple XMPP client written with Jabberlang
%%%               XMPP library (Echo)
%%%               This is the bahviour based version
%%% Created : 17 Oct 2004 by Mickael Remond <mickael.remond@erlang-fr.org>

-module(xmpp_echo_behaviour).
-behaviour(gen_xmpp_client).

%% XMPP configuration attributes
-host("localhost").
%% -port(5222).
-username("echo").
-authentication({password,"echo"}).
-resource("Erlang echo behaviour").

%% Optional:
-include("xmpp.hrl").

%% Behaviour callbacks
-export([init/2,
	 presence/5,
	 message/7,
	 iq/7]).

-export([start/0]).

%% Module API
start() ->
    gen_xmpp_client:start_link(?MODULE, [], []).

%% gen_xmpp_client callbacks
init(Args, State) ->
    {ok, State}.

%% Ignore presence packets
presence(_XMPP, _Type, _From, _Attrs, _Elts) ->
    ok.

%% Echo: Reply to messages with the same message
message(XMPP, Type, From, Subject, Body, Attrs, _Elts) ->
    xmpp:message(XMPP, From, Type, Subject, Body).

%% Ignore IQ query
iq(XMPP, Type, From, QueryNS, PacketID, Attrs, SubElts) ->
    ok.
