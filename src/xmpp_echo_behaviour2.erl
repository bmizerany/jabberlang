%%% File    : xmpp_echo_behaviour2.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : This is a simple XMPP client written with Jabberlang
%%%               XMPP library (Echo)
%%%               This is the bahviour based version
%%%               It uses start arguments instead of module attributes
%%% Created : 17 Oct 2004 by Mickael Remond <mickael.remond@erlang-fr.org>

-module(xmpp_echo_behaviour2).
-behaviour(gen_xmpp_client).

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
    Args = [
	    {host, "localhost"},
	    {port, 5222},
	    {username, "echo"},
	    {authentication, {password, "echo"}},
	    {resource, "Echo XMPP behaviour 2"}],
    gen_xmpp_client:start_link(?MODULE, Args, []).

%% gen_xmpp_client callbacks
init(_Args, State) ->
    {ok, State}.

%% Ignore presence packets
presence(_XMPP, _Type, _From, _Attrs, _Elts) ->
    ok.

%% Echo: Reply to messages with the same message
message(XMPP, Type, From, Subject, Body, _Attrs, _Elts) ->
    xmpp:message(XMPP, From, Type, Subject, Body).

%% Ignore IQ query
iq(_XMPP, _Type, _From, _QueryNS, _PacketID, _Attrs, _SubElts) ->
    ok.
