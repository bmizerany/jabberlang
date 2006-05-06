%%% File    : xmpp_echo.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : This is a simple XMPP client written with Jabberlang
%%%               XMPP library (Echo)
%%% Created : 17 Oct 2004 by Mickael Remond <mickael.remond@erlang-fr.org>

-module(xmpp_echo).

-export([start/1,
	 %% XMPP callbacks:
	 presence/5,
	 message/7,
	 iq/7]).

start(Host) ->
    {ok, XMPP} = xmpp:start(Host),
    xmpp:set_login_information(XMPP, "echo",{password,"echo"},"ErlangEcho"),
    %% Optionnal: xmpp:set_client_info(XMPP, "Echo client", "1.0"),
    xmpp:set_callback_module(XMPP, ?MODULE),
    xmpp:connect(XMPP).

%% Ignore presence packets
presence(_XMPP, _Type, _From, _Attrs, _Elts) ->
    ok.

%% Echo: Reply to messages with the same message
message(XMPP, Type, From, Subject, Body, _Attrs, _Elts) ->
    xmpp:message(XMPP, From, Type, Subject, Body).

%% Ignore (but displays) IQ queries
iq(_XMPP, Type, _From, QueryNS, _PacketID, _Attrs, _SubElts) ->
    io:format("IQ ~s ~s~n", [Type, QueryNS]),
    ok.
