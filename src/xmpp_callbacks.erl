%%% File    : xmpp_callbacks.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : Default XMPP callbacks
%%% Created : 17 Oct 2004 by Mickael Remond <mickael.remond@erlang-fr.org>

-module(xmpp_callbacks).

-export([presence/5, message/7, iq/7]).

-include("xmpp.hrl").

%% TODO: Use State in callbacks ?

presence(XMPP, Type, From, Attrs, Elts) ->
    ?INFO_MSG("Presence ~p: ~s (~p) (~p)", [XMPP, Type, Attrs, Elts]).

message(XMPP, Type, From, Subject, Body, Attrs, Elts) ->
    ?INFO_MSG("Message ~p: ~s (~p) (~p)", [XMPP, Type, Attrs, Elts]).

iq(XMPP, Type, From, QueryNS, PacketID, Attrs, Elts) ->
    ?INFO_MSG("IQ ~p: ~s (~p) (~p)", [XMPP, Type, Attrs, Elts]).
