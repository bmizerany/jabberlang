%%% File    : xmpp_iq.erl
%%% Author  : Mickael Remond <mremond@erlang-fr.org>
%%% Description : This module is an helper to format XMPP IQ queries
%%%               and answers
%%% Created :  2 Jan 2005 by Mickael Remond <mremond@erlang-fr.org>
-module(xmpp_iq).

%% Note that the functions naming convention being used is the following:
%% Suffixes:
%%   _r = IQ result
%%   _s = IQ set
%%   _g = IQ get

-export([iq/4,
	 iq_r/4]).
-export([version_r/4]).

%% ====
%% Generic function

%% IQ result formatting
%% IQ_QueryID = string: Reference (ID) of the IQ get or set query
%% Namespace = string:  to use for the result. (same as query)
%% Content = string: Content of the IQ query tag.
%% Return = string: Complete IQ packet
iq_r(IQ_QueryID, To, Namespace, Content) ->
    Query = io_lib:format("<query xmlns='~s'>~s</query>",
			  [Namespace, Content]),
    iq(IQ_QueryID, "result", To, Query).

%% Generic IQ formatter.
%% IQ_QueryID = string: Reference (ID) of the IQ get or set query
%% Type = Type of the query
%% To = IQ packet target can be [] or JID of the IQ target
%% Query = string: Content of the IQ query tag.
%% Return = string: Complete IQ packet
iq(IQ_QueryID, Type, To, Query) ->
    case To of
	[] ->
	    io_lib:format("<iq id='~s' type='~s'>~s</iq>",
			  [IQ_QueryID, Type, Query]);
	_ ->
	    io_lib:format("<iq id='~s' to='~s' type='~s'>~s</iq>",
			  [IQ_QueryID, To, Type, Query])
    end.

%% Specific IQ formatting functions
version_r(IQ_QueryID, To, ClientName, ClientVersion) ->
    _Os = os(),
    Content = io_lib:format("<name>~s</name><version>~s</version><os>~s</os>",
			    [ClientName, ClientVersion, os()]),
    iq_r(IQ_QueryID, To, "jabber:iq:version", Content).

%% Return = string (deep list): representation of the OS version
os() ->
    [os_type(), " ", os_version()].
    
os_type() ->
    case os:type() of
	{Osfamily, Osname} ->
	    [atom_to_list(Osfamily), " ", atom_to_list(Osname)];
	Osfamily ->
	    atom_to_list(Osfamily)
    end.

os_version() ->
    case os:version() of
	{Major, Minor, Release} ->
	    [integer_to_list(Major), ".", integer_to_list(Minor), ".",
	     integer_to_list(Release)];
	VersionString ->
	    VersionString
    end.
    
