%%% File    : xmpp.erl
%%% Author  : Mickael Remond <mremond@erlang-fr.org>
%%% Description : Main XMPP library client features
%%% Created : 14 Jul 2004 by Mickael Remond <mremond@erlang-fr.org>
-module(xmpp).
-behaviour(gen_fsm).

%% XMPP api
-export([start/0, start/1, start/2,
	 stop/1,
	 set_login_information/3,
	 set_login_information/4,
	 set_host/2,
	 set_host/3,
	 connect/1,
	 register_user/3,
	 set_callback_module/2,
	 message/4, message/5,
	 send/2,
	 subscribe/2
	]).

%% XMPP API: Those function are used to tweak the XMPP library
%% behaviour for automatic IQ response:
-export([set_client_info/3]).

%% FSM states
-export([unconfigured/3,
	 ready_to_connect/3,
	 wait_for_stream/2,
	 wait_for_authentication_method/2,
	 wait_for_authentication_result/2,
	 wait_for_registration_result/2,
	 wait_for_element/2
	 ]).

%% FSM exports
-export([start_link/2]).
-export([init/1,
	 handle_event/3,
         handle_sync_event/4,
         code_change/4,
         handle_info/3,
	 handle_event/3,
	 handle_sync_event/4,
         terminate/3]).

%% Internal exports
-export([receiver/2]).

-include("xmpp.hrl").


%% API
start() ->
    start_link(?defaultserver, ?defaultport).

start(Server) ->
    start_link(Server, ?defaultport).

start(Server, Port) ->
    start_link(Server, Port).

%% End gen_fsm XMPP process.
stop(Pid) ->
    Pid ! {stop}.

%% Launch connection
%% The connection is a synchronous call
connect(Pid) ->
    gen_fsm:sync_send_event(Pid, {connect}, ?calltimeout).
    
%% Set login information
%% Username, Authentication[, Resource]
%% Authentication is: {password, Password}
set_login_information(Pid, Username, Authentication) ->
    Pid ! {set_login_information, Username, Authentication},
    ok.
set_login_information(Pid, Username, Authentication, Resource) ->
    Pid ! {set_login_information, Username, Authentication, Resource},
    ok.

%% set_host(Pid, Hostname[, Port])
set_host(Pid, Hostname) ->
    Pid ! {set_host, Hostname},
    ok.
set_host(Pid, Hostname, Port) ->
    Pid ! {set_host, Hostname, Port},
    ok.

%% Allow customization of answers to the jabber:iq:version iq packet.
%% In your client code, you can optionnaly call:
%%   xmpp:set_client_info(XMPP, "Client Name ", "1.0"),
%% If not set, default values are provided.
set_client_info(Pid, ClientName, ClientVersion) ->
    Pid ! {set_client_info, ClientName, ClientVersion}.

%% Set the callback module that will received unprocessed XMPP messages
set_callback_module(Pid, Module) ->
    Pid ! {callback_module, Module},
    ok.

%% Send message
message(Pid, To, Type, Subject, Body) ->
%     Message = io_lib:format("<message to='~s' type='~s'><subject>~s</subject><body>~s</body></message>",
% 			    [To, Type, Subject, Body]),
    Message = io_lib:format("<message to='~s' type='~s'><subject>~s</subject><body><![CDATA[~s]]></body></message>",
 			    [To, Type, Subject, Body]),
    Pid ! lists:flatten(Message),
    ok.
message(Pid, To, Type, Body) ->
    Message = io_lib:format("<message to='~s' type='~s'><body>~s</body></message>",[To, Type, Body]),
    Pid ! lists:flatten(Message),
    ok.

%% Send IQ packet and wait for answer
%%  Note: simply use the send function to send iq result, because, we
%%  don't want to wait for answers in this case
iq(Pid, Type, To, Query) ->
    %% Generate a unique reference:
    {A,B,C} = erlang:now(),
    Ref=lists:flatten(io_lib:format("~p~p~p", [A,B,C])),

    IQStanza = case To of
		   [] ->
		       io_lib:format("<iq id='~s' type='~s'>~s</iq>", [Ref,Type,Query]);
		   _ ->
		       io_lib:format("<iq id='~s' to='~s' type='~s'>~s</iq>", [Ref,To,Type,Query])
	       end,
    Pid ! {iq, Ref, self(), lists:flatten(IQStanza)},
    receive
	{iqresult, Ref, Result} ->
	    Result
    after 60000 ->
	    {error, timeout}
    end.

%% Function that can be used by a server admin to register a given user:
register_user(Pid, User, Auth) ->
    {password, Password} = Auth,
    Query = io_lib:format("<query xmlns='jabber:iq:register'><username>~s</username><password>~s</password></query>", [User, Password]),
    iq(Pid, "set", "", Query).
   
%% Generic send function (does not wait for answer: Async send)
%% Pid = XMPP reference
send(Pid, XMPP_Packet) ->
    Pid ! lists:flatten(XMPP_Packet).

%% roster_add: Add an item to the users roster
subscribe(Pid, To) ->
    %% Subscribe to user presence:
    PresenceSubscribe = io_lib:format("<presence type='subscribe' to='~s'><status/></presence>",[To]),
    Pid ! lists:flatten(PresenceSubscribe),
    %% Add the user in the roster:
    {A,B,C} = erlang:now(),
    _Ref=lists:flatten(io_lib:format("~p~p~p", [A,B,C])),
    RosterUpdateQuery = io_lib:format("<query xmlns='jabber:iq:roster'><item jid='~s'/></query>", [To]),
    iq(Pid, "set", To, RosterUpdateQuery).

%% FSM callbacks
start_link(Server, Port) ->
    gen_fsm:start_link(?MODULE, [Server, Port], []).

init([Server, Port]) ->
    {ok, unconfigured, #state{host=Server, port=Port}}.


%% Internal function
open_client_stream(Socket, Host) ->
    case gen_tcp:send(Socket, xml_stream_client(Host)) of
	ok -> ok;
	_Other -> ?ERROR_MSG("Cannot open XMPP client stream~n", [])
    end.

%% Get available authentication method for a given user
get_authentication_methods(Socket, Username) ->
    %% Use id attributes to be able to match answer
    Message = io_lib:format("<iq type='get'><query xmlns='jabber:iq:auth'><username>~s</username></query></iq>", [Username]),
    gen_tcp:send(Socket, Message).

%% Send password authentication packet
%% TODO: Make resource configurable...
password_authentication(Socket, Username, Password, Resource) ->
    Message = io_lib:format("<iq type='set'><query xmlns='jabber:iq:auth'><username>~s</username><password>~s</password><resource>~s</resource></query></iq>", [Username, Password, Resource]),
    gen_tcp:send(Socket, Message).

%% TODO: Factorize with low level function generic iq
%% TODO: Handle iq packet id attributes to check that the answer
%%       really correspond to the needed answer
register(Socket, Username, Authentication, Resource) ->
    {password, Password} = Authentication,
    Message = io_lib:format("<iq type='set'><query xmlns='jabber:iq:register'><username>~s</username><password>~s</password><resource>~s</resource></query></iq>", [Username, Password, Resource]),
    gen_tcp:send(Socket, Message).    

send_presence(Socket, Show, Status) ->
    send_presence(Socket, "available", Show, Status).
send_presence(Socket, Type, Show, Status) ->
    Message = io_lib:format("<presence type='~s'><show>~s</show><status>~s</status></presence>", [Type, Show, Status]),
    gen_tcp:send(Socket, Message).

%% Callbacks:
%% We first need to set up mandatory parameters (Login informations)
handle_info({set_login_information, Username, Authentication, Resource}, StateName, StateData) ->
    NewState = case StateName of
		   unconfigured -> ready_to_connect;
		   OtherState   -> OtherState
	       end,
    {next_state, NewState, StateData#state{username = Username,
				   authentication = Authentication,
				   resource = Resource}};
handle_info({set_login_information, Username, Authentication}, StateName, StateData) ->
    NewState = case StateName of
		   unconfigured -> ready_to_connect;
		   OtherState   -> OtherState
	       end,
    {next_state, NewState, StateData#state{username = Username,
				   authentication = Authentication}};
%% Set host[, Port]
handle_info({set_host, Hostname}, StateName, StateData) ->
    {next_state, StateName, StateData#state{host = Hostname}};
handle_info({set_host, Hostname, Port}, StateName, StateData) ->
    {next_state, StateName, StateData#state{host = Hostname, port = Port}};
%% Client_info (jabber:iq:version) customization)
handle_info({set_client_info, ClientName, ClientVersion}, StateName, StateData) ->
    {next_state, StateName, StateData#state{client_name = ClientName,
					    client_version = ClientVersion}};
%% The first callbacks are used as an API in all states
%% Set the callback module that will "receive" unprocessed messages
handle_info({callback_module, Module}, StateName, StateData) ->
    ?INFO_MSG("Callback module is now ~p", [Module]),
    {next_state, StateName, StateData#state{callback_module=Module}};
%% Stop the XMPP gen_fsm
handle_info({stop}, _StateName, StateData) ->
    ?INFO_MSG("Disconnecting from the XMPP server~n", []),
    gen_tcp:send(StateData#state.socket, ?STREAM_TRAILER),
    gen_tcp:close(StateData#state.socket),

    %% Necessary to close the xml_stream parser:
    XMLStreamPid = StateData#state.xml_stream_pid,
    exit(XMLStreamPid, normal),

    {stop, normal, StateData};
%% IQ: Store the tuple {IQIDRef, CallerPid} in the StateData
handle_info({iq, Ref, CallerPid, Stanza}, StateName, StateData) ->
    %% ?INFO_MSG("Sending [~p]", [Stanza]),
    IQRefList = StateData#state.iq_ref_list,
    NewStateData = StateData#state{iq_ref_list=IQRefList++[{Ref,CallerPid}]},
    gen_tcp:send(StateData#state.socket, Stanza),    
    {next_state, StateName, NewStateData};
%% Use handle info to send data from the client to the server
%% (bidirectional)
%% Define the data that we can send in a wrapper API
handle_info(Message, StateName, StateData) ->
    %% ?INFO_MSG("Sending [~p]", [Message]),
    gen_tcp:send(StateData#state.socket, Message),
    {next_state, StateName, StateData}.

%% Synchrone calls to connect.
unconfigured({connect}, _From, StateData) ->
    ?ERROR_MSG("Mandatory parameters not set (Username, Authentication)", []),
    {reply, {error, unconfigured}, unconfigured, StateData#state{from_pid=undefined}}.
ready_to_connect({connect}, From, StateData) ->
    Host = StateData#state.host,
    Port = StateData#state.port,
    Socket = case gen_tcp:connect(Host, Port, [{packet,0},
						 binary,
						 {active, false},
						 {reuseaddr, true}], infinity) of
		 {ok, Sock}      -> 
		     ?INFO_MSG("Connected to ~s:~p~n", [Host,Port]),
		     Sock;
		 {error, Reason} -> ?ERROR_MSG("Connection error [~p]~n",
					       [Reason]),
				    exit(Reason)
	     end,

    open_client_stream(Socket, Host),

    %% Start receiver
    %% On the fly parsing library
    ok = erl_ddll:load_driver(ejabberd:get_so_path(), expat_erl),
    Pid = xml_stream:start(self()),
    _ReceiverPid = spawn(?MODULE, receiver, [Socket, Pid]),

    {next_state, wait_for_stream, StateData#state{socket = Socket, xml_stream_pid = Pid, from_pid=From}}.


wait_for_stream({xmlstreamstart,"stream:stream", _Attributes}, StateData) ->
    %% The stream is now open
    %% Retrieve supported authentication methods:
    get_authentication_methods(StateData#state.socket, StateData#state.username),
    {next_state, wait_for_authentication_method, StateData};
%% cannot receive open stream due to an XMPP error:
wait_for_stream({xmlstreamelement,{xmlelement, "stream:error", _Attrs,
				   [{xmlelement, Reason, _ErrorAttrs, []}]}}, StateData) ->
    ?ERROR_MSG("Stream error: ~p~n", [Reason]),
    {stop, xmpp_error_opening_stream, StateData}.

% Receiving elements of the form:
%
% {xmlstreamelement,{xmlelement,
%                                    "iq",
%                                    [{"type","result"}],
%                                    [{xmlelement,
%                                         "query",
%                                         [{"xmlns","jabber:iq:auth"}],
%                                         [{xmlelement,
%                                              "username",
%                                              [],
%                                              [{xmlcdata,"mremond"}]},
%                                          {xmlelement,"password",[],[]},
%                                          {xmlelement,"digest",[],[]},
%                                          {xmlelement,"resource",[],[]}]}]}}
%
wait_for_authentication_method({xmlstreamelement, {xmlelement, "iq", _Attrs, Elts}}, StateData) ->
    %% Extracting 
    [{xmlelement, "query", [{"xmlns","jabber:iq:auth"}],SubElts}] = Elts,

    %% Use password authentication first
    %% TODO: support digest and resource
    case lists:keysearch("password", 2, SubElts) of
	false -> ?ERROR_MSG("Jabberlang library only support password authentication for now. "
			    "Password authentication is not accepted by the server~n", []),
		 {stop, authentication_method_not_supported, StateData};
	{value, {xmlelement, "password", _, _}} ->
	    %% Authentication
	    {password, Password} = StateData#state.authentication,
	    password_authentication(StateData#state.socket,
				    StateData#state.username,
				    Password,
				    StateData#state.resource),
	    {next_state, wait_for_authentication_result, StateData}
	end;
%% Error: Disconnected
wait_for_authentication_method({xmlstreamelement,{xmlelement, "stream:error", _Attrs,
						  [{xmlcdata,"Disconnected"}]}}, StateData) ->
    ?ERROR_MSG("Stream error: ~p~n", ["Disconnected"]),
    {stop, xmpp_error, StateData};
%% General error case:
wait_for_authentication_method({xmlstreamelement,{xmlelement, "stream:error", _Attrs,
						  [{xmlelement, Reason, _ErrorAttrs, []}]}}, StateData) ->
    ?ERROR_MSG("Stream error: ~p~n", [Reason]),
    {stop, xmpp_error, StateData}.


%% Authentication successfull
wait_for_authentication_result({xmlstreamelement,{xmlelement,"iq",[{"type","result"}],[]}}, StateData) ->
    ?INFO_MSG("Authentication successfull. You are logged in as ~p ~n", [StateData#state.username]),
    %% After authentication, send presence information (TODO: Move that after roster retrieval)
    send_presence(StateData#state.socket, StateData#state.show, StateData#state.status),
    gen_fsm:reply(StateData#state.from_pid, ok),
    {next_state, wait_for_element, StateData#state{from_pid=undefined}};
%% Error: Disconnected
wait_for_authentication_result({xmlstreamelement,{xmlelement, "stream:error", _Attrs,
						  [{xmlcdata,"Disconnected"}]}}, StateData) ->
    ?ERROR_MSG("Stream error: ~p~n", ["Disconnected"]),
    {stop, xmpp_error, StateData};
%% If unauthorized: Try to register user...
%%  TODO: I should check that the packet here is the answer to the login packet.
wait_for_authentication_result(Other, StateData) ->
    case StateData#state.auto_registration of
	false ->
	    ?ERROR_MSG("Authentication failed (~p)", [Other]),
	    gen_fsm:reply(StateData#state.from_pid, {error, authentication_failed}),
	    {stop, authentication_failed, StateData#state{from_pid=undefined}};
	true ->
	    ?INFO_MSG("Authentication failed (~p)", [Other]),
	    ?INFO_MSG("Trying to register user ~s", [StateData#state.username]),
	    register(StateData#state.socket, StateData#state.username,
			  StateData#state.authentication, StateData#state.resource),
	    {next_state, wait_for_registration_result, StateData}
    end.

wait_for_registration_result({xmlstreamelement,{xmlelement,"iq",Attrs,_SubElts}}, StateData) ->
    case lists:keysearch("type", 1, Attrs) of
	%% Registration successfull:
	{value, {"type", "result"}} ->
	    ?INFO_MSG("Successfully registered user ~s", [StateData#state.username]),
	    %% After registration, we need to redo authentication
  	    {password, Password} = StateData#state.authentication,
  	    password_authentication(StateData#state.socket,
  				    StateData#state.username,
  				    Password,
  				    StateData#state.resource),
  	    {next_state, wait_for_authentication_result, StateData#state{auto_registration=false}};
%%  TODO: I should check that the packet here is the answer to the login packet.	
	Other ->
	    ?ERROR_MSG("Authentication and registration failed for user ~s (~p)", [StateData#state.username, Other]),
	    gen_fsm:reply(StateData#state.from_pid, {error, registration_failed}),
	    {stop, registration_failed, StateData#state{from_pid=undefined}}
    end;
%% Error: Disconnected
wait_for_registration_result({xmlstreamelement,{xmlelement, "stream:error", _Attrs,
						  [{xmlcdata,"Disconnected"}]}}, StateData) ->
    ?ERROR_MSG("Stream error: ~p~n", ["Disconnected"]),
    {stop, xmpp_error, StateData};
%% Error: General case:
wait_for_registration_result({xmlstreamelement,{xmlelement, "stream:error", _Attrs,
						  [{xmlelement, Reason, _ErrorAttrs, []}]}}, StateData) ->
    ?ERROR_MSG("Stream error: ~p~n", [Reason]),
    {stop, xmpp_error, StateData}.

wait_for_element({xmlstreamelement,{xmlelement, "stream:error", _Attrs,
				    [{xmlelement, Reason, _ErrorAttrs, []}]}}, StateData) ->
    ?ERROR_MSG("Stream error: ~p~n", [Reason]),
    {next_state, wait_for_element, StateData};
%% TODO: End of stream should be handle in all state (as well as stream:error)
wait_for_element({xmlstreamend,"stream:stream"}, StateData) ->
    ?INFO_MSG("Disconnected~n", []),
    {stop, normal, StateData};
%% Message packet:
wait_for_element({xmlstreamelement,{xmlelement,"message",Attrs,Elts}}, StateData) ->
    case lists:keysearch("type", 1, Attrs) of
	false ->
	    process_message(
	      self(),
	      StateData#state.socket,
	      StateData#state.callback_module,
	      "normal", Attrs, Elts);
	{value, {"type", Type}} ->
	    process_message(
	      self(),
	      StateData#state.socket,
	      StateData#state.callback_module,
	      Type, Attrs, Elts)
    end,
    {next_state, wait_for_element, StateData};
%% presence packet:
wait_for_element({xmlstreamelement,{xmlelement,"presence",Attrs,Elts}}, StateData) ->
    case lists:keysearch("type", 1, Attrs) of
	false -> process_presence(
		   self(),
		   StateData#state.socket,
		   StateData#state.callback_module,
		   "available", Attrs, Elts);
	{value, {"type", Type}} ->
	    process_presence(
	      self(),
	      StateData#state.socket,
	      StateData#state.callback_module,
	      Type, Attrs, Elts)
    end,
    {next_state, wait_for_element, StateData};
%% Process IQ element: If this is a result: send the data back to the
%% waiting process. Otherwise use callback to pass it to the standard callback module.
wait_for_element(XML = {xmlstreamelement,{xmlelement,"iq",Attrs,Elts}}, StateData) ->
    %%If this is a result: send the data back to the waiting process
    NewStateData = 
	case lists:keysearch("type", 1, Attrs) of
	    {value, {"type", "result"}} ->
		case lists:keysearch("id", 1, Attrs) of
		    {value, {"id", Ref}} ->
			NewIQRefList = process_iq_result(StateData#state.iq_ref_list,
							 Ref,
							 XML),
			StateData#state{iq_ref_list=NewIQRefList};
		    _ -> ?ERROR_MSG("IQ result without id attribute: ~p~n", [XML]),
			 StateData
		end;
	    {value, {"type", "error"}} ->
		case lists:keysearch("id", 1, Attrs) of
		    {value, {"id", Ref}} ->
			NewIQRefList = process_iq_result(StateData#state.iq_ref_list,
							 Ref,
							 XML),
			StateData#state{iq_ref_list=NewIQRefList};
		    _ -> ?ERROR_MSG("IQ error without id attribute: ~p~n", [XML]),
			 StateData
		end;
	    {value, {"type", Type}} ->
		process_iq(
		  self(),
		  StateData#state.socket,
		  StateData#state.callback_module,
		  Type, Attrs, Elts,
		  StateData),
		StateData
	end,
    {next_state, wait_for_element, NewStateData};
wait_for_element(XML, StateData) ->
    ?INFO_MSG("XML element: ~p~n", [XML]),
    {next_state, wait_for_element, StateData}.

%% Someone is asking for presence subscription
%% By default, enable subscription and add this people to the roster
%% TODO: Use a parameter to determine if autosubscribtion is allowed
%% TODO: see status element content for subscribtion reason:
%% 		   [{xmlelement,
%% 		     "status",
%% 		     [],
%% 		     [{xmlcdata,
%% 		       "I would like to add you to my roster."},
%% 		      {xmlcdata,"\n"}]}]}}
process_presence(_Pid, Socket, _Module, "subscribe", Attrs, _Elts) ->
    {value, {"from", Who}} = lists:keysearch("from", 1, Attrs),
    PresenceTag = "<presence type='~s' to='~s'/>",
    Allowsubscribtion = io_lib:format(PresenceTag,["subscribed", Who]),
    gen_tcp:send(Socket, Allowsubscribtion),
    Subscribe = io_lib:format(PresenceTag,["subscribe", Who]),
    gen_tcp:send(Socket, Subscribe);
process_presence(_Pid, _Socket, _Module, "subscribed", Attrs, _Elts) ->
    {value, {"from", Who}} = lists:keysearch("from", 1, Attrs),
    ?INFO_MSG("Now subscribed to ~s", [Who]);
%% Generic presence: use callbacks
%% TODO: For available presence: Extract show, status (and priority ?)
process_presence(Pid, _Socket, Module, Type, Attrs, Elts) ->
    {value, {"from", Who}} = lists:keysearch("from", 1, Attrs),
    Module:presence(Pid, Type, Who, Attrs, Elts).

%% The client has received a message
%% Use callback module to "send" it to the client
process_message(Pid, _Socket, Module, Type, Attrs, SubElts) ->
    {value, {"from", Who}} = lists:keysearch("from", 1, Attrs),
    Body = get_subelts_cdata("body", SubElts),
    Subject = get_subelts_cdata("subject", SubElts),
    Module:message(Pid, Type, Who, Subject, Body, Attrs, SubElts).

%% The client has received an iq query
process_iq(Pid, _Socket, Module, Type, Attrs, SubElts, StateData) ->
    case lists:keysearch("id", 1, Attrs) of
	{value, {"id", Ref}} ->
	    {value, {"from", Who}} = lists:keysearch("from", 1, Attrs),
	    %% Extract query namespace
	    QueryNS = get_subelts_attr("query", "xmlns", SubElts),
	    automatic_iq(Pid, Module, Type, Who, QueryNS, Ref, Attrs, SubElts, StateData);
	_ ->
	    ?ERROR_MSG("IQ query without id. Ignoring [~p - ~p]", [Attrs, SubElts])
    end.

%% Send the IQ result to the waiting process
%% TODO: Get Namespace and process some of them from the generic
%%       client module
process_iq_result(IQRefList, Ref, Stanza) ->
    case lists:keysearch(Ref,1,IQRefList) of
	{value, {Ref, Pid}} ->
	    ?INFO_MSG("Received IQ answer: ~p~n", [Stanza]),
	    Pid ! {iqresult, Ref, Stanza},
	    _NewIQRefList = lists:keydelete(Ref, 1, IQRefList);
	_Other ->
	    ?ERROR_MSG("IQ result without existing id reference: ~p~n", [Stanza]),
	    IQRefList
    end.

%% Get content of a tag, given a list of elements as input.
%% Warning, this function is not recursive: Only search in a list of element
get_subelts_cdata(Tagname, SubElts) ->
    case lists:keysearch(Tagname, 2, SubElts) of
	false -> "";
	{value, Tag} -> xml:get_tag_cdata(Tag)
    end.

%% Get tag attribute value, given a list of elements as input.
%% Warning, this function is not recursive: Only search in a list of element
get_subelts_attr(Tagname, AttrName, SubElts) ->
    case lists:keysearch(Tagname, 2, SubElts) of
	false -> "";
	{value, Tag} -> xml:get_tag_attr_s(AttrName, Tag)
    end.

%% Wrapper pour les requ�tes iq synchrone vers un �quivalent de call
%% en Erlang.  Introduire un mechanisme pour redispatcher la r�ponse
%% vers le bon process qui peut se mettre en attente. Le process qui
%% r�cup�re le mode d'authentification se met en attente sur un
%% receive {iq, result, Id, Content}, avec l'Id attribu�. Il d�clare
%% aussi le mapping entre Id et PId � un process dispatcheur.. Lorsque
%% le process de r�ception (receiver) r�cup�rer un iq type result, il
%% r�cup�re l'id, � partir de l� retrouve le pid du process qui a �mis
%% l'id, et renvoie la r�ponse � ce processus, qui r�cup�re la r�ponse
%% et se trouve ainsi d�bloqu�. Pr�voir un timeout pour le processus
%% en attente, (mais assez long).

receiver(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} -> 
	    xml_stream:send_text(Pid, binary_to_list(Data)),
	    receiver(Socket, Pid);
	{error, _Reason} -> 
	    ok %% End receiver TODO: End other process
    end.


%% Formatting Jabber XML
xml_stream_client(Server) ->
    io_lib:format(?STREAM_CLIENT_HEADER, [Server]).

%% Automatic handler for jabber:iq:version queries
automatic_iq(Pid, _Module, _Type, From, "jabber:iq:version", PacketID, _Attrs, _SubElts, StateData) ->
    ClientName    = StateData#state.client_name,
    ClientVersion = StateData#state.client_version,
    %% Format IQ result:
    XMPP_Packet = xmpp_iq:version_r(PacketID, From, ClientName, ClientVersion),
    %% The the resulting packet (async).
    xmpp:send(Pid, XMPP_Packet);

%% No automatic IQ reply: Let the callback module handle the answer.
automatic_iq(Pid, Module, Type, From, QueryNS, PacketID, Attrs, SubElts, _StateData) ->
    Module:iq(Pid, Type, From, QueryNS, PacketID, Attrs, SubElts).

%% OTP related code

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.


handle_event(Event, StateName, StateData) ->
    io:format("handle_event event: ~p ~n", [Event]),
    {next_state, StateName, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    io:format("handle_sync_event event: ~p ~n", [Event]),
    Reply = ok,
    {reply, Reply, StateName, StateData}.



%% R�cup�ration de statistiques
% OUT(1,mremond@localhost/tkabber):
% <iq id='12'
% 	to='localhost'
% 	type='get'
% 	xml:lang='fr'>
%   <query xmlns='http://jabber.org/protocol/stats'>
%     <stat name='users/online'/>
%   </query>
% </iq>
% IN(1,mremond@localhost/tkabber):
% <iq from='localhost'
% 	to='mremond@localhost/tkabber'
% 	id='12'
% 	type='result'>
%   <query xmlns='http://jabber.org/protocol/stats'>
%     <stat name='users/online'
% 	units='users'
% 	value='1'/>
%   </query>
% </iq>
% OUT(1,mremond@localhost/tkabber):
% <iq id='13'
% 	to='localhost'
% 	type='get'
% 	xml:lang='fr'>
%   <query xmlns='http://jabber.org/protocol/stats'>
%     <stat name='users/total'/>
%   </query>
% </iq>
% IN(1,mremond@localhost/tkabber):
% <iq from='localhost'
% 	to='mremond@localhost/tkabber'
% 	id='13'
% 	type='result'>
%   <query xmlns='http://jabber.org/protocol/stats'>
%     <stat name='users/total'
% 	units='users'
% 	value='1'/>
%   </query>
% </iq>
