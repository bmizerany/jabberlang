%%% File    : gen_xmpp_client.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : Generic behaviour to develop XMPP client.
%%%               XMPP generic client can be either parametrized:
%%%               - With module attributes
%%%               - With function call parameters 
%%% Created : 18 Oct 2004 by Mickael Remond <mickael.remond@erlang-fr.org>

-module(gen_xmpp_client).

-export([behaviour_info/1]).

-export([start/3, start_link/3]).

-include("xmpp.hrl").

%% defining expected callbacks in the behaviour code
behaviour_info(callbacks) ->
    [{init, 2},
     {presence, 5},
     {message, 7},
     {iq, 7}];
behaviour_info(_Other) ->
    undefined.

%% Options are gen_fsm options
start(_Module, _Args, _Options) ->
    ok.

%% Options are gen_fsm options
start_link(Module, Args, _Options) ->
    {ok, XMPP} = xmpp:start(),
    xmpp:set_callback_module(XMPP, Module),
    configure_xmmp_client(XMPP, Module, Args),
    State = #state{}, %% TODO: Take state from the XMPP module
    {ok, _NewState} = Module:init(Args, State),
    xmpp:connect(XMPP),
    {ok, XMPP}.

%% Set-up initial XMPP parameters
configure_xmmp_client(XMPP, Module, Args) ->
    %% Host, Port
    Host = get_argument(Module, host, Args),
    Port = get_argument(Module, port, Args),
    set_host(XMPP, Host, Port),
    %% Login informations
    Username       = get_argument(Module, username, Args),
    Authentication = get_argument(Module, authentication, Args),
    Resource       = get_argument(Module, resource, Args),
    case lists:member(undefined, [Username, Authentication]) of
	%% If username or authentication is not set
	true -> 
	    ?ERROR_MSG("username or authentication undefined", []);
	false ->
	    xmpp:set_login_information(XMPP, Username,Authentication,Resource)
    end.

%% Set XMPP hostname/port parameters
%% Do nothing
set_host(_XMPP, undefined, _) ->
    ok;
%% Only set hostname
set_host(XMPP, Host, undefined) ->
    xmpp:set_host(XMPP, Host);
%% Set hostname and port
set_host(XMPP, Host, Port) ->
    xmpp:set_host(XMPP, Host, Port).
    

%% Getting XMPP parameters
%% Try to get value from attributes in the module implementing the behaviours
get_argument(Module, Argument, Arguments) ->
    ModuleAttributes = Module:module_info(attributes),
    %% First try to get the information from the module attributes
    case lists:keysearch(Argument, 1, ModuleAttributes) of
	%% and, if not available in the module attributes, try to get the information from the start up arguments
	false -> 
	    get_argument(Argument, Arguments);
	{value, {Argument, Value}} ->
	    case Value of
		%% Extract tuples from the list
		[Tuple] when tuple(Tuple) -> Tuple;
		[Integer] when integer(Integer) -> Integer;
		Value -> Value
	    end
    end.

%% return undefined or the value of the argument tuple ({Name, Value})
%% from the argument list
get_argument(Argument, Arguments) ->
    case lists:keysearch(Argument, 1, Arguments) of
	false -> undefined;
	{value, {Argument, Value}} ->
	    Value
    end.
	    
