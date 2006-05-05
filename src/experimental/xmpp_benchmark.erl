%%% File    : xmpp_benchmark.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Description : The purpose of the script is to initialise a realistic
%%%               Jabber user base, with a realistic roster distribution.
%%% 
%%%               This script can be used along with an XMPP server benchmark
%%%               tool such as Tsung that will take care of generating the
%%%               load.
%%%               
%%%               Limitations:
%%%               - Only creates local user in roster.
%%%  
%%% Created :  1 Nov 2004 by Mickael Remond <mickael.remond@process-one.net>
%%% Rewrite : 26 Dec 2005 by Mickael Remond <mickael.remond@process-one.net>
%%% Statistical roster distribution code first created by Nicolas Niclausse
%%%
-module(xmpp_benchmark).
-author("mickael.remond@process-one.net").

-export([create_db/1, create_db/2]).
-export([step1/1, step2/2]).
-export([connect_users/0]).
-export([populate_rosters/1]).

%% Internal export
-export([subscribe/3]).

-define(DEBUG, false).
-define(MAX_RANDOM, 10000000).

%% Statistics generation
-import(math, [log/1, pi/0, sqrt/1, pow/2]).
-record(invgaussian, {mu , lambda}).
-record(normal, {mean = 0 , stddev= 1 }).

%% Create a realistic XMPP user database with the given number of users
create_db(NumberOfUsers) ->
    create_db(xmpp, NumberOfUsers).

%% Possible methods are: 
%%   {xmpp, Server, Port}
%%   {odbc, DSN}
%%   {mnesia, Node}
%%   {mysql, Server, User, Password, Database}
create_db(Method = {mysql, _Server, _User, _Password, _Database}, NumberOfUsers) ->
    error_logger:tty(?DEBUG),
    create_users_tables(),
    random(),
    io:format("Creating users~n",[]),
    register_users(Method, NumberOfUsers),
    
    %% Step 1: Create users list with roster size
    io:format("Generating users's roster size~n",[]),
    roster_size2(NumberOfUsers),

    io:format("Generating rosters~n",[]),
    create_users_rosters2(Method);
create_db(Method, NumberOfUsers) ->
    error_logger:tty(?DEBUG),
    generate_users(NumberOfUsers),
    create_jabber_users(Method, NumberOfUsers).

%% Only launch users definition
step1(NumberOfUsers) ->
    error_logger:tty(?DEBUG),
    generate_users(NumberOfUsers).

%% Use existing user definition to generate the Jabber userbase.
step2(Method, NumberOfUsers) ->
    error_logger:tty(?DEBUG),
    {ok, RosterSizeTable} = ets:file2tab("/tmp/roster_size.ets"),
    {ok, RelationsTable} = ets:file2tab("/tmp/relations.ets"),
    create_jabber_users(Method, NumberOfUsers).

%% Init user db
%% This function takes approximatively 4 minutes for 30000 users
%% During this steps, nothing is done to the target Jabber/XMPP server
generate_users(NumberOfUsers) ->
    create_users_tables(),
    random(),
    
    %% Step 1: Create users list with roster size
    io:format("Generating users's roster size~n",[]),
    roster_size2(NumberOfUsers),

    %% Step 2: Create users relationships (Who is in who's roster)
    io:format("Creating users' rosters~n",[]),
    create_users_rosters2(),

    %% Save the result of the user generation
    ets:tab2file(roster_size, "/tmp/roster_size.ets"),
    ets:tab2file(relations, "/tmp/relations.ets").

%% During this steps, the previously generated Jabber users database is
%% inserted in the target Jabber server
create_jabber_users(Method, NumberOfUsers) ->
    %% Step 3: Create users in Jabber
    io:format("Creating users on the target XMPP server~n",[]),
    register_users(Method, NumberOfUsers),

    %% Step 4: Roster creation pass 1
    io:format("Creating users' roster entries~n",[]),
    populate_rosters(Method),

    case Method of 
	%% Step 5: Connect with all users id to answer subscrition and ask for
	%% symetric subscription.
	{xmpp, Server, Port} -> populate_rosters(Method);
	{odbc, DSN} -> ok;
	{mnesia, Node} -> ok;
	{mysql, Server, User, Password, Database} -> ok
    end.

create_users_tables() ->
    %% {{Random, Usernumber}, RosterSize, RemainingSpaceInRoster}
    ets:new(roster_size, [named_table, ordered_set, public]),
    %% {User1, User2}
    ets:new(relations, [named_table, bag, public]).

%% Step1: Define user names and initialize roster_size table based 
%% statistical roster distribution
roster_size(NumberOfUsers) ->
    RosterSizeList = lists:map(fun(A)->trunc(A+1)-1 end ,
		  invgaussian([14.921,2.229], NumberOfUsers + 1)),
    roster_size(NumberOfUsers, RosterSizeList).
roster_size(0, _) -> ok;
roster_size(_, []) -> ok;
roster_size(UserNumber, [UserRosterSize|RosterSizeList]) ->
    case UserRosterSize of
	0 -> ok;
	_ ->
	    Random = random:uniform(?MAX_RANDOM),
	    ets:insert(roster_size, {{Random, UserNumber}, UserRosterSize, UserRosterSize})
    end,
    roster_size(UserNumber - 1, RosterSizeList).

%% Step1: Define user names and initialize roster_size table based 
%% statistical roster distribution
roster_size2(0) -> ok;
roster_size2(UserNumber) ->
    Size = trunc(invgaussian(#invgaussian{mu=14.921, lambda=2.229})+1)-1,
    case Size of
	UserRosterSize when UserRosterSize > 1000 ->
	    %% We do not generate roster size bigger than 1000
	    %% We try generating a new size:
	    roster_size2(UserNumber);
	0 ->
	    %% We do not store user with 0 roster size in the roster size table:
	    roster_size2(UserNumber - 1);
	UserRosterSize ->
	    Random = random:uniform(?MAX_RANDOM),
	    ets:insert(roster_size, {{Random, UserNumber}, UserRosterSize, UserRosterSize}),
	    roster_size2(UserNumber - 1)
    end.

create_users_rosters2() ->
    case ets:first(roster_size) of
	'$end_of_table' ->
	    ok;
	Key ->
	    [{{_Random, User}, _UserRosterSize, RemainingItemToAdd}] = ets:lookup(roster_size, Key),
	    ets:delete(roster_size, Key),
	    create_roster2(User, RemainingItemToAdd),
	    create_users_rosters2()
    end.

create_users_rosters2(Method) ->
    case ets:first(roster_size) of
	'$end_of_table' ->
	    ok;
	Key ->
	    [{{_Random, User}, _UserRosterSize, RemainingItemToAdd}] = ets:lookup(roster_size, Key),
	    ets:delete(roster_size, Key),
	    create_roster2(Method, User, RemainingItemToAdd),
	    create_users_rosters2(Method)
    end.

create_roster2(User1, RemainingItemsToAdd) ->
    case ets:match_object(roster_size, '$1', RemainingItemsToAdd) of
	{User1Roster, _Continuation} ->
	    lists:foreach(fun(RosterEntry) ->
				  {{Random, User2}, UserRosterSize, RemainingItems} = RosterEntry,
				  ets:delete(roster_size, {Random, User2}),
				  ets:insert(relations, {User1,User2}),

				  NewRest = RemainingItems - 1,
				  case NewRest of
				      0 ->
					  %% Do not recreate the user2 entry: No more remaining items
					  ok;
				      _ ->
					  ets:insert(roster_size,
						     {{random:uniform(?MAX_RANDOM), User2}, UserRosterSize, NewRest})
				  end
			  end,
			  User1Roster);
	'$end_of_table' ->
	    ok
    end.

create_roster2({mysql, Server, User, Password, DB}, User1, RemainingItemsToAdd) ->
    {ok, Ref} = mysql_connect(Server, User, Password, DB),
    case ets:match_object(roster_size, '$1', RemainingItemsToAdd) of
	{User1Roster, _Continuation} ->
	    lists:foreach(fun(RosterEntry) ->
				  {{Random, User2}, UserRosterSize, RemainingItems} = RosterEntry,
				  ets:delete(roster_size, {Random, User2}),

		      Username1 = "user" ++ integer_to_list(User1),
		      Username2 = "user" ++ integer_to_list(User2),
		      mysql_subscribe(Username1, Username2, Server),
		      mysql_subscribe(Username2, Username1, Server),
				  
				  NewRest = RemainingItems - 1,
				  case NewRest of
				      0 ->
					  %% Do not recreate the user2 entry: No more remaining items
					  ok;
				      _ ->
					  ets:insert(roster_size,
						     {{random:uniform(?MAX_RANDOM), User2}, UserRosterSize, NewRest})
				  end
			  end,
			  User1Roster);
	'$end_of_table' ->
	    ok
    end.

create_users_rosters() ->
    case get_possible_users() of
	[User] ->
	    ok;
	[] ->
	    ok;
	[User|Users] ->
	    create_roster(User, Users),
	    create_users_rosters()
    end.

%%get users with remaining space in roster
get_possible_users() ->
    %% Generated with: ets:fun2ms(fun({User,_, RemainingUsers}) when RemainingUsers > 0 -> User end)
    MatchSpec = [{{'$1','_','$2'},[{'>','$2',0}],['$1']}],
    ets:select(roster_size, MatchSpec).

%% Create roster table entries for a given user
create_roster(User, Users) ->
    %% Get remaining space in roster
    [{User, _RosterSize, RemainingRosterSize}] = ets:lookup(roster_size, User),

    %% Remove User already in roster
    AlreadyInRosterOfUsers = ets:match(relations, {'$1', User}),
    PossibleUsers = lists:subtract(Users, AlreadyInRosterOfUsers),
    create_roster(User, PossibleUsers, RemainingRosterSize).
create_roster(User, Users, 0) ->
    ok;
create_roster(User, Users, RemainingRosterSize) ->
    case length(Users) of
	0 ->
	    %% This can happen for the last users, when no more users are
	    %% available for roster addition.
	    [{User, RosterSize, _}] = ets:lookup(roster_size, User),
	    ets:insert(roster_size, {User, RosterSize, 0});
	NumberRemainingUsers ->
	    RandomUserNumber = random:uniform(NumberRemainingUsers),
	    RandomUser = lists:nth(RandomUserNumber, Users),
	    RemainingUsers = lists:delete(RandomUser, Users),

	    %% io:format("Creating relation {~p,~p}~n",[User,RandomUser]),
	    ets:insert(relations, {User,RandomUser}),
	    ets:insert(relations, {RandomUser,User}),
	    update_roster_size(User),    
	    update_roster_size(RandomUser),
	    create_roster(User, RemainingUsers, RemainingRosterSize-1)
    end.

update_roster_size(User) ->
    [{User, RosterSize, RemainingRosterSpace}] =
	ets:lookup(roster_size, User),
    %%io:format("~p remaining roster item(s) for user ~p~n", [RemainingRosterSpace-1, User]),
    ets:insert(roster_size, {User, RosterSize, RemainingRosterSpace-1}).

%% Step 3: Register all users in the Jabber Server
%% In the XMPP module, users created automatically if they do not exists.
register_users({xmpp, Server, _Port}, NumberofUsers) ->
    %% TODO: Use Records
    %% TODO: Use parameter to change host
    {ok, XMPP} = xmpp:start(Server),    
    %% TODO: Parameter for admin login and password:
    xmpp:set_login_information(XMPP, "admin", {password,"admin"}, "XMPPBenchmark"),
    xmpp:connect(XMPP),
    xmpp_register_users(XMPP, NumberofUsers),
    xmpp:stop(XMPP);
register_users({odbc, DSN}, NumberofUsers) ->
    {ok, Ref} = odbc:connect(DSN, [{scrollable_cursors, off}]),
    odbc_register_users(Ref, NumberofUsers),
    odbc:disconnect(Ref);
register_users({mysql, Server, User, Password, DB}, NumberofUsers) ->
    {ok, Ref} = mysql_connect(Server, User, Password, DB),
    mysql_register_users(?MODULE, NumberofUsers);
register_users({mnesia, Node}, NumberofUsers) ->
    mnesia_register_users(Node, NumberofUsers).

%% Performs the actual XMPP registration:
xmpp_register_users(XMPP, 0) ->
    ok;
xmpp_register_users(XMPP, Usernumber) ->
    User = "user"++integer_to_list(Usernumber),
    xmpp:register_user(XMPP, User, {password,User}),
    xmpp_register_users(XMPP, Usernumber-1).

%% Performs the actual ODBC registration:
odbc_register_users(Ref, 0) ->
    ok;
odbc_register_users(Ref, Usernumber) ->
    User = "user"++integer_to_list(Usernumber),
    odbc:sql_query(Ref, "insert into users(username, password) values('" ++ User ++ "', '" ++ User ++ "');"),
    odbc_register_users(Ref, Usernumber-1).

%% Performs the actual MySQL registration:
mysql_register_users(Ref, 0) ->
    ok;
mysql_register_users(Ref, Usernumber) ->
    User = "user"++integer_to_list(Usernumber),
    mysql:fetch(Ref, "insert into users(username, password) values('" ++ User ++ "', '" ++ User ++ "');"),
    mysql_register_users(Ref, Usernumber-1).

%% Performs the actual Mnesia registration:
mnesia_register_users(Node, 0) ->
    ok;
mnesia_register_users(Node, Usernumber) ->
    User = "user"++integer_to_list(Usernumber),
    %% #passwd{us={"User","Host"}, password="Password"}
    Record = {passwd, {User, "localhost"}, User},
    rpc:call(Node, mnesia, dirty_write, [Record]),
    mnesia_register_users(Node, Usernumber - 1).

%% Connect and if the user does not exist, register it:
connect_user(Host, User) ->
    {ok, XMPP} = xmpp:start("localhost"),
    xmpp:set_login_information(XMPP, User,{password,User},"XMPPBenchmark"),
    xmpp:connect(XMPP),
    xmpp:stop(XMPP).

%% Step 4: Roster creation pass 1
%% In the XMPP module, users created automatically if they do not exists.
%% TODO: I should connect once for every user and only send new
%% registration data. This should be optimized.
populate_rosters({xmpp, _Server, _Port}) ->
    error_logger:tty(?DEBUG),
    SubscribePid = spawn(?MODULE, subscribe, ["localhost", [], []]), 
    %% TODO: Use Records
    %% TODO: Use parameter to change host
    ets:foldl(fun({User1, User2}, Acc) ->
		      Username1 = "user" ++ integer_to_list(User1),
		      Username2 = "user" ++ integer_to_list(User2),
		      %% io:format("~p: Adding ~p to my roster.~n", [Username1, Username2]),
 		      create_user_roster(SubscribePid, Username1, Username2),
		      %% TODO: This is not good as I force to disconnect, reconnect for each roster entry:
 		      create_user_roster(SubscribePid, Username2, Username1),
 		      Acc
 	      end,
 	      [], relations),
    SubscribePid ! {stop};
populate_rosters({odbc, DSN}) ->
    error_logger:tty(?DEBUG),    
    {ok, Ref} = odbc:connect(DSN, [{scrollable_cursors, off}]),
    ets:foldl(fun({User1, User2}, Acc) ->
		      Username1 = "user" ++ integer_to_list(User1),
		      Username2 = "user" ++ integer_to_list(User2),
		      odbc_subscribe(Ref, Username1, Username2, "localhost"),
		      odbc_subscribe(Ref, Username2, Username1, "localhost"),
 		      Acc
 	      end,
 	      [], relations),
    odbc:disconnect(Ref);
populate_rosters({mysql, Server, User, Password, DB}) ->
    error_logger:tty(?DEBUG),    
    {ok, Ref} = mysql_connect(Server, User, Password, DB),
    ets:foldl(fun({User1, User2}, Acc) ->
		      Username1 = "user" ++ integer_to_list(User1),
		      Username2 = "user" ++ integer_to_list(User2),
		      mysql_subscribe(Username1, Username2, Server),
		      mysql_subscribe(Username2, Username1, Server),
 		      Acc
 	      end,
 	      [], relations);
populate_rosters({mnesia, Node}) ->
    error_logger:tty(?DEBUG),    
    ets:foldl(fun({User1, User2}, Acc) ->
		      Username1 = "user" ++ integer_to_list(User1),
		      Username2 = "user" ++ integer_to_list(User2),
		      mnesia_subscribe(Node, Username1, Username2, "localhost"),
		      mnesia_subscribe(Node, Username2, Username1, "localhost"),
 		      Acc
 	      end,
 	      [], relations).

%% Create a roster entry via ODBC
odbc_subscribe(ODBCRef, Username1, Username2, Host) ->
    User2 = Username2++"@"++Host,
    Query = io_lib:format("insert into rosterusers(username, jid, nick, subscription, ask, server, subscribe, type) values('~s', '~s', '', 'B', 'N', 'N', '', 'item');", [Username1, User2]),
    odbc:sql_query(ODBCRef, Query).

%% Create a roster entry via native MySQL interface
mysql_subscribe(Username1, Username2, Host) ->
    User2 = Username2++"@"++Host,
    Query = io_lib:format("insert into rosterusers(username, jid, nick, subscription, ask, server, subscribe, type) values('~s', '~s', '', 'B', 'N', 'N', '', 'item');", [Username1, User2]),
    mysql:fetch(?MODULE, Query).

%% Create a roster entry via mnesia
mnesia_subscribe(Node, Username1, Username2, Host) ->
    %% #roster{usj={"User","Host",{"User2","Host",[]}},
    %%         us={"User","Host"},
    %%         jid={"User2","Host", []},
    %%         name = "User2",
    %%         subscription = both|to|from|none,
    %%         ask = none,
    %%         groups = ["Group1", "Group2"],xattrs = [],xs = []}
    Record = {roster, {Username1, Host,{Username2,Host,[]}},
	      {Username1,Host},
	      {Username2,Host, []},
	      Username2,
	      both, none, [], [], []},
    Result = rpc:call(Node, mnesia, dirty_write, [Record]).

create_user_roster(SubscribePid, User1, User2) ->
    SubscribePid ! {subscribe, self(), User1, User2},
    %% Wait for result
    receive
	Result -> ok
    end.

%% Step 5: Connect with all users id to answer subscrition and ask for
%% symetric subscription.
connect_users() ->
    %% TODO: Use Records
    %% TODO: Use parameter to change host
    ets:foldl(fun({Usernumber, _RosterSize, _RemainingSpaceInRoster}, Acc) ->
		      User = "user"++integer_to_list(Usernumber),
		      connect_user("localhost", User),
		      io:format("Session: ~p~n", [User]),
		      Acc
	      end,
	      [], roster_size).

%% Create_user_roster
%% This is a function process whose main purpose is to manage XMPP connection.
%% If two subsequent subscriptions are made from the same user, the
%% XMPP connection does not need to be resetted.
subscribe(Host, ExistingXMPP, ConnectedUser) ->
    receive
	{subscribe, CallerPid, ConnectedUser, User2} ->
	    Result = xmpp:subscribe(ExistingXMPP, User2++"@"++Host),
	    CallerPid ! Result,
	    subscribe(Host, ExistingXMPP, ConnectedUser);
	{subscribe, CallerPid, User1, User2} ->
	    case ExistingXMPP of
		[]                 -> ok;
		ExistingConnection ->
		    xmpp:stop(ExistingXMPP)
	    end,
	    {ok, XMPP} = xmpp:start("localhost"),
	    xmpp:set_login_information(XMPP, User1,{password,User1},"XMPPBenchmark"),
	    xmpp:connect(XMPP),
	    Result = xmpp:subscribe(XMPP, User2++"@"++Host),
	    CallerPid ! Result,
	    subscribe(Host, XMPP, User1);
	{stop} ->
	    case ExistingXMPP of
		[]                 -> ok;
		ExistingConnection ->
		    xmpp:stop(ExistingXMPP)
	    end
    end.

%%% =================
%%% Specific DB functions
%%% =================
mysql_connect(Server, User, Password, DB) ->
    LogFun = fun(_Level, _Format, _Arguments) -> ok end,
    case mysql:start_link(?MODULE, Server, User, Password, DB, LogFun) of
	{ok, Ref} ->
	    {ok, Ref};
	{error,{already_started, Pid}}  ->
	    {ok, Pid};
	{error, Reason} ->
	    {error, Reason}
    end.


%%% =================
%%% Statistical code
%%% =================
 
%% Random seed initialization.
random() ->
    {A,B,C}=erlang:now(),
    random:seed(A,B,C).

%% The invgaussian code comes from Tsung benchmark tool:
invgaussian([Mu,Lambda],N) ->
    invgaussian(#invgaussian{mu=Mu,lambda=Lambda},N);
invgaussian(Param,N) ->
    sample(fun(X) -> invgaussian(X) end , Param, N).
    
%% random sample from a Inverse Gaussian distribution
invgaussian(#invgaussian{mu=Mu, lambda=Lambda}) ->
    Y = Mu*pow(normal(), 2),
    X1 = Mu+Mu*Y/(2*Lambda)-Mu*sqrt(4*Lambda*Y+pow(Y,2))/(2*Lambda),
    U = random:uniform(),
    X = (Mu/(Mu+X1))-U,
    case X >=0 of 
        true  -> X1;
        false -> Mu*Mu/X1
    end.

%% get n samples from a function F with parameter Param
sample(F, Param, N)    -> sample(F, [], Param, N-1).
sample(F, X, Param, 0) -> [F(Param) | X] ;
sample(F, X, Param, N) -> sample(F, [F(Param)|X], Param, N-1 ).

normal() ->
    [Val] = normal(#normal{},1),
    Val.

normal([Mean,StdDev],N) ->
    normal(#normal{mean=Mean,stddev=StdDev},N);
normal(Param,N) ->
    sample(fun(X) -> normal(X) end , Param, N).
    
normal(N) when integer(N)->
    normal(#normal{},N);
normal(#normal{mean=M,stddev=S}) ->
    normal_boxm(M,S,0,0,1).

%%% use the polar form of the Box-Muller transformation
normal_boxm(M,S,X1,X2,W) when W < 1->
    W2 = sqrt( (-2.0 * log( W ) ) / W ),
    Y1 = X1 * W2,
    M + Y1 * S;
normal_boxm(M,S,_,_,_W) ->
    X1 = 2.0 * random:uniform() - 1.0,
    X2 = 2.0 * random:uniform() - 1.0,
    normal_boxm(M,S,X1,X2,X1 * X1 + X2 * X2).
