% Libraries
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_cors)).
:- use_module(library(date)).
:- use_module(library(random)).

% JSON Libraries
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

% Primary knowledge base



% Secundary knowledge base
:- dynamic node/3.
:- dynamic connection/4.
:- dynamic shortest_currentRoute/2.
:- dynamic strongest_currentRoute/2.
:- dynamic safest_currentRoute/2.


% HTTP Server setup at 'Port'
startServer(Port) :-
        http_server(http_dispatch, [port(Port)]),
        asserta(port(Port)).

% Cors setup
:- set_setting(http:cors, [*]).

% Server startup
start_server:-
    consult('g4s-ai-http-config-cloud'),  % Loads server's configuration
    server_port(Port),
    startServer(Port).

% Shutdown server
stopServer:-
        retract(port(Port)),
        http_stop_server(Port,_).

% Server init
:- start_server.

%================== Common Use ======================%

addPlayers() :-
	getPlayers(Data),
    parse_players(Data).

addConnections() :-
	getConnections(Data),
	parse_connections(Data).

getPlayers(Data) :-
    players_url(URL),
    setup_call_cleanup(
        http_open(URL, In, [ cert_verify_hook(cert_accept_any)]),
        json_read_dict(In, Data),
        close(In)
	).

getConnections(Data) :-
	connections_url(URL),
	setup_call_cleanup(
        http_open(URL, In, [ cert_verify_hook(cert_accept_any)]),
        json_read_dict(In, Data),
        close(In)
	).


getPlayerName(Email, PlayerName) :-
	atom_concat('email/',Email,Urlpath),
	players_url(URL),
	atom_concat(URL, Urlpath, UrlFinal),
    http_open(UrlFinal, Stream, [cert_verify_hook(cert_accept_any)]),
    json_read_dict(Stream, Data),
	PlayerName = Data.name.

parse_players([]).
parse_players([H|List]):-
    asserta(node(H.get(id),H.get(name),H.get(tags))),
    parse_players(List).

parse_connections([]):-
	prepareConnections().

parse_connections([H|List]):-
    asserta(connectionTemp(H.get(player),H.get(friend),H.get(connectionStrength))),
    parse_connections(List).

prepareConnections() :-
	forall(connectionTemp(A, B, C),(
		connectionTemp(B, A, D),
		asserta(connection(A, B, C, D)))),
	retractall(connectionTemp(_,_,_)).

%======== Shortest route between two players (HTTP) ========%

:- http_handler('/api/shortest-route', shortest_compute, []).

shortest_compute(Request) :-
	cors_enable(Request, [methods([get])]),
    shortest_prepare(Request, Path),
	prolog_to_json(Path, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

shortest_prepare(Request, Path) :-
    http_parameters(Request, [emailPlayer(EmailPlayer, [string]), emailTarget(EmailTarget, [string])]),
	addPlayers(),
	addConnections(),
	getPlayerName(EmailPlayer, PlayerName),
	getPlayerName(EmailTarget, TargetName),
	shortest_route(PlayerName, TargetName, Threshold, Path),
	retractall(connection(_,_,_,_)),
	retractall(node(_,_,_)).


%======== Shortest route between two players (Core) ========%

shortest_allDfs(Player1, Player2, PathList):- get_time(T1),
    findall(Path, shortest_dfs(Player1, Player2, Path), PathList),
    length(PathList, PathLength),
    get_time(T2),
    write(PathLength),write(' paths found in '),
    T is T2-T1,write(T),write(' seconds'),nl,
    write('Possible Path List: '),write(PathList),nl,nl.

shortest_dfs(Orig, Dest, Path):- shortest_dfsAux(Orig, Dest, [Orig], Path).

shortest_dfsAux(Dest, Dest, LA, Path):- !, reverse(LA, Path).
shortest_dfsAux(Current, Dest, LA, Path):-
    node(CurrentID, Current,_), (connection(CurrentID, NX, _, _); connection(NX, CurrentID, _, _)),
    node(NX,X,_),\+ member(X,LA), shortest_dfsAux(X,Dest,[X|LA],Path).


shortest_route(Orig, Dest, ShortestPathList):-
		get_time(Ti),
		(shortest_findRoute(Orig, Dest); true),
		retract(shortest_currentRoute(ShortestPathList, _)),
		get_time(Tf),
		T is Tf-Ti,
		write('Solution generation time:'), write(T), nl.

shortest_findRoute(Orig, Dest):-
		asserta(shortest_currentRoute(_,10000)),
		shortest_dfs(Orig, Dest, PathList),
		shortest_updateRoute(PathList),
		fail.

shortest_updateRoute(PathList):-
		shortest_currentRoute(_, CurrentPathLength),
		length(PathList, PathLength),
    	PathLength < CurrentPathLength, retract(shortest_currentRoute(_,_)),
		asserta(shortest_currentRoute(PathList, PathLength)).

% aux methods

intersect([ ],_,[ ]).
intersect([X|L1],L2,[X|LI]):-member(X,L2),!,intersect(L1,L2,LI).
intersect([_|L1],L2, LI):- intersect(L1,L2,LI).

%======== Safest path between two players (HTTP) ========%

:- http_handler('/api/safest-route', safest_routeCompute, []).

safest_routeCompute(Request) :-
	cors_enable(Request, [methods([get])]),
    safest_routePrepare(Request, Path),
	prolog_to_json(Path, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

safest_routePrepare(Request, Path) :-
    http_parameters(Request, [emailPlayer(EmailPlayer, [string]), emailTarget(EmailTarget, [string]), threshold(Threshold, [integer])]),
	addPlayers(),
	addConnections(),
	getPlayerName(EmailPlayer, PlayerName),
	getPlayerName(EmailTarget, TargetName),
	safest_route(PlayerName, TargetName, Threshold, Path),
	retractall(connection(_,_,_,_)),
	retractall(node(_,_,_)).


%======== Safest path between two players (Core) ========%

safest_dfs(Orig, Dest, Threshold, Strength, Path):- safest_dfsAux(Orig, Dest, [Orig], Threshold, 0, Strength, Path).

safest_dfsAux(Dest, Dest, AuxList, _ , Strength, Strength, Path):-!, reverse(AuxList,Path).
safest_dfsAux(Current, Dest, AuxList, Threshold, Strength, ReturnStrength, Path):-
		node(CurrentID,Current,_),
		(connection(CurrentID, FriendID, StrengthA, StrengthB);
		connection(FriendID, CurrentID, StrengthA, StrengthB)),
		node(FriendID, Friend, _),
		\+ member(Friend, AuxList),
		StrengthA >= Threshold,
		StrengthB >= Threshold,
		CurrentStrength is Strength + StrengthA + StrengthB,
		safest_dfsAux(Friend, Dest, [Friend | AuxList], Threshold, CurrentStrength, ReturnStrength, Path).


safest_route(Orig, Dest, Threshold, SafestPath):-
		get_time(Initial_Time),
		(safest_findRoute(Orig,Dest, Threshold);true),
		retract(safest_currentRoute(SafestPath,Strength)),
		((Strength >= 0, !, get_time(End_Time),
		T is End_Time - Initial_Time,
		write('Time:'),write(T),nl,
		write('Strength: '), write(Strength),nl);
		write('No path found.'),false).

safest_findRoute(Orig, Dest, Threshold):-
		asserta(safest_currentRoute(_,-10000)),
		safest_dfs(Orig, Dest, Threshold, Strength, PathList),
		safest_updateRoute(Strength, PathList),
		fail.

safest_updateRoute(Strength, PathList):-
		safest_currentRoute(_,Current_Strength),
		Strength > Current_Strength, retract(safest_currentRoute(_,_)),
		asserta(safest_currentRoute(PathList,Strength)).


% player suggestion

:-dynamic suggest_currentPath/2.

suggest_players(Player, Level, SuggestedPlayersList):-
		network_getNetworkByLevel(Player, Level, NetworkList),
		suggest_removeFriends(Player, NetworkList, CandidateList),
		suggest_getRelatedPlayers(Player, CandidateList, RelatedPlayersList),
		suggest_checkSuggestedPaths(Player, RelatedPlayersList, SuggestedPlayersList).

network_getNetworkByLevel(_, _, [antonio, beatriz, carlos, eduardo, isabel, jose]):-!.

suggest_removeFriends(_, [ ], []).
suggest_removeFriends(Player, [CurrentPlayer | NetworkList], CandidateList):-
		node(CurrentID, CurrentPlayer, _),
		node(PlayerID,Player,_),
		(connection(PlayerID, CurrentID, _, _);
		connection(CurrentID, PlayerID, _, _)), !,
		suggest_removeFriends(Player, NetworkList, CandidateList).
suggest_removeFriends(Player, [CurrentPlayer | NetworkList], [CurrentPlayer | CandidateList]):-
		suggest_removeFriends(Player, NetworkList, CandidateList).

suggest_getRelatedPlayers(_, [ ], []).
suggest_getRelatedPlayers(Player, [NetworkPlayer | Network], [ NetworkPlayer | RelatedPlayersList]):-
		node(_,Player,PlayerTagList),
		node(_, NetworkPlayer,NetworkPlayerTagList),
		intersect(PlayerTagList, NetworkPlayerTagList, CommonTagList),
		CommonTagList=[_|_], !,
		suggest_getRelatedPlayers(Player, Network, RelatedPlayersList).
suggest_getRelatedPlayers(Player, [_ | Network], RelatedPlayersList):-
		suggest_getRelatedPlayers(Player, Network, RelatedPlayersList).


suggest_checkSuggestedPaths(_, [], []).
suggest_checkSuggestedPaths(Player, [CurrentPlayer | RelatedPlayersList], [CurrentPlayer | SuggestedPlayersList]):-
		node(_, CurrentPlayer, CurrentPlayerTagList),
		node(_, Player, PlayerTagList),
		intersect(PlayerTagList, CurrentPlayerTagList, CommonTagList),
		suggest_findPathByPlayer(Player, CurrentPlayer, CommonTagList, Paths),
		Paths=[_|_], !,
		suggest_checkSuggestedPaths(Player, RelatedPlayersList, SuggestedPlayersList).
suggest_checkSuggestedPaths(Player, [_ | RelatedPlayersList], SuggestedPlayersList):-
		suggest_checkSuggestedPaths(Player, RelatedPlayersList, SuggestedPlayersList).


suggest_findPathByPlayer(_, _, [], []).
suggest_findPathByPlayer(Player, SuggestedPlayer, [CurrentTag | CommonTagList], [SuggestedPath | Paths]):-
		(suggest_findPathByTag(Player, SuggestedPlayer, CurrentTag);true),
		retract(suggest_currentPath(SuggestedPath, SuggestedPathLength)),
		\+ SuggestedPathLength = 10000,!,
		suggest_findPathByPlayer(Player, SuggestedPlayer, CommonTagList, Paths).
suggest_findPathByPlayer(Player, SuggestedPlayer, [_ | CommonTagList], Paths):-
		suggest_findPathByPlayer(Player, SuggestedPlayer, CommonTagList, Paths).


suggest_findPathByTag(_, _, []).
suggest_findPathByTag(Player, SuggestedPlayer, Tag):-
		asserta(suggest_currentPath(_, 10000)),
		suggest_dfs(Player, SuggestedPlayer, Tag, PathList),
		suggest_updateRoute(PathList),
		fail.

suggest_updateRoute(PathList):-
		suggest_currentPath(_, CurrentLength),
		length(PathList, PathListLength),
		PathListLength < CurrentLength,
		retract(suggest_currentPath(_,_)),
		asserta(suggest_currentPath(PathList, PathListLength)).

suggest_dfs(Orig,Dest, Tag, Path):-suggest_dfsAux(Orig,Dest,[Orig], Tag, Path).

suggest_dfsAux(Dest,Dest,AuxList, _, Path):-!,reverse(AuxList,Path).
suggest_dfsAux(Current,Dest,AuxList, Tag, Path):-
		node(CurrentID, Current, _),
		(connection(CurrentID, FriendID, _, _);
		connection(FriendID, CurrentID, _, _)),
		node(FriendID, Friend, FriendTagList),
		\+ member(Friend, AuxList),
		member(Tag, FriendTagList),
		suggest_dfsAux(Friend, Dest, [Friend | AuxList], Tag, Path).

%======== Strongest path between two players ========%

:- http_handler('/api/strongest-route', strongest_routeCompute, []).

strongest_routeCompute(Request) :-
	cors_enable(Request, [methods([get])]),
    strongest_routePrepare(Request, Path),
	prolog_to_json(Path, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

strongest_routePrepare(Request, Path) :-
    http_parameters(Request, [emailPlayer(EmailPlayer, [string]), emailTarget(EmailTarget, [string])]),
	addPlayers(),
	addConnections(),
	getPlayerName(EmailPlayer, PlayerName),
	getPlayerName(EmailTarget, TargetName),
	strongest_route(PlayerName, TargetName, Path),
	retractall(connection(_,_,_,_)),
	retractall(node(_,_,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strongest_dfs(Orig, Dest, Strength, Path):- strongest_dfsAux(Orig, Dest, [Orig], Strength, Path).

strongest_dfsAux(Dest, Dest, AuxList, 0, Path):-!,reverse(AuxList, Path).
strongest_dfsAux(Current, Dest, AuxList, Strength, Path):-
	node(CurrentID, Current, _),
	(connection(CurrentID, FriendID, StrengthA, StrengthB);
	connection(FriendID, CurrentID, StrengthA, StrengthB)),
	node(FriendID, Friend, _),
	\+ member(Friend, AuxList),
	strongest_dfsAux(Friend, Dest, [Friend | AuxList], SX, Path),
	Strength is (SX + StrengthA + StrengthB).

strongest_route(Orig, Dest, StrongestPath):-
    (strongest_findRoute(Orig, Dest); true),
    retract(strongest_currentRoute(StrongestPath, _)).

strongest_findRoute(Orig, Dest):-
    asserta(strongest_currentRoute(_, 0)),
    strongest_dfs(Orig, Dest, Strength, PathList),
    strongest_updateRoute(Strength, PathList),
    fail.

strongest_updateRoute(Strength, PathList):-
    strongest_currentRoute(_, Current_Strength),
    Strength > Current_Strength, retract(strongest_currentRoute(_,_)),
    asserta(strongest_currentRoute(PathList, Strength)).


%====================================================%
