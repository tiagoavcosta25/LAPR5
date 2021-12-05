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
:- dynamic safest_currentRoute/2.
:- dynamic strongest_currentRoute/2.
:- dynamic suggest_currentRoute/2.
:- dynamic common_tags_users/2.


% HTTP Server setup at 'Port'
startServer(Port) :-
        http_server(http_dispatch, [port(Port)]),
        asserta(port(Port)).

% Cors setup
:- set_setting(http:cors, [*]).

% Server startup
start_server:-
    consult('g4s-ai-http-config-local'),  % Loads server's configuration
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

%======== Auxiliary Methods ========%

intersect([ ],_,[ ]).
intersect([X|L1],L2,[X|LI]):-member(X,L2),!,intersect(L1,L2,LI).
intersect([_|L1],L2, LI):- intersect(L1,L2,LI).

%======== Shortest route between two players (HTTP) ========%
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
    node(PlayerId, PlayerName, _),
    node(TargetId, TargetName, _),
shortest_route(PlayerId, TargetId, Path),
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
    (connection(Current, X, _, _);
    connection(X, Current, _, _)),
    \+ member(X,LA),
    shortest_dfsAux(X,Dest,[X|LA],Path).


shortest_route(Orig, Dest, ShortestPathList):-
		(shortest_findRoute(Orig, Dest); true),
		retract(shortest_currentRoute(ShortestPathList, _)).

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
        node(PlayerId, PlayerName, _),
        node(TargetId, TargetName, _),
	safest_route(PlayerId, TargetId, Threshold, Path),
	retractall(connection(_,_,_,_)),
	retractall(node(_,_,_)).


%======== Safest path between two players (Core) ========%

safest_dfs(Orig, Dest, Threshold, Strength, Path):- safest_dfsAux(Orig, Dest, [Orig], Threshold, 0, Strength, Path).

safest_dfsAux(Dest, Dest, AuxList, _ , Strength, Strength, Path):-!, reverse(AuxList,Path).
safest_dfsAux(Current, Dest, AuxList, Threshold, Strength, ReturnStrength, Path):-
		(connection(Current, Friend, StrengthA, StrengthB);
		connection(Friend, Current, StrengthA, StrengthB)),
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


%======== Suggest Players (HTTP) ========%

:- http_handler('/api/suggest-players', suggest_compute, []).

suggest_compute(Request) :-
    cors_enable(Request, [methods([get])]),
    suggest_prepare(Request, SuggestedPlayersList),
    prolog_to_json(SuggestedPlayersList, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

suggest_prepare(Request, SuggestedPlayersList) :-
    http_parameters(Request, [emailPlayer(EmailPlayer, [string]), scope(Scope, [string])]),
    addPlayers(),
    addConnections(),
    getPlayerName(EmailPlayer, PlayerName),
    node(PlayerId, PlayerName, _),
    suggest_players(PlayerId, Scope, SuggestedPlayersList),
    retractall(connection(_,_,_,_)),
    retractall(node(_,_,_)).


%======== Suggest Players (Core) ========%

suggest_players(Player, Level, SuggestedPlayersList):-
		network_getNetworkByLevel(Player, Level, NetworkList, _),
		suggest_removeFriends(Player, NetworkList, CandidateList),
		suggest_getRelatedPlayers(Player, CandidateList, RelatedPlayersList),
		suggest_checkSuggestedPaths(Player, RelatedPlayersList, SuggestedPlayersList).

suggest_removeFriends(_, [ ], []).
suggest_removeFriends(Player, [CurrentPlayer | NetworkList], CandidateList):-
		(connection(Player, CurrentPlayer, _, _);
		connection(CurrentPlayer, Player, _, _)), !,
		suggest_removeFriends(Player, NetworkList, CandidateList).
suggest_removeFriends(Player, [CurrentPlayer | NetworkList], [CurrentPlayer | CandidateList]):-
		suggest_removeFriends(Player, NetworkList, CandidateList).

suggest_getRelatedPlayers(_, [ ], []).
suggest_getRelatedPlayers(Player, [NetworkPlayer | Network], [ NetworkPlayer | RelatedPlayersList]):-
		node(Player,_,PlayerTagList),
		node(NetworkPlayer, _,NetworkPlayerTagList),
		intersect(PlayerTagList, NetworkPlayerTagList, CommonTagList),
		CommonTagList=[_|_], !,
		suggest_getRelatedPlayers(Player, Network, RelatedPlayersList).
suggest_getRelatedPlayers(Player, [_ | Network], RelatedPlayersList):-
		suggest_getRelatedPlayers(Player, Network, RelatedPlayersList).


suggest_checkSuggestedPaths(_, [], []).
suggest_checkSuggestedPaths(Player, [CurrentPlayer | RelatedPlayersList], [CurrentPlayer | SuggestedPlayersList]):-
		node(CurrentPlayer, _, CurrentPlayerTagList),
		node(Player, _, PlayerTagList),
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
		node(Current, _, _),
		(connection(Current, Friend, _, _);
		connection(Friend, Current, _, _)),
		node(Friend, _, FriendTagList),
		\+ member(Friend, AuxList),
		member(Tag, FriendTagList),
		suggest_dfsAux(Friend, Dest, [Friend | AuxList], Tag, Path).

%======== Strongest route between two players ========%

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
	node(PlayerId, PlayerName, _),
	node(TargetId, TargetName, _),
	strongest_route(PlayerId, TargetId, Path),
	retractall(connection(_,_,_,_)),
	retractall(node(_,_,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strongest_dfs(OrigId, DestId, Strength, Path):- strongest_dfsAux(OrigId, DestId, [OrigId], Strength, Path).

strongest_dfsAux(DestId, DestId, AuxList, 0, Path):-!,reverse(AuxList, Path).
strongest_dfsAux(CurrentId, DestId, AuxList, Strength, Path):-
	(connection(CurrentId, FriendID, StrengthA, StrengthB);
	connection(FriendID, CurrentId, StrengthA, StrengthB)),
	\+ member(FriendID, AuxList),
	strongest_dfsAux(FriendID, DestId, [FriendID | AuxList], SX, Path),
	Strength is (SX + StrengthA + StrengthB).

strongest_route(OrigId, DestId, StrongestPath):-
    (strongest_findRoute(OrigId, DestId); true),
    retract(strongest_currentRoute(StrongestPath, _)).

strongest_findRoute(OrigId, DestId):-
    asserta(strongest_currentRoute(_, 0)),
    strongest_dfs(OrigId, DestId, Strength, PathList),
    strongest_updateRoute(Strength, PathList),
    fail.

strongest_updateRoute(Strength, PathList):-
    strongest_currentRoute(_, Current_Strength),
    Strength > Current_Strength, retract(strongest_currentRoute(_,_)),
    asserta(strongest_currentRoute(PathList, Strength)).



%======== Players with X common tags ========%

:- http_handler('/api/common-tags', common_tagsCompute, []).

common_tagsCompute(Request) :-
	cors_enable(Request, [methods([get])]),
    common_tagsPrepare(Request, Path),
	prolog_to_json(Path, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

common_tagsPrepare(Request, Path) :-
    http_parameters(Request, [num(Num, [number])]),
	addPlayers(),
	addConnections(),
	common_tags(Num, Path),
	retractall(connection(_,_,_,_)),
	retractall(node(_,_,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

common_tags(X,List_Result):-
    common_tags_get_all_tags(All_TagsT),
	common_tags_change_to_synonyms(All_TagsT, All_Tags),
    findall(Combination,common_tags_combination(X,All_Tags,Combination),Combinations),
    findall(UserId,node(UserId,_,_),Users),
    common_tags_users_combination(X,Users,Combinations),
    findall([Comb,ListUsers],common_tags_users(Comb,ListUsers),List_Result),
    retractall(common_tags_users(_,_)).

common_tags_users_combination(_,_,[]).
common_tags_users_combination(X,Users,[Combination|Combinations]):-
    common_tags_users_combination_aux(X,Combination,Users,Users_With_Tags),
    common_tags_users_combination(X,Users,Combinations),
    !,
	comon_tags_list_length(Users_With_Tags, L),
	( L > 1-> assertz(common_tags_users(Combination,Users_With_Tags)) ; ! ).

common_tags_users_combination_aux(_,_,[],[]):-!.
common_tags_users_combination_aux(X,Tags,[U|Users],Result):-
    node(U,_,User_TagsT),
	common_tags_change_to_synonyms(User_TagsT, User_Tags),
    intersection(Tags, User_Tags,Commun),
    length(Commun, Size),
    Size >= X, !,
    common_tags_users_combination_aux(X,Tags,Users,Result1),
    append([U], Result1, Result).
common_tags_users_combination_aux(X,Tags,[_|Users],Result):-
    !,
    common_tags_users_combination_aux(X,Tags,Users,Result).

common_tags_get_all_tags(Tags):-
    findall(User_Tags,node(_,_,User_Tags),All_Tags),
    common_tags_remove_repeated_tags(All_Tags,Tags).

common_tags_remove_repeated_tags([],[]).
common_tags_remove_repeated_tags([List|All_Tags],Tags):-
    common_tags_remove_repeated_tags(All_Tags,Tags1),!,
    union(List,Tags1,Tags).

%=== Combinaçoes ===
common_tags_combination(0,_,[]).
common_tags_combination(N,[X|T],[X|Comb]):-N>0,N1 is N-1,common_tags_combination(N1,T,Comb).
common_tags_combination(N,[_|T],Comb):-N>0,common_tags_combination(N,T,Comb).

comon_tags_list_length([], 0).
comon_tags_list_length([_|TAIL], N) :- comon_tags_list_length(TAIL, N1), N is N1 + 1.

common_tags_change_to_synonyms([],[]).
common_tags_change_to_synonyms([Tag|All_Tags],Tags):-
    common_tags_change_to_synonyms(All_Tags,Tags1),!,
	(synonym(Tag, Sign) ->
		union([Sign], Tags1, Tags);
		union([Tag], Tags1, Tags)).

		%======== Network By Level ========%

:- http_handler('/api/network/size', network_levelCompute, []).

network_levelCompute(Request) :-
    cors_enable(Request, [methods([get])]),
    network_levelPrepare(Request, Path, Size),
    prolog_to_json(Path, JSONObject),
    prolog_to_json(Size, JSONObject2),
    reply_json([JSONObject, JSONObject2], [json_object(dict)]).

network_levelPrepare(Request, Path, Size) :-
    http_parameters(Request, [email(Email, [string]) ,level(Level, [number])]),
    addPlayers(),
    addConnections(),
    getPlayerName(Email, PlayerName),
    node(PlayerId, PlayerName, _),
    network_getNetworkByLevel(PlayerId, Level, Path, Size),
    retractall(connection(_,_,_,_)),
    retractall(node(_,_,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-dynamic user_visited/1.

network_getNetworkByLevel(Orig,Level,L, Total):-
    retractall(userVisited(_)),
    findall(Orig,dfs(Orig,Level),_),
    findall(User,userVisited(User),L),
    retractall(userVisited(_)),
    length(L,Total).


dfs(Orig,Level):-
    dfs2(Orig,Level,[Orig]).


dfs2(_,0,_):-!.


dfs2(Act,Level,LA):-
    Level > 0,
    (connection(Act,X,_,_);connection(X,Act,_,_)),
    \+userVisited(X),
    \+ member(X,LA),
    Level1 is Level-1,
    asserta(userVisited(X)),
    dfs2(X,Level1,[X|LA]).
