% Libraries
:- use_module(library(http/http_ssl_plugin)).
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
:- dynamic connectionTemp/3.
:- dynamic connection/6.
:- dynamic occ/7.
:- dynamic shortest_currentRoute/3.
:- dynamic safest_currentRoute/2.
:- dynamic strongest_currentRoute/2.
:- dynamic suggest_currentRoute/2.
:- dynamic common_tags_users/3.
:- dynamic aStar_orderedList/1.
:- dynamic occ/7.
:- dynamic fear/2.
:- dynamic hope/2.


% HTTP Server setup at 'Port'
startServer(Port) :-
		http_server(http_dispatch,
                    [ port(Port),
                      ssl([ certificate_file('C:/Users/Administrador/Desktop/g4s_ai/g4s-artificial-intelligence-module/certificate.crt'),
                            key_file('C:/Users/Administrador/Desktop/g4s_ai/g4s-artificial-intelligence-module/private.key')
                          ])
                    ]),
        asserta(port(Port)).




% Cors setup
:- set_setting(http:cors, [*]).

% Server startup
start_server:-
    consult('g4s-ai-http-config-cloud'),  % Loads servers configuration
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

getDCalc(IdA, IdB, Data) :-
    dcalc_url(BaseURL),
    atom_concat(BaseURL, IdA, URLIdA),
    atom_concat(URLIdA, '/', URLIdBReady),
    atom_concat(URLIdBReady, IdB, URL),
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
    asserta(occ(H.get(id), 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)),
    parse_players(List).

parse_connections([]):-
	prepareConnections().

parse_connections([H|List]):-
    asserta(connectionTemp(H.get(player),H.get(friend),H.get(connectionStrength))),
    parse_connections(List).

prepareConnections() :-
	forall(connectionTemp(A, B, C),(
		connectionTemp(B, A, D),
		getDCalc(A, B, DABTemp),
		getDCalc(B, A, DBATemp),
		sigmoid(DABTemp.get(dCalc), DAB),
		sigmoid(DBATemp.get(dCalc), DBA),
		asserta(connection(A, B, C, D, DAB, DBA)))),
	retractall(connectionTemp(_,_,_)).

%======== Auxiliary Methods ========%

intersect([ ],_,[ ]).
intersect([X|L1],L2,[X|LI]):-member(X,L2),!,intersect(L1,L2,LI).
intersect([_|L1],L2, LI):- intersect(L1,L2,LI).

listUnion([ ],L,L).
listUnion([X|L1],L2,LU):-
    member(X,L2),
    listUnion(L1,L2,LU).
listUnion([X|L1],L2,[X|LU]):-
    listUnion(L1,L2,LU).

sigmoid(X, ReturnValue):-
    ReturnValue is (400/(1 + (2.71828 ** (-0.02 * X)))) - 200.

setRelValues([], []):-!.
setRelValues([H|T], [HFinal|Valores]):-
	((H > 200, !, HFinal is 100);
	(H < -200, !, HFinal is 0);
	(!, HFinal is (H + 200) / 4)),
        setRelValues(T, Valores).

getMulticriteria(ConnectionStrength, RelationStrength, Output):-
    MultiConnection is ConnectionStrength / 2,
    MultiRelation is ((RelationStrength + 200) / 4) / 2,
    Output is MultiConnection + MultiRelation.


%======== Shortest route between two players (HTTP) ========%

:- http_handler('/api/shortest-route', shortest_compute, []).

shortest_compute(Request) :-
cors_enable(Request, [methods([get])]),
	shortest_prepare(Request, Strength, Path),
	prolog_to_json(Path, JSONObject),
	prolog_to_json(Strength, JSONObject2),
	reply_json([JSONObject, JSONObject2], [json_object(dict)]).

shortest_prepare(Request, Strength, Path) :-
http_parameters(Request, [emailPlayer(EmailPlayer, [string]), emailTarget(EmailTarget, [string]),mode(Mode, [integer]) , n(N, [integer])]),
addPlayers(),
addConnections(),
getPlayerName(EmailPlayer, PlayerName),
getPlayerName(EmailTarget, TargetName),
    node(PlayerId, PlayerName, _),
    node(TargetId, TargetName, _),
shortest_route(Mode, 0, N, PlayerId, TargetId, Strength, Path),
retractall(connection(_,_,_,_,_,_)),
retractall(node(_,_,_)).

% ======== Shortest route w/ emotion (HTTP) % ========%

:- http_handler('/api/shortest-route-emotions', shortest_computeEmotion, []).

shortest_computeEmotion(Request) :-
cors_enable(Request, [methods([get])]),
	shortest_prepareEmotion(Request, Strength, Path),
	prolog_to_json(Path, JSONObject),
	prolog_to_json(Strength, JSONObject2),
	reply_json([JSONObject, JSONObject2], [json_object(dict)]).

shortest_prepareEmotion(Request, Strength, Path) :-
http_parameters(Request, [emailPlayer(EmailPlayer, [string]), emailTarget(EmailTarget, [string]),mode(Mode, [integer]) , n(N, [integer]), joy(Joy, [number]), anguish(Anguish, [number]), hope(Hope, [number]), deception(Deception, [number]), fear(Fear, [number]), relief(Relief, [number])]),
addPlayers(),
addConnections(),
getPlayerName(EmailPlayer, PlayerName),
getPlayerName(EmailTarget, TargetName),
    node(PlayerId, PlayerName, _),
    node(TargetId, TargetName, _),
    retract(occ(PlayerId, _,_,_,_,_,_)),
    asserta(occ(PlayerId, Joy, Anguish, Hope, Deception, Fear, Relief)),
shortest_route(Mode, 1, N, PlayerId, TargetId, Strength, Path),
retractall(connection(_,_,_,_,_,_)),
retractall(node(_,_,_)).


%======== Shortest route between two players (Core) ========%

shortest_allDfs(Player1, Player2, PathList):- get_time(T1),
    findall(Path, shortest_dfs(0, 0, 100, Player1, Player2, _, Path), PathList),
    length(PathList, PathLength),
    get_time(T2),
    write(PathLength),write(' paths found in '),
    T is T2-T1,write(T),write(' seconds'),nl,
    write('Possible Path List: '),write(PathList),nl,nl.

shortest_dfs(Mode, EmotionBool, N, Orig, Dest, Strength, Path):- shortest_dfsAux(Mode, EmotionBool, 0, N, Orig, Dest, [Orig], Strength, Path).

shortest_dfsAux(_, _, _, _, Dest, Dest, LA, 0, Path):- !, reverse(LA, Path).
shortest_dfsAux(_, _, M, N, _, _, _, _, _):- M >= N, !, false.
shortest_dfsAux(0, EmotionBool, M, N, Current, Dest, LA, Strength, Path):-
    (connection(Current, X, StrengthA, _, _, _);
    connection(X, Current, _, StrengthA, _, _)),
    emotion_checkSameEmotion(EmotionBool, Current, X),
    \+ member(X,LA),
    M1 is M + 1,
    shortest_dfsAux(0, EmotionBool, M1, N, X,Dest,[X|LA], Strength1, Path),
	Strength is Strength1 + StrengthA.
shortest_dfsAux(1, EmotionBool, M, N, Current, Dest, LA, Strength, Path):-
    (connection(Current, X, StrengthA, _, RelA, _);
    connection(X, Current, _, StrengthA, _, RelA)),
    emotion_checkSameEmotion(EmotionBool, Current, X),
    \+ member(X,LA),
    M1 is M + 1,
    shortest_dfsAux(1, EmotionBool, M1, N, X,Dest,[X|LA], Strength1, Path),
    getMulticriteria(StrengthA, RelA, FinalStrength),
    Strength is Strength1 + FinalStrength.

shortest_route(Mode, EmotionBool, N, Orig, Dest, Strength, ShortestPathList):-
		(shortest_findRoute(Mode, EmotionBool, N, Orig, Dest); true),
		retract(shortest_currentRoute(ShortestPathList, _, Strength)).

shortest_findRoute(Mode, EmotionBool, N, Orig, Dest):-
		asserta(shortest_currentRoute(_,10000, _)),
		shortest_dfs(Mode, EmotionBool, N, Orig, Dest, Strength, PathList),
		shortest_updateRoute(Strength, PathList),
		fail.

shortest_updateRoute(Strength, PathList):-
		shortest_currentRoute(_, CurrentPathLength, _),
		length(PathList, PathLength),
		PathLength < CurrentPathLength, retract(shortest_currentRoute(_,_,_)),
		asserta(shortest_currentRoute(PathList, PathLength, Strength)).

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
	retractall(connection(_,_,_,_,_,_)),
	retractall(node(_,_,_)).


%======== Safest path between two players (Core) ========%

safest_dfs(Orig, Dest, Threshold, Strength, Path):- safest_dfsAux(Orig, Dest, [Orig], Threshold, 0, Strength, Path).

safest_dfsAux(Dest, Dest, AuxList, _ , Strength, Strength, Path):-!, reverse(AuxList,Path).
safest_dfsAux(Current, Dest, AuxList, Threshold, Strength, ReturnStrength, Path):-
		(connection(Current, Friend, StrengthA, StrengthB,_,_);
		connection(Friend, Current, StrengthA, StrengthB,_,_)),
		\+ member(Friend, AuxList),
		StrengthA >= Threshold,
		StrengthB >= Threshold,
		CurrentStrength is Strength + StrengthA + StrengthB,
		safest_dfsAux(Friend, Dest, [Friend | AuxList], Threshold, CurrentStrength, ReturnStrength, Path).


safest_route(Orig, Dest, Threshold, SafestPath):-
		(safest_findRoute(Orig,Dest, Threshold);true),
		retract(safest_currentRoute(SafestPath,Strength)),
		((Strength >= 0, !);
		false).

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
    retractall(connection(_,_,_,_,_,_)),
    retractall(node(_,_,_)).


%======== Suggest Players (Core) ========%

suggest_players(Player, Level, SuggestedPlayersList):-
		network_getNetworkByLevel(Player, Level, NetworkList, _),
		suggest_removeFriends(Player, NetworkList, CandidateList),
		suggest_getRelatedPlayers(Player, CandidateList, RelatedPlayersList),
		suggest_checkSuggestedPaths(Player, RelatedPlayersList, SuggestedPlayersList).

suggest_removeFriends(_, [ ], []).
suggest_removeFriends(Player, [CurrentPlayer | NetworkList], CandidateList):-
		(connection(Player, CurrentPlayer, _, _,_,_);
		connection(CurrentPlayer, Player, _, _,_,_)), !,
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
		(connection(Current, Friend, _, _,_,_);
		connection(Friend, Current, _, _,_,_)),
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
	retractall(connection(_,_,_,_,_,_)),
	retractall(node(_,_,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strongest_dfs(OrigId, DestId, Strength, Path):- strongest_dfsAux(OrigId, DestId, [OrigId], Strength, Path).

strongest_dfsAux(DestId, DestId, AuxList, 0, Path):-!,reverse(AuxList, Path).
strongest_dfsAux(CurrentId, DestId, AuxList, Strength, Path):-
	(connection(CurrentId, FriendID, StrengthA, StrengthB,_,_);
	connection(FriendID, CurrentId, StrengthA, StrengthB,_,_)),
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
    common_tagsPrepare(Request, ResultTag, ResultUsers),
	prolog_to_json(ResultTag, JSONObject),
	prolog_to_json(ResultUsers, JSONObject2),
	reply_json([JSONObject, JSONObject2], [json_object(dict)]).

common_tagsPrepare(Request, ResultTag, ResultUsers) :-
    http_parameters(Request, [id(Id, [string]),ntags(NTags, [number]),nusers(NUsers, [number]),taglist(TagList, [string])]),
	addPlayers(),
	addConnections(),
	(TagList = "_" ->
		common_tags(Id,NTags, NUsers, [], ResultTag, ResultUsers);
		split_string(TagList, "/", "", TagListResult),
		common_tags(Id,NTags, NUsers, TagListResult, ResultTag, ResultUsers)),
	retractall(connection(_,_,_,_,_,_)),
	retractall(node(_,_,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

common_tags(Id, NTags,NUsers,TagList,ResultTag,ResultUsers):-
    node(Id,_,All_TagsT),
	common_tags_change_to_synonyms(All_TagsT, All_Tags),
    findall(Combination,common_tags_combination(NTags,All_Tags,Combination),CombinationsTemp),
	common_tags_test_list(CombinationsTemp, TagList, Combinations),
    findall(UserId,node(UserId,_,_),Users),
	asserta(common_tags_users([],[],0)),
    common_tags_users_combination(NTags,NUsers,Users,Combinations),
	common_tags_users(ResultTag,ResultUsers,_),
    retractall(common_tags_users(_,_,_)).

common_tags_users_combination(_,_,_,[]).
common_tags_users_combination(NTags,NUsers,Users,[Combination|Combinations]):-
    common_tags_users_combination_aux(NTags,Combination,Users,Users_With_Tags),
    common_tags_users_combination(NTags,NUsers,Users,Combinations),
    !,
	length(Users_With_Tags, L),
	( L >= NUsers->
	common_tags_users(_,_,Size),
	(L > Size ->
	retract(common_tags_users(_,_,_)),
	asserta(common_tags_users(Combination,Users_With_Tags, L)) ; ! );!).

common_tags_users_combination_aux(_,_,[],[]):-!.
common_tags_users_combination_aux(NTags,Tags,[U|Users],Result):-
    node(U,_,User_TagsT),
	common_tags_change_to_synonyms(User_TagsT, User_Tags),
    intersection(Tags, User_Tags,Commun),
    length(Commun, Size),
    Size >= NTags, !,
    common_tags_users_combination_aux(NTags,Tags,Users,Result1),
    append([U], Result1, Result).
common_tags_users_combination_aux(NTags,Tags,[_|Users],Result):-
    !,
    common_tags_users_combination_aux(NTags,Tags,Users,Result).

common_tags_test_list([],_,[]):-!.
common_tags_test_list([CombinationsH|CombinationsT], Tags, FinalCombinations):-
	common_tags_test_list(CombinationsT, Tags, FinalCombinations1),
	(common_tags_test_lists(Tags, CombinationsH) ->
		append([CombinationsH], FinalCombinations1, FinalCombinations);
		append([], FinalCombinations1, FinalCombinations)).

common_tags_test_lists(List1, List2) :-
    forall(member(Element,List1), member(Element,List2)).


%=== CombinaÃ§oes ===
common_tags_combination(0,_,[]).
common_tags_combination(N,[X|T],[X|Comb]):-N>0,N1 is N-1,common_tags_combination(N1,T,Comb).
common_tags_combination(N,[_|T],Comb):-N>0,common_tags_combination(N,T,Comb).

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
    retractall(connection(_,_,_,_,_,_)),
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
    (connection(Act,X,_,_,_,_);connection(X,Act,_,_,_,_)),
    \+userVisited(X),
    \+ member(X,LA),
    Level1 is Level-1,
    asserta(userVisited(X)),
    dfs2(X,Level1,[X|LA]).


%======== A-Star (HTTP) ========%

:- http_handler('/api/a-star', aStar_compute, []).

aStar_compute(Request) :-
	cors_enable(Request, [methods([get])]),
    aStar_prepare(Request, Path, Cost),
	prolog_to_json(Path, JSONObject),
	prolog_to_json(Cost, JSONObject2),
	reply_json([JSONObject, JSONObject2], [json_object(dict)]).

aStar_prepare(Request, Path, Cost) :-
    http_parameters(Request, [emailPlayer(EmailPlayer, [string]), emailTarget(EmailTarget, [string]),
    n(N, [integer]), mode(Mode, [integer])]),
	addPlayers(),
	addConnections(),
	getPlayerName(EmailPlayer, PlayerName),
	getPlayerName(EmailTarget, TargetName),
        node(PlayerId, PlayerName, _),
        node(TargetId, TargetName, _),
	aStar_find(Mode, 0, N, PlayerId, TargetId, Path, Cost),
	retractall(connection(_,_,_,_,_,_)),
	retractall(node(_,_,_)).

%======== A-Star w/ Emotions (HTTP) ========%

:- http_handler('/api/a-star-emotions', aStar_computeEmotions, []).

aStar_computeEmotions(Request) :-
	cors_enable(Request, [methods([get])]),
    aStar_prepareEmotions(Request, Path, Cost),
	prolog_to_json(Path, JSONObject),
	prolog_to_json(Cost, JSONObject2),
	reply_json([JSONObject, JSONObject2], [json_object(dict)]).

aStar_prepareEmotions(Request, Path, Cost) :-
    http_parameters(Request, [emailPlayer(EmailPlayer, [string]), emailTarget(EmailTarget, [string]), n(N, [integer]), mode(Mode, [integer]), joy(Joy, [number]), anguish(Anguish, [number]), hope(Hope, [number]), deception(Deception, [number]), fear(Fear, [number]), relief(Relief, [number])]),
	addPlayers(),
	addConnections(),
	getPlayerName(EmailPlayer, PlayerName),
	getPlayerName(EmailTarget, TargetName),
        node(PlayerId, PlayerName, _),
        node(TargetId, TargetName, _),
        retract(occ(PlayerId, _,_,_,_,_,_)),
        asserta(occ(PlayerId, Joy, Anguish, Hope, Deception, Fear, Relief)),
	aStar_find(Mode, 1, N, PlayerId, TargetId, Path, Cost),
	retractall(connection(_,_,_,_,_,_)),
	retractall(node(_,_,_)).


%======== A-Star (Core) ========%

aStar_find(Mode, EmotionBool, Threshold, Orig, Dest, Path, Cost):-
    (retract(aStar_orderedList(_));true),
    aStar_getStrengthListByPlayer(Mode, Threshold, Orig, StrengthList),
    asserta(aStar_orderedList(StrengthList)),
    aStar_aux(Mode, EmotionBool, 0, Threshold, Dest,[(_,0,[Orig])],Path,Cost).
aStar_aux(_, _, M, N, Dest,[(_,Cost,[Dest|T])|_],Path,Cost):- M >= N,reverse([Dest|T],Path).
aStar_aux(0, EmotionBool, M, N, Dest,[(_,Ca,LA)|Others],Path,Cost):-
    LA=[Act|_],
    findall((CEX,CaX,[X|LA]),
    (Dest\==Act,
    (connection(Act,X,CostX, _, _, _);
    connection(X, Act, _, CostX, _, _)),
    emotion_checkSameEmotion(EmotionBool, Act, X),
    \+ member(X,LA),
    CaX is CostX + Ca,
    aStar_estimate(N,M,EstX),
    CEX is CaX + EstX),
    New),
    append(Others,New,All),
    sort(All,AllOrd),
    M1 is M + 1,
    aStar_aux(0, EmotionBool, M1, N, Dest,AllOrd,Path,Cost).

aStar_aux(1, EmotionBool, M, N, Dest,[(_,Ca,LA)|Others],Path,Cost):-
    LA=[Act|_],
    findall((CEX,CaX,[X|LA]),
    (Dest\==Act,
    (connection(Act,X,ConnStrength, _, RelStrength, _);
    connection(X, Act, _, ConnStrength, _, RelStrength)),
    emotion_checkSameEmotion(EmotionBool, Act, X),
    \+ member(X,LA),
    getMulticriteria(ConnStrength, RelStrength, CostX),
    CaX is CostX + Ca,
    aStar_estimate(N,M,EstX),
    CEX is CaX + EstX),
    New),
    append(Others,New,All),
    sort(All,AllOrd),
    M1 is M + 1,
    aStar_aux(1, EmotionBool, M1, N, Dest,AllOrd,Path,Cost).


aStar_estimate(N,M, Est):-
    retract(aStar_orderedList([H|List])),
    Est is H * (N - M),
    asserta(aStar_orderedList(List)).

aStar_sort(List, ResultList):- sort(0,  @>=, List,  ResultList).

aStar_getOrderedList(ReturnList):-
    findall(FirstStrength, connection(_, _, FirstStrength, _, _, _), AllFirstList),
    findall(SecondStrength, connection(_, _, _, SecondStrength, _, _), AllSecondList),
    listUnion(AllFirstList, AllSecondList, AllList),
    sort(0, @>=, AllList, ReturnList).

aStar_getStrengthListByPlayer(Mode, N, PlayerId, ReturnList):-
    aStar_getStrengthListByLevel(Mode, 0, N, [PlayerId], AllList),
    sort(0, @>=, AllList, ReturnList).

aStar_getStrengthListByLevel(_, N, N, _, []):-!.
aStar_getStrengthListByLevel(Mode, M, N, PlayerList, ReturnList):-
    aStar_getStrengthListByPlayersList(Mode, PlayerList, FriendsList, StrengthList),
    M1 is M + 1,
    aStar_getStrengthListByLevel(Mode, M1, N, FriendsList, List),
    listUnion(StrengthList, List, ReturnList).

aStar_getStrengthListByPlayersList(_, [], [], []):-!.
aStar_getStrengthListByPlayersList(Mode, [PlayerId | PlayerList], ReturnFriendList, ReturnList):-
    network_getNetworkByLevel(PlayerId, 1, FriendsList, _),
    aStar_getStrengthListByFriendsList(Mode, PlayerId, FriendsList, StrengthList),
    aStar_getStrengthListByPlayersList(Mode, PlayerList, FList, List),
    listUnion(FriendsList, FList, ReturnFriendList),
    listUnion(StrengthList, List, ReturnList).

aStar_getStrengthListByFriendsList(_, _, [], []):-!.
aStar_getStrengthListByFriendsList(0, PlayerId, [FriendId|FriendList], [FirstStrength|[SecondStrength|StrengthList]]):-
    (connection(PlayerId, FriendId, FirstStrength, SecondStrength, _, _);
    connection(FriendId, PlayerId, FirstStrength, SecondStrength, _, _)),
    aStar_getStrengthListByFriendsList(0, PlayerId, FriendList, StrengthList).
aStar_getStrengthListByFriendsList(1, PlayerId, [FriendId|FriendList], [FirstMulti|[SecondMulti|StrengthList]]):-
    (connection(PlayerId, FriendId, FirstStrength, SecondStrength, FirstRelStrength, SecondRelStrength);
    connection(FriendId, PlayerId, FirstStrength, SecondStrength, FirstRelStrength, SecondRelStrength)),
    getMulticriteria(FirstStrength, FirstRelStrength, FirstMulti),
    getMulticriteria(SecondStrength, SecondRelStrength, SecondMulti),
    aStar_getStrengthListByFriendsList(1, PlayerId, FriendList, StrengthList).


%======== Emotion Relation Variation (HTTP) ========%

:- http_handler('/api/emotion-relation', emotion_relationCompute, []).

emotion_relationCompute(Request) :-
	cors_enable(Request, [methods([get])]),
    emotion_relationPrepare(Request, NewJoy, NewAnguish, Hope, Deception, Fear, Relief),
	prolog_to_json(NewJoy, JSONObject),
	prolog_to_json(NewAnguish, JSONObject2),
        prolog_to_json(Hope, JSONObject3),
	prolog_to_json(Deception, JSONObject4),
	prolog_to_json(Fear, JSONObject5),
	prolog_to_json(Relief, JSONObject6),
	reply_json([JSONObject, JSONObject2, JSONObject3, JSONObject4, JSONObject5, JSONObject6], [json_object(dict)]).

emotion_relationPrepare(Request, NewJoy, NewAnguish, Hope, Deception, Fear, Relief) :-
    http_parameters(Request, [emailPlayer(EmailPlayer, [string]), value(Value, [integer]),joy(Joy, [number]),anguish(Anguish, [number]),hope(Hope, [number]),deception(Deception, [number]),fear(Fear, [number]),relief(Relief, [number])]),
	addPlayers(),
	addConnections(),
	getPlayerName(EmailPlayer, PlayerName),
        node(PlayerId, PlayerName, _),
        retract(occ(PlayerId, _,_,_,_,_,_)),
        asserta(occ(PlayerId, Joy, Anguish, Hope, Deception, Fear, Relief)),
	emotion_relationChange(PlayerId, Value, NewJoy, NewAnguish),
	retractall(connection(_,_,_,_,_,_)),
	retractall(node(_,_,_)).

%======== Emotion Suggested Variation (HTTP) ========%

:- http_handler('/api/emotion-suggested', emotion_suggestedCompute, []).

emotion_suggestedCompute(Request) :-
	cors_enable(Request, [methods([get])]),
    emotion_suggestedPrepare(Request, Joy, Anguish, NewHope, NewDeception, NewFear, NewRelief),
	prolog_to_json(Joy, JSONObject),
        prolog_to_json(Anguish, JSONObject2),
        prolog_to_json(NewHope, JSONObject3),
	prolog_to_json(NewDeception, JSONObject4),
	prolog_to_json(NewFear, JSONObject5),
	prolog_to_json(NewRelief, JSONObject6),
	reply_json([JSONObject, JSONObject2, JSONObject3, JSONObject4, JSONObject5, JSONObject6], [json_object(dict)]).

emotion_suggestedPrepare(Request, Joy, Anguish,NewHope, NewDeception, NewFear, NewRelief) :-
    http_parameters(Request, [emailPlayer(EmailPlayer, [string]),ntags(TagCount, [number]),nusers(PlayerCount, [number]),taglist(MandatoryTags, [string]),joy(Joy, [number]),anguish(Anguish, [number]),hope(Hope, [number]),deception(Deception, [number]),fear(Fear, [number]),relief(Relief, [number])]),
	addPlayers(),
	addConnections(),
	getPlayerName(EmailPlayer, PlayerName),
        node(PlayerId, PlayerName, _),
        retract(occ(PlayerId, _,_,_,_,_,_)),
        asserta(occ(PlayerId, Joy, Anguish, Hope, Deception, Fear, Relief)),
	emotion_groupSuggestion(PlayerId, TagCount, PlayerCount, MandatoryTags, NewHope, NewDeception, NewFear, NewRelief),
	retractall(connection(_,_,_,_,_,_)),
	retractall(node(_,_,_)).


%======== Emotion Variation (Core) ========%

emotion_increase(PreviousValue, Value, Saturation, Return):-
    ((Value < Saturation, !, Min is Value);
    (!, Min is Saturation)),
    Div is Min / Saturation,
    Return is PreviousValue + (1 - PreviousValue) * Div.

emotion_decrease(PreviousValue, Value, Saturation, Return):-
    ((Value < Saturation, !, Min is Value);
    (!, Min is Saturation)),
    Div is 1 - (Min / Saturation),
    Return is PreviousValue * Div.

emotion_relationChange(PlayerId, Value, NewJoy, NewAnguish):-
    occ(PlayerId, Joy, Anguish, _, _, _, _),
    ((Value > 0, !,
     emotion_increase(Joy, Value, 200, NewJoy),
     emotion_decrease(Anguish, Value, 200, NewAnguish));
    (Value < 0, !,
    emotion_increase(Anguish, -Value, 200, NewAnguish),
    emotion_decrease(Joy, -Value, 200, NewJoy))),
    retract(occ(PlayerId, Joy, Anguish, Hope, Deception, Fear, Relief)),
    asserta(occ(PlayerId, NewJoy, NewAnguish, Hope, Deception, Fear, Relief)).

emotion_groupSuggestion(PlayerId, TagCount, PlayerCount, MandatoryTags, NewHope, NewDeception, NewFear, NewRelief):-
    common_tags(PlayerId, TagCount, PlayerCount, MandatoryTags, _,SuggestedGroup),
    emotion_checkHope(PlayerId, SuggestedGroup, NewHope, NewDeception),
    emotion_checkFear(PlayerId, SuggestedGroup, NewFear, NewRelief),
    retract(occ(PlayerId, Joy, Anguish, _, _, _, _)),
    asserta(occ(PlayerId, Joy, Anguish, NewHope, NewDeception, NewFear, NewRelief)).

emotion_checkHope(PlayerId, SuggestedGroup, NewHope, NewDeception):-
    emotion_countHope(PlayerId, SuggestedGroup, 0, Counter),
    occ(PlayerId, _, _, Hope, Deception, _, _),
    length(SuggestedGroup, Length),
    Complementary is Length - Counter,
    ((Counter > Complementary, !,
      emotion_increase(Hope, Counter, Length, NewHope),
     emotion_decrease(Deception, Complementary, Length, NewDeception));
    (emotion_increase(Deception, Complementary, Length, NewDeception),
    emotion_decrease(Hope, Counter, Length, NewHope))).

emotion_countHope(_, [], Counter, Return):- Return is Counter.
emotion_countHope(PlayerId, [H | Group], Counter, Return):-
    hope(PlayerId, H),
    !, Counter1 is Counter + 1,
    emotion_countHope(PlayerId, Group, Counter1, Return).
emotion_countHope(PlayerId, [_ | Group], Counter, Return):-
    !,emotion_countHope(PlayerId, Group, Counter, Return).



emotion_checkFear(PlayerId, SuggestedGroup, NewFear, NewRelief):-
    emotion_countFear(PlayerId, SuggestedGroup, 0, Counter),
    occ(PlayerId, _, _, _, _, Fear, Relief),
    length(SuggestedGroup, Length),
    Complementary is Length - Counter,
    ((Counter > Complementary, !,
      emotion_increase(Fear, Counter, Length, NewFear),
     emotion_decrease(Relief, Complementary, Length, NewRelief));
    (emotion_increase(Relief, Complementary, Length, NewRelief),
    emotion_decrease(Fear, Counter, Length, NewFear))).

emotion_countFear(_, [], Counter, Return):- Return is Counter.
emotion_countFear(PlayerId, [H | Group], Counter, Return):-
    fear(PlayerId, H),
    !, Counter1 is Counter + 1,
    emotion_countFear(PlayerId, Group, Counter1, Return).
emotion_countFear(PlayerId, [_ | Group], Counter, Return):-
    !,emotion_countFear(PlayerId, Group, Counter, Return).

emotion_getMax(PlayerId, MaxValue, MaxEmotion):-
    occ(PlayerId, Joy, Anguish, Hope,Deception, Fear, Relief),
    emotion_maxEmotion([Joy, Anguish, Hope, Deception, Fear, Relief], [joy, anguish, hope, deception, fear, relief],-100, emotion, MaxValue, MaxEmotion).

emotion_maxEmotion([], [], MaxAuxValue, MaxAuxEmotion, MaxReturnValue, MaxReturnEmotion):- !, MaxReturnValue is MaxAuxValue, MaxReturnEmotion = MaxAuxEmotion.
emotion_maxEmotion([HV|TV], [HE|TE], MaxAuxValue, _, MaxReturnValue, MaxReturnEmotion):-
    HV >= MaxAuxValue, !,
    emotion_maxEmotion(TV, TE, HV, HE, MaxReturnValue, MaxReturnEmotion).
emotion_maxEmotion([_|TV], [_|TE], MaxAuxValue, MaxAuxEmotion, MaxReturnValue, MaxReturnEmotion):-
    !, emotion_maxEmotion(TV, TE, MaxAuxValue, MaxAuxEmotion, MaxReturnValue, MaxReturnEmotion).

emotion_checkSameEmotion(EmotionBool, Act, X):-
    ((EmotionBool =:= 1, !,
     emotion_getMax(Act, _, ActEmotion),
     emotion_getMax(X, _, XEmotion),
     ((ActEmotion = XEmotion,!);
     false));
    true).
