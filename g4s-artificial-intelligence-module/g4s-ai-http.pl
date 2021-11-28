% Libraries
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json_convert)).
%:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

% HTTP Request Relations
:- http_handler('/suggestPlayers', suggest_players, []).
:- http_handler('/shortestPath', shortest_path, []).
:- http_handler('/safestPath', safest_route, []).

% HTTP Server Creation on port 'Port'
server(Port) :-
        http_server(http_dispatch, [port(Port)]).

:- json_object playerDTO(id:string, email:integer, name:string, phone:string, dob:string, emotional:string, facebook:string, linkedin:string, tags:string).


p_json(Request) :-
        http_read_json(Request, JSON, [json_object(dict)]),
        R = playerDTO(JSON.id,JSON.email, JSON.name, JSON.phone, JSON.dob, JSON.emotional, JSON.facebook, JSON.linkedin, JSON.tags),
        prolog_to_json(R, JSONObject),
        reply_json(JSONObject, [json_object(dict)]).

persistence_getPlayers():-
        http_get('https://localhost:5001/api/players/',Reply,[ cert_verify_hook(cert_accept_any)
            ]),
        write('Reply: '), write(Reply), nl,
        persistence_registerPlayers(Reply).


persistence_registerPlayers([]).
persistence_registerPlayers([CurrentPlayer | PlayerList]):-
        asserta(node(CurrentPlayer.id, CurrentPlayer.name, CurrentPlayer.tags)),
        persistence_registerPlayers(PlayerList).

persistence_getConnections():-
        http_get('https://localhost:5001/api/Connections',Reply,[]),
        write('Reply: '), write(Reply), nl,
        persistence_registerConnections(Reply).

persistence_registerConnections([ ]).
persistence_registerConnections([CurrentConnection | ConnectionList]):-
        asserta(connection(CurrentConnection.player, CurrentConnection.friend, CurrentConnection.connectionStrength, CurrentConnection.connectionStrength)),
        persistence_registerConnections(ConnectionList).


% shortest path

:-dynamic shortest_currentPath/2.

all_dfs(Player1, Player2, PathList):- get_time(T1),
    findall(Path, dfs(Player1, Player2, Path), PathList),
    length(PathList, PathLength),
    get_time(T2),
    write(PathLength),write(' paths found in '),
    T is T2-T1,write(T),write(' seconds'),nl,
    write('Possible Path List: '),write(PathList),nl,nl.

dfs(Orig, Dest, Path):- dfs2(Orig, Dest, [Orig], Path).

dfs2(Dest, Dest, LA, Path):- !, reverse(LA, Path).
dfs2(Current, Dest, LA, Path):-
    node(CurrentID, Current,_), (connection(CurrentID, NX, _, _); connection(NX, CurrentID, _, _)),
    node(NX,X,_),\+ member(X,LA), dfs2(X,Dest,[X|LA],Path).


shortest_path(Orig, Dest, ShortestPathList):-
		get_time(Ti),
		(shortest_findPath(Orig, Dest); true),
		retract(shortest_currentPath(ShortestPathList, _)),
		get_time(Tf),
		T is Tf-Ti,
		write('Solution generation time:'), write(T), nl.

shortest_findPath(Orig, Dest):-
		asserta(shortest_currentPath(_,10000)),
		dfs(Orig, Dest, PathList),
		shortest_updatePath(PathList),
		fail.

shortest_updatePath(PathList):-
		shortest_currentPath(_, CurrentPathLength),
		length(PathList, PathLength),
    PathLength < CurrentPathLength, retract(shortest_currentPath(_,_)),
		asserta(shortest_currentPath(PathList, PathLength)).


% aux methods

intersect([ ],_,[ ]).
intersect([X|L1],L2,[X|LI]):-member(X,L2),!,intersect(L1,L2,LI).
intersect([_|L1],L2, LI):- intersect(L1,L2,LI).

% safest route

:-dynamic safest_current_route/2.

dfs_safest_route(Orig,Dest,Threshold, Strength, Path):-dfs2_safest_route(Orig,Dest,[Orig],Threshold, 0, Strength, Path).

dfs2_safest_route(Dest,Dest,LA, _, S,S, Cam):-!,reverse(LA,Cam).
dfs2_safest_route(Act,Dest,LA,Threshold,Strength, ReturnStrength,Cam):-node(NAct,Act,_),(connection(NAct,NX,N_StrengthA,N_StrengthB);connection(NX,NAct,N_StrengthA,N_StrengthB)),
    node(NX,X,_),\+ member(X,LA),N_StrengthA >= Threshold,
    N_StrengthB >= Threshold, FinalStrength1 is N_StrengthA + N_StrengthB,FinalStrength is Strength + FinalStrength1, dfs2_safest_route(X,Dest,[X|LA], Threshold, FinalStrength, ReturnStrength,Cam).


safest_route(Orig,Dest,Threshold, SafestPath):-
		get_time(Initial_Time),
		(find_safest_route(Orig,Dest, Threshold);true),
		retract(safest_current_route(SafestPath,Strength)),
		((Strength >= 0, !, get_time(End_Time),
		T is End_Time - Initial_Time,
		write('Time:'),write(T),nl,
		write('Strength: '), write(Strength),nl);
		write('No path found.'),false).

find_safest_route(Orig,Dest,Threshold):-
		asserta(safest_current_route(_,-10000)),
		dfs_safest_route(Orig,Dest,Threshold, Strength, PathList),
		update_safest_route(Strength, PathList),
		fail.

update_safest_route(Strength, PathList):-
		safest_current_route(_,Current_Strength),
		Strength > Current_Strength,retract(safest_current_route(_,_)),
		asserta(safest_current_route(PathList,Strength)).


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
