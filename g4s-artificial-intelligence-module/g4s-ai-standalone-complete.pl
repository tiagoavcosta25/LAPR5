% Knowledge Base - Standalone Mode

node(1,ana,[natureza,pintura,musica,sw,porto]).
node(11,antonio,[natureza,pintura,carros,futebol,lisboa]).
node(12,beatriz,[natureza,musica,carros,porto,moda]).
node(13,carlos,[natureza,musica,sw,futebol,coimbra]).
node(14,daniel,[natureza,cinema,jogos,sw,moda]).
node(21,eduardo,[natureza,cinema,teatro,carros,coimbra]).
node(22,isabel,[natureza,musica,porto,lisboa,cinema]).
node(23,jose,[natureza,pintura,sw,musica,carros,lisboa]).
node(24,luisa,[natureza,cinema,jogos,moda,porto]).
node(31,maria,[natureza,pintura,musica,moda,porto]).
node(32,anabela,[natureza,cinema,musica,tecnologia,porto]).
node(33,andre,[natureza,carros,futebol,coimbra]).
node(34,catia,[natureza,musica,cinema,lisboa,moda]).
node(41,cesar,[natureza,teatro,tecnologia,futebol,porto]).
node(42,diogo,[natureza,futebol,sw,jogos,porto]).
node(43,ernesto,[natureza,teatro,carros,porto]).
node(44,isaura,[natureza,moda,tecnologia,cinema]).
node(200,sara,[natureza,moda,musica,sw,coimbra]).

node(51,rodolfo,[natureza,musica,sw]).
node(61,rita,[moda,tecnologia,cinema]).


connection(1,11,10,8).
connection(1,12,2,6).
connection(1,13,-3,-2).
connection(1,14,1,-5).
connection(11,21,5,7).
connection(11,22,2,-4).
connection(11,23,-2,8).
connection(11,24,6,0).
connection(12,21,4,9).
connection(12,22,-3,-8).
connection(12,23,2,4).
connection(12,24,-2,4).
connection(13,21,3,2).
connection(13,22,0,-3).
connection(13,23,5,9).
connection(13,24,-2, 4).
connection(14,21,2,6).
connection(14,22,6,-3).
connection(14,23,7,0).
connection(14,24,2,2).
connection(21,31,2,1).
connection(21,32,-2,3).
connection(21,33,3,5).
connection(21,34,4,2).
connection(22,31,5,-4).
connection(22,32,-1,6).
connection(22,33,2,1).
connection(22,34,2,3).
connection(23,31,4,-3).
connection(23,32,3,5).
connection(23,33,4,1).
connection(23,34,-2,-3).
connection(24,31,1,-5).
connection(24,32,1,0).
connection(24,33,3,-1).
connection(24,34,-1,5).
connection(31,41,2,4).
connection(31,42,6,3).
connection(31,43,2,1).
connection(31,44,2,1).
connection(32,41,2,3).
connection(32,42,-1,0).
connection(32,43,0,1).
connection(32,44,1,2).
connection(33,41,4,-1).
connection(33,42,-1,3).
connection(33,43,7,2).
connection(33,44,5,-3).
connection(34,41,3,2).
connection(34,42,1,-1).
connection(34,43,2,4).
connection(34,44,1,-2).
connection(41,200,2,0).
connection(42,200,7,-2).
connection(43,200,-2,4).
connection(44,200,-1,-3).

connection(1,51,6,2).
connection(51,61,7,3).
connection(61,200,2,4).

% aux methods

intersect([ ],_,[ ]).
intersect([X|L1],L2,[X|LI]):-member(X,L2),!,intersect(L1,L2,LI).
intersect([_|L1],L2, LI):- intersect(L1,L2,LI).

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

% safest route

:-dynamic safest_currentRoute/2.

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
