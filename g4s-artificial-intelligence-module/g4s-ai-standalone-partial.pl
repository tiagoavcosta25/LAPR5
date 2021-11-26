% Knowledge Base - Standalone Mode

node(1,john,[gaming,programming]).
node(2,jane,[gaming]).
node(3,steve,[programming]).
node(4,james,[gaming]).
node(5,carol,[programming]).
node(6,michelle,[movies]).



connection(1,2,10,8).
connection(2,4,2,6).
connection(2,5,-3,-2).
connection(2,6,-3,-2).
connection(1,3,-3,-2).
connection(3,5,-3,-2).



% aux methods

intersect([ ],_,[ ]).
intersect([X|L1],L2,[X|LI]):-member(X,L2),!,intersect(L1,L2,LI).
intersect([_|L1],L2, LI):- intersect(L1,L2,LI).

% shortest path

:-dynamic shortest_current_path/2.

all_dfs(Player1,Player2,PathList):-get_time(T1),
    findall(Path,dfs(Player1,Player2,Path),PathList),
    length(PathList,PathLength),
    get_time(T2),
    write(PathLength),write(' paths found in '),
    T is T2-T1,write(T),write(' seconds'),nl,
    write('Possible Path List: '),write(PathList),nl,nl.

dfs(Orig,Dest,Path):-dfs2(Orig,Dest,[Orig],Path).

dfs2(Dest,Dest,LA,Path):-!,reverse(LA,Path).
dfs2(Current,Dest,LA,Path):-node(CurrentID,Current,_),(connection(CurrentID,NX,_,_);connection(NX,CurrentID,_,_)),
    node(NX,X,_),\+ member(X,LA),dfs2(X,Dest,[X|LA],Path).


shortest_path(Orig,Dest,ShortestPathList):-
		get_time(Ti),
		(find_shortest_path(Orig,Dest);true),
		retract(shortest_current_path(ShortestPathList,_)),
		get_time(Tf),
		T is Tf-Ti,
		write('Solution generation time:'),write(T),nl.

find_shortest_path(Orig,Dest):-
		asserta(shortest_current_path(_,10000)),
		dfs(Orig,Dest,PathList),
		update_shortest_path(PathList),
		fail.

update_shortest_path(PathList):-
		shortest_current_path(_,N),
		length(PathList,P),
                P<N,retract(shortest_current_path(_,_)),
		asserta(shortest_current_path(PathList,P)).

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

network_getNetworkByLevel(_, _, [jane, steve, james, carol, michelle]):-!.

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
