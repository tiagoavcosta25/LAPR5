% Knowledge Base - Standalone Mode

node(1,john,[gaming,programming,'c#']).
node(2,jane,[gaming,csharp]).
node(3,steve,[programming]).
node(4,james,[gaming]).
node(5,carol,[programming]).
node(6,michelle,[movies]).



connection(1,2,10,8,50,0).
connection(2,4,2,6,100,12).
connection(2,5,19,-19,12,19).
connection(2,6,4,0,25,24).
connection(1,3,11,3,35,90).
connection(3,5,12,-2,10,31).


synonym('c#', csharp).


% Secundary knowledge base
:- dynamic shortest_currentRoute/2.
:- dynamic safest_currentRoute/2.
:- dynamic aStar_orderedList/1.

% aux methods

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


%======== Shortest route between two players (Core) ========%

shortest_allDfs(Player1, Player2, PathList):- get_time(T1),
    findall(Path, shortest_dfs(Player1, Player2, Path), PathList),
    length(PathList, PathLength),
    get_time(T2),
    write(PathLength),write(' paths found in '),
    T is T2-T1,write(T),write(' seconds'),nl,
    write('Possible Path List: '),write(PathList),nl,nl.

shortest_dfs(N, Orig, Dest, Path):- shortest_dfsAux(0, N, Orig, Dest, [Orig], Path).

shortest_dfsAux(_, _, Dest, Dest, LA, Path):- !, reverse(LA, Path).
shortest_dfsAux(M, N, _, _, _, _):- M >= N, !, false.
shortest_dfsAux(M, N, Current, Dest, LA, Path):-
    (connection(Current, X, _, _, _, _);
    connection(X, Current, _, _, _, _)),
    \+ member(X,LA),
    M1 is M + 1,
    shortest_dfsAux(M1, N, X,Dest,[X|LA],Path).


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

% safest route

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


% player suggestion

:-dynamic suggest_currentPath/2.

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


% strongest route

:-dynamic strongest_currentRoute/2.

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
	get_time(Initial_Time),
    (strongest_findRoute(Orig, Dest); true),
    retract(strongest_currentRoute(StrongestPath, Strength)),
    get_time(End_Time),
    T is End_Time - Initial_Time,
    write('Time:'),write(T),nl,
    write('Strength:'),write(Strength),nl,
    write('Solution path:'),write(StrongestPath),nl.

strongest_findRoute(Orig, Dest):-
    asserta(strongest_currentRoute(_, 0)),
    strongest_dfs(Orig, Dest, Strength, PathList),
    strongest_updateRoute(Strength, PathList),
    fail.

strongest_updateRoute(Strength, PathList):-
    strongest_currentRoute(_, Current_Strength),
    Strength > Current_Strength, retract(strongest_currentRoute(_,_)),
    asserta(strongest_currentRoute(PathList, Strength)).


% common_tags

:- dynamic common_tags_users/2.

common_tags(X,List_Result):-
    get_time(T1),
    common_tags_get_all_tags(All_TagsT),
	common_tags_change_to_synonyms(All_TagsT, All_Tags),
    findall(Combination,common_tags_combination(X,All_Tags,Combination),Combinations),
    findall(UserId,node(UserId,_,_),Users),
    common_tags_users_combination(X,Users,Combinations),
    findall([Comb,ListUsers],common_tags_users(Comb,ListUsers),List_Result),
    retractall(common_tags_users(_,_)),
    write('Solution found in '),
    get_time(T2),
    T is T2-T1,write(T),write(' seconds'),nl.

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


% getNetworkByLevel

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
    (connection(Act,X,_,_, _, _);connection(X,Act,_,_, _, _)),
    \+userVisited(X),
    \+ member(X,LA),
    Level1 is Level-1,
    asserta(userVisited(X)),
    dfs2(X,Level1,[X|LA]).


%======== A-Star (Core) ========%

aStar_find(Mode, Threshold, Orig, Dest, Path, Cost):-
    (retract(aStar_orderedList(_));true),
    aStar_getStrengthListByPlayer(Mode, Threshold, Orig, StrengthList),
    asserta(aStar_orderedList(StrengthList)),
    aStar_aux(Mode, 0, Threshold, Dest,[(_,0,[Orig])],Path,Cost).
aStar_aux(_, M, N, Dest,[(_,Cost,[Dest|T])|_],Path,Cost):- M >= N,reverse([Dest|T],Path).
aStar_aux(0, M, N, Dest,[(_,Ca,LA)|Others],Path,Cost):-
    LA=[Act|_],
    findall((CEX,CaX,[X|LA]),
    (Dest\==Act,
    (connection(Act,X,CostX, _, _, _);
    connection(X, Act, _, CostX, _, _)),
    \+ member(X,LA),
    CaX is CostX + Ca,
    aStar_estimate(N,M,EstX),
    CEX is CaX + EstX),
    New),
    append(Others,New,All),
    sort(All,AllOrd),
    M1 is M + 1,
    aStar_aux(0, M1, N, Dest,AllOrd,Path,Cost).

aStar_aux(1, M, N, Dest,[(_,Ca,LA)|Others],Path,Cost):-
    LA=[Act|_],
    findall((CEX,CaX,[X|LA]),
    (Dest\==Act,
    (connection(Act,X,ConnStrength, _, RelStrength, _);
    connection(X, Act, _, ConnStrength, _, RelStrength)),
    \+ member(X,LA),
    getMulticriteria(ConnStrength, RelStrength, CostX),
    CaX is CostX + Ca,
    aStar_estimate(N,M,EstX),
    CEX is CaX + EstX),
    New),
    append(Others,New,All),
    sort(All,AllOrd),
    M1 is M + 1,
    aStar_aux(1, M1, N, Dest,AllOrd,Path,Cost).


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
