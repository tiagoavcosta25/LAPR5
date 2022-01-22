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

synonym(tecnologia, jogos).
synonym(teatro, musica).


connection(1,11,10,8, 1, 1).
connection(1,12,2,6, 1, 1).
connection(1,13,-3,-2, 1, 1).
connection(1,14,1,-5, 1, 1).
connection(11,21,5,7, 1, 1).
connection(11,22,2,-4, 1, 1).
connection(11,23,-2,8, 1, 1).
connection(11,24,6,0, 1, 1).
connection(12,21,4,9, 1, 1).
connection(12,22,-3,-8, 1, 1).
connection(12,23,2,4, 1, 1).
connection(12,24,-2,4, 1, 1).
connection(13,21,3,2, 1, 1).
connection(13,22,0,-3, 1, 1).
connection(13,23,5,9, 1, 1).
connection(13,24,-2, 4, 1, 1).
connection(14,21,2,6, 1, 1).
connection(14,22,6,-3, 1, 1).
connection(14,23,7,0, 1, 1).
connection(14,24,2,2, 1, 1).
connection(21,31,2,1, 1, 1).
connection(21,32,-2,3, 1, 1).
connection(21,33,3,5, 1, 1).
connection(21,34,4,2, 1, 1).
connection(22,31,5,-4, 1, 1).
connection(22,32,-1,6, 1, 1).
connection(22,33,2,1, 1, 1).
connection(22,34,2,3, 1, 1).
connection(23,31,4,-3, 1, 1).
connection(23,32,3,5, 1, 1).
connection(23,33,4,1, 1, 1).
connection(23,34,-2,-3, 1, 1).
connection(24,31,1,-5, 1, 1).
connection(24,32,1,0, 1, 1).
connection(24,33,3,-1, 1, 1).
connection(24,34,-1,5, 1, 1).
connection(31,41,2,4, 1, 1).
connection(31,42,6,3, 1, 1).
connection(31,43,2,1, 1, 1).
connection(31,44,2,1, 1, 1).
connection(32,41,2,3, 1, 1).
connection(32,42,-1,0, 1, 1).
connection(32,43,0,1, 1, 1).
connection(32,44,1,2, 1, 1).
connection(33,41,4,-1, 1, 1).
connection(33,42,-1,3, 1, 1).
connection(33,43,7,2, 1, 1).
connection(33,44,5,-3, 1, 1).
connection(34,41,3,2, 1, 1).
connection(34,42,1,-1, 1, 1).
connection(34,43,2,4, 1, 1).
connection(34,44,1,-2, 1, 1).
connection(41,200,2,0, 1, 1).
connection(42,200,7,-2, 1, 1).
connection(43,200,-2,4, 1, 1).
connection(44,200,-1,-3, 1, 1).

connection(1,51,6,2, 1, 1).
connection(51,61,7,3, 1, 1).
connection(61,200,2,4, 1, 1).

occ(1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5).

hope(1, 21).
fear(1, 31).
fear(1, 41).

% Secundary knowledge base
:- dynamic shortest_currentRoute/2.
:- dynamic safest_currentRoute/2.
:- dynamic aStar_orderedList/1.
:- dynamic occ/7.

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

shortest_dfs(Mode, N, Orig, Dest, Strength, Path):- shortest_dfsAux(Mode, 0, N, Orig, Dest, [Orig], Strength, Path).

shortest_dfsAux(_, _, _, Dest, Dest, LA, 0, Path):- !, reverse(LA, Path).
shortest_dfsAux(_, M, N, _, _, _, _, _):- M >= N, !, false.
shortest_dfsAux(0, M, N, Current, Dest, LA, Strength, Path):-
    (connection(Current, X, StrengthA, _, _, _);
    connection(X, Current, _, StrengthA, _, _)),
    \+ member(X,LA),
    M1 is M + 1,
    shortest_dfsAux(0, M1, N, X,Dest,[X|LA], Strength1, Path),
	Strength is Strength1 + StrengthA.

shortest_dfsAux(1, M, N, Current, Dest, LA, Strength, Path):-
    (connection(Current, X, StrengthA, _, RelA, _);
    connection(X, Current, _, StrengthA, _, RelA)),
    \+ member(X,LA),
    M1 is M + 1,
    shortest_dfsAux(1, M1, N, X,Dest,[X|LA], Strength1, Path),
	getMulticriteria(StrengthA, RelA, FinalStrength),
	Strength is Strength1 + FinalStrength.


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
		(connection(Current, Friend, StrengthA, StrengthB,_,_);
		connection(Friend, Current, StrengthA, StrengthB,_,_)),
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
		(connection(Current, Friend, _, _, _, _);
		connection(Friend, Current, _, _, _, _)),
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
	(connection(CurrentID, FriendID, StrengthA, StrengthB, _, _);
	connection(FriendID, CurrentID, StrengthA, StrengthB, _, _)),
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

:- dynamic common_tags_users/3.

common_tags(Id, NTags,NUsers,TagList,ResultTag,ResultUsers):-
    get_time(T1),
    node(Id,_,All_TagsT),
	common_tags_change_to_synonyms(All_TagsT, All_Tags),
    findall(Combination,common_tags_combination(NTags,All_Tags,Combination),CombinationsTemp),
	common_tags_test_list(CombinationsTemp, TagList, Combinations),
    findall(UserId,node(UserId,_,_),Users),
	asserta(common_tags_users([],[],0)),
    common_tags_users_combination(NTags,NUsers,Users,Combinations),
	common_tags_users(ResultTag,ResultUsers,_),
    retractall(common_tags_users(_,_,_)),
    write('Solution found in '),
    get_time(T2),
    T is T2-T1,write(T),write(' seconds'),nl.

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

emotion_groupSuggestion(PlayerId, TagList, NewHope, NewDeception, NewFear, NewRelief):-
    suggest_playerGroups(PlayerId, TagList, SuggestedGroup),
    emotion_checkHope(PlayerId, SuggestedGroup, NewHope, NewDeception),
    emotion_checkFear(PlayerId, SuggestedGroup, NewFear, NewRelief),
    retract(occ(PlayerId, Joy, Anguish, _, _, _, _)),
    asserta(occ(PlayerId, Joy, Anguish, NewHope, NewDeception, NewFear, NewRelief)).

emotion_checkHope(PlayerId, SuggestedGroup, NewHope, NewDeception):-
    emotion_countHope(PlayerId, SuggestedGroup, 0, Counter),
    occ(PlayerId, _, _, Hope, Deception, _, _),
    length(SuggestedGroup, Length),
    ((Counter > 0, !,
      emotion_increase(Hope, Counter, Length, NewHope),
     emotion_decrease(Deception, Counter, Length, NewDeception));
    (emotion_increase(Deception, Counter, Length, NewDeception),
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
    ((Counter > 0, !,
      emotion_increase(Fear, Counter, Length, NewFear),
     emotion_decrease(Relief, Counter, Length, NewRelief));
    (emotion_increase(Relief, Counter, Length, NewRelief),
    emotion_decrease(Fear, Counter, Length, NewFear))).

emotion_countFear(_, [], Counter, Return):- Return is Counter.
emotion_countFear(PlayerId, [H | Group], Counter, Return):-
    fear(PlayerId, H),
    !, Counter1 is Counter + 1,
    emotion_countFear(PlayerId, Group, Counter1, Return).
emotion_countFear(PlayerId, [_ | Group], Counter, Return):-
    !,emotion_countFear(PlayerId, Group, Counter, Return).

suggest_playerGroups(_, _, [3,6, 4, 5]).
