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

% safest route

:-dynamic safest_current_route/2.

dfs_safest_route(Orig,Dest,Threshold, Strength, Path):-dfs2_safest_route(Orig,Dest,[Orig],Threshold, 0, Strength, Path).

dfs2_safest_route(Dest,Dest,LA, _, S,S, Cam):-!,reverse(LA,Cam).
dfs2_safest_route(Act,Dest,LA,Threshold,Strength, ReturnStrength,Cam):-node(NAct,Act,_),(connection(NAct,NX,N_StrengthA,N_StrengthB);connection(NX,NAct,N_StrengthA,N_StrengthB)),
    node(NX,X,_),\+ member(X,LA),N_StrengthA > Threshold,
    N_StrengthB > Threshold, FinalStrength1 is N_StrengthA + N_StrengthB,FinalStrength is Strength + FinalStrength1, dfs2_safest_route(X,Dest,[X|LA], Threshold, FinalStrength, ReturnStrength,Cam).


safest_route(Orig,Dest,Threshold, SafestPath):-
		get_time(Initial_Time),
		(find_safest_route(Orig,Dest, Threshold);true),
		retract(safest_current_route(SafestPath,Strength)),
		get_time(End_Time),
		T is End_Time - Initial_Time,
		write('Time:'),write(T),nl,
		write('Strength: '), write(Strength),nl.

find_safest_route(Orig,Dest,Threshold):-
		asserta(safest_current_route(_,-10000)),
		dfs_safest_route(Orig,Dest,Threshold, Strength, PathList),
		update_safest_route(Strength, PathList),
		fail.

update_safest_route(Strength, PathList):-
		safest_current_route(_,Current_Strength),
		Strength > Current_Strength,retract(safest_current_route(_,_)),
		asserta(safest_current_route(PathList,Strength)).
