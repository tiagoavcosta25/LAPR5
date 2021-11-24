% Library
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

% HTTP Request Relations
:- http_handler('/register_user', register_user, []).

% HTTP Server Creation on port 'Port'
server(Port) :-
        http_server(http_dispatch, [port(Port)]).

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
