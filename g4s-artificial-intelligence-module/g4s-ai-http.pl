% Library
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

% HTTP Request Relations
:- http_handler('/register_user', register_user, []).

% HTTP Server Creation on port 'Port'
server(Port) :-
        http_server(http_dispatch, [port(Port)]).


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
dfs2(Act,Dest,LA,Path):-node(NAct,Act,_),(connection(NAct,NX,_,_);connection(NX,NAct,_,_)),
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
