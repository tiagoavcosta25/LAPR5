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
:- dynamic strongest_currentRoute/2.


% HTTP Server setup at 'Port'                           
startServer(Port) :-   
        http_server(http_dispatch, [port(Port)]),
        asserta(port(Port)).
		
% Cors setup
:- set_setting(http:cors, [*]).

% Server startup
start_server:-
    consult(ai_http_config),  % Loads server's configuration
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

%======== Strongest path between two players ========%

:- http_handler('/api/strongest_route', strongest_route_compute, []).

strongest_route_compute(Request) :-
	cors_enable(Request, [methods([get])]),
    strongest_route_prepare(Request, Path),
	prolog_to_json(Path, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).

strongest_route_prepare(Request, Path) :-
    http_parameters(Request, [emailPlayer(EmailPlayer, [string]), emailTarget(EmailTarget, [string])]),
	addPlayers(),
	addConnections(),
	getPlayerName(EmailPlayer, PlayerName),
	getPlayerName(EmailTarget, TargetName),	
	strongest_route(PlayerName, TargetName, Path),
	retractall(connection(_,_,_,_)),
	retractall(node(_,_,_)).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    (strongest_findRoute(Orig, Dest); true),
    retract(strongest_currentRoute(StrongestPath, _)).

strongest_findRoute(Orig, Dest):-
    asserta(strongest_currentRoute(_, 0)),
    strongest_dfs(Orig, Dest, Strength, PathList),
    strongest_updateRoute(Strength, PathList),
    fail.

strongest_updateRoute(Strength, PathList):-
    strongest_currentRoute(_, Current_Strength),
    Strength > Current_Strength, retract(strongest_currentRoute(_,_)),
    asserta(strongest_currentRoute(PathList, Strength)).


%====================================================%