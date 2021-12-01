% Bibliotecas HTTP
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_cors)).
:- use_module(library(date)).
:- use_module(library(random)).

% Bibliotecas JSON
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).



%% Base Conhecimento principal

:- dynamic no/2.
:- dynamic ligacoes/4.



% Gerir servidor
startServer(Port) :-						
    http_server(http_dispatch, [port(Port)]),
    asserta(port(Port)).

stopServer:-
    retract(port(Port)),
    http_stop_server(Port,_).


%Cors
:- set_setting(http:cors, [*]).

% Handlers 

%Porta default para o server
default_server_port(5000).

%url da API

obter_users_url("https://localhost:5001/api/users").
obter_ligacoes_url("http://localhost:5000/api/Connections/bidirecional").

adicionarClientes():-
    obterUsersRegistados(Data),
    parse_users(Data).

%% Obter users Registados (Data nao convertida)

obterUsersRegistados(Data):-
    obter_users_url(URL),
    setup_call_cleanup(
        http_open(URL, In, [ cert_verify_hook(cert_accept_any)]),
        json_read_dict(In, Data),
        close(In)
).    

%% Convertes Json de todos Users em Lista

parse_users([]).
parse_users([H|List]):-
    asserta(no(H.get(id),H.get(interestTags))),
    parse_users(List).


%% Colocar as ligacoes da api dinamicamente no prolog
adicionarLigacoes():-
    obterLigacoes(Data),
    parse_ligacoes(Data).


obterLigacoes(Data):-
    obter_ligacoes_url(URL),
    setup_call_cleanup(
        http_open(URL, In, [ cert_verify_hook(cert_accept_any)]),
        json_read_dict(In, Data),
        close(In)
).   

parse_ligacoes([]).
parse_ligacoes([H|List]):-
    asserta(ligacoes(H.get(oUser),H.get(dUser),H.get(connectionStrengthOUser),H.get(connectionStrengthDUser))),
    parse_ligacoes(List).


%Métodos que carregam os dados para sistema
carregar_dados_sistema:-
    adicionarClientes(),
    adicionarLigacoes().

inicializar_server:-
    default_server_port(Port),
    startServer(Port),!,
    carregar_dados_sistema,!.

:-inicializar_server.
