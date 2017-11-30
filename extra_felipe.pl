
% baseado nas arvores genericas e em grafos
% tem que carregar os dois na base antes de rodar esse
%

%arco(ge, a, b).
%arco(ge, b, c).
%arco(ge, c, d).
%arco(ge, a, e).
%arco(ge, e, d).
%arco(ge, f, d).

%nodo(ge, a, 20).
%nodo(ge, b, 10).
%nodo(ge, c, 5).
%nodo(ge, d, 13).
%nodo(ge, e, 7).
%nodo(ge, f, 17).

nodo(def, a, 1).
arco(def, a, b).

% arv(objint(root, par), [arv(objint(root2, seq), [arv(objterm(left,[right], 540, 23), []), arv(objterm(right, [], 324, 14), [])])])


% arv(objint(root, par), [
%	arv(objint(root2, seq), [
%		arv(objterm(t1,[t2, t4], 540, 23), []),
%		arv(objterm(t2, [t3], 324, 14), [])
%	]),
%	arv(objterm(t3,[], 50, 11), []),
%	arv(objterm(t4,[t3], 150, 142), [])
% ])

extrai_grafo_aon(AG, G):-
    getObjterm(AG, LOT),
    addArcosList(LOT, G).

addArcosList([],_).
addArcosList([H|T], G):-
    H = objterm(O, R, _, D),
    addArcoRecursoList(G, O, R),
    asserta(nodo(G, O, D)),
    addArcosList(T, G).

addArcoRecursoList(_,_,[]).
addArcoRecursoList(G, O, [H|T]):-
    (
        arco(G, O, H);
        asserta(arco(G, O, H))
    ),
    addArcoRecursoList(G, O, T).


% caminho critico
%


caminho_critico(G, L):-
    getStartArcosList(G, SL),
    getEndArcosList(G, EL),
    getAllPathsList(G, SL, EL, [], L1),
    getCriticalPath(G, L1, [], 0, L).

getCriticalPath(_,[],L,_,L).
getCriticalPath(G, [H|T], LA, SA, L):-
    prazo_critico(G, H, R),
    (
        (
            R > SA,
            LR = H,
            SR is R
        );
        (
            LR = LA,
            SR = SA
        )
    ),
    getCriticalPath(G, T, LR, SR, L).

getAllPathsList(_,[],_,E, E).
getAllPathsList(G, [H|T], EL, E, AP):-
    getAllPaths(G, H, EL, E, AP1),
    getAllPathsList(G, T, EL, AP1, AP).

getAllPaths(_,_,[], E, E).
getAllPaths(G, S, [H|T], E, AP):-
    getPaths(G, S, H, L),
    concatena(L, E, LE),
    getAllPaths(G, S, T, LE, AP).

getStartArco(G, X):-
    arco(G, X, _),
    \+arco(G, _, X).

getStartArcosList(G, L):-
    bagof(X, getStartArco(G, X), L1),
    sort(L1, L).

getEndArco(G, X):-
    arco(G, _, X),
    \+arco(G, X, _).

getEndArcosList(G, L):-
    bagof(X, getEndArco(G, X), L1),
    sort(L1, L).

getPaths(G, N1, N2, L):-
    bagof(X, getListPaths(G, N1, N2, X), L).

getListPaths(G, N1, N2, L):-
    paths(G, N1, N2, [N1], L).

paths(G, N1, N2, E, R):-
    arco(G, N1, N2), concatena(E,[N2], R);
    (
        arco(G, N1, Aux),
        noContains(E, Aux),
        concatena(E,[Aux], E2),
        paths(G, Aux, N2, E2, R)
    ).


% prazo caminho critico
%

prazo_caminho_critico(G, P):-
    caminho_critico(G, L),
    prazo_critico(G, L, P).

prazo_critico(G, L, P):-
    prazoList(G, L, 0, P).

prazoList(_,[], E, E).
prazoList(G, [H|T], E, P):-
    (   nodo(G, H, D); D is 0),
    X1 is D+E,
    prazoList(G, T, X1, P).
