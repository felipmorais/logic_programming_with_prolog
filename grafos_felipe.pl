
% grafos
%

arco(g1, b, a).
arco(g1, a, c).
arco(g1, a, e).
arco(g1, c, g).
arco(g1, e, b).
arco(g1, e, d).
arco(g1, f, e).
arco(g1, g, f).
arco(g1, d, g).
%arco(g1, d, b).

arco(ga, a, b).
arco(ga, a, c).
arco(ga, b, c).
arco(ga, c, d).

arco(gb, 1, 2).
arco(gb, 3, 1).
arco(gb, 2, 3).
arco(gb, 1, 4).

arco(gc, 1, 2).
arco(gc, 2, 3).
arco(gc, 3, 4).
arco(gc, 4, 1).


arco(k5,a,b).
arco(k5,a,c).
arco(k5,a,d).
arco(k5,a,e).
arco(k5,b,c).
arco(k5,b,d).
arco(k5,b,e).
arco(k5,c,d).
arco(k5,c,e).
arco(k5,d,e).

arco(k33, a, d).
arco(k33, a, e).
arco(k33, a, f).
arco(k33, b, d).
arco(k33, b, e).
arco(k33, b, f).
arco(k33, c, d).
arco(k33, c, e).
arco(k33, c, f).

equiv(def1, [], def2, []).

arco_bi(G, X, Y):-
    arco(G, X, Y);
    arco(G, Y, X).

% nodos_do_grafo(G, L) – verdadeiro se L é a lista de nodos do grafo G.
%


nodos_do_grafo(G, L):-
    findall(N1, arco(G, N1, _), L1),
    findall(N2, arco(G, _, N2), L2),
    concatena(L1, L2, L3),
    sort(L3, L4),
    (
        var(L) -> L = L4;
        (
            sort(L, L5),
            L5 = L4
        )
    ).

concatena([], L2, L2).
concatena([H|L1], L2, [H|L3]):-
	concatena(L1, L2, L3).

% conectado(G,N1,N2) – verdadeiro se o nodo N1 está conectado ao nodo N2 de forma diretaou indireta no grafo G.
%
conectado(G, N1, N2):-
    conect(G, N1, N2, []).

conect(G, N1, N2, E):-
    arco(G, N1, N2);
    (
        arco(G, N1, Aux),
        noContains(E, [N1, Aux]),
        conect(G, Aux, N2, [[N1, Aux]|E])
    ).


%  ciclo(G,N) – verdadeiro se há um ciclo no grafo G que parte e
%  retorna ao nodo N.
%

ciclo(G, N):-
    conectado(G, N, N).


% grafo_ciclico(G) – verdadeiro se há um ciclo no grafo G.
%

grafo_ciclico(G):-
    nodos_do_grafo(G, L),
    grafo_ciclico_list(G, L).

grafo_ciclico_list(_,[]):- false.
grafo_ciclico_list(G, [H|T]):-
    ciclo(G, H);
    grafo_ciclico_list(G, T).


% caminho(G, N1,N2,C) – verdadeiro se C é uma lista de nodos que forma
% um caminho do nodo N1 ao nodo N2 dentro do grafo G.
%

caminho(G, N1, N2, [N2]):- arco(G, N1, N2).
caminho(G, N1, N2, [H|T]):-
    N1 == H, caminho(G, H, N2, T);
    arco(G, N1, H),
    caminho(G, H, N2, T).

%desconsidera o N1 e N2 na lista
%caminho(G, N1, N2, []):- arco(G, N1, N2).
%caminho(G, N1, N2, [H|T]):-
%    arco(G, N1, H),
%    caminho(G, H, N2, T).



% caminho_mais_curto(G, N1,N2,C) - verdadeiro se C é uma lista de nodos
% que forma o caminho mais curto do nodo N1 ao nodo N2 dentro do grafo
% G.

caminho_mais_curto(G, N1, N2, C):-
    camin(G, N1, N2, C).

camin(G, N1, N2, [N2]):- arco(G, N1, N2).
camin(G, N1, N2, [N1|C]):-
    % arco(G, N1, N2);
    (
        findall(X, arco(G, N1, X), L),
        caminList(G, L, N2, C)
    );
    (
        findall(X, arco(G, N1, X), L),
        caminListNode(G, L, N2, C)
    ).

caminList(_, [],_,_):- false.
caminList(G, [H|T], N2, C):-
    arco(G, H, N2), C = [H|[N2]];
    caminList(G, T, N2, C).

caminListNode(_, [],_,_):- false.
caminListNode(G, [H|T], N2, C):-
    camin(G, H, N2, C);
    caminListNode(G, T, N2, C).


% hamiltoniano(G) – verdadeiro se o grafo G tem um ciclo Hamiltoniano.
%

hamiltoniano(G):-
    nodos_do_grafo(G, L),
    hamiltonianoList(G, L).

hamiltonianoList(_, []):- false.
hamiltonianoList(G, [H|T]):-
    hamiltonFirst(G, H, []);
    hamiltonianoList(G, T).

hamiltonFirst(G, N, E):-
    hamilton(G, N, E, L),
    writeln(L),
    getLast(L, X),
    arco(G, X, N),
    nodos_do_grafo(G, L).

hamilton(G, N, E, [N|R]):-
    noContains(E, N),
    findall(X, arco(G, N, X), L),
    hamiltonList(G, L, [N|E], R).

hamiltonList(_,[],_,[]).
hamiltonList(G, [H|T], E, R):-
    hamilton(G, H, E, R);
    hamiltonList(G, T, E, R).

noContains([], _).
noContains([H|T], E):-
    H \== E,
    noContains(T, E).

getLast([H|T], L):-
    T == [], L = H;
    getLast(T, L).


% euleriano(G) – verdadeiro se o grafo G tem um ciclo Euleriano
%

arestas_do_grafo(G, L):-
    findall([X,Y], arco(G, X, Y), L1),
    sort(L1, L).

euleriano(G):-
    arestas_do_grafo(G, L),
    eulerianoList(G, L).

eulerianoList(_, []):- false.
eulerianoList(G, [H|T]):-
    eulerianoFirst(G, H, []);
    eulerianoList(G, T).

eulerianoFirst(G, N, E):-
    euler(G, N, E, L),
    writeln(L),
    getLast(L, X),
    connectedArcos(X, N),
    sort(L, L1),
    arestas_do_grafo(G, L1).

euler(G, [X,Y], E, [[X,Y]|R]):-
    noContains(E, [X,Y]),
    findall([Y,Y2], arco(G, Y, Y2), L),
    eulerList(G, L, [[X,Y]|E], R).

eulerList(_,[],_,[]).
eulerList(G, [H|T], E, R):-
    euler(G, H, E, R);
    eulerList(G, T, E, R).


connectedArcos([_,Y], [X,_]):-
    Y == X.

% planar(G) – verdadeiro se o grafo G é planar.
%

planar(G):-
    \+isK5(G),
    \+isK33(G).

hasPath(G, N1, N2):-
    arco_bi(G, N1, N2).

isK5(G):-
    hasPath(G, A, B),
    hasPath(G, A, C),
    hasPath(G, A, D),
    hasPath(G, A, E),
    hasPath(G, B, C),
    hasPath(G, B, D),
    hasPath(G, B, E),
    hasPath(G, C, D),
    hasPath(G, C, E),
    hasPath(G, D, E),
    A \= B,
    A \= C,
    A \= D,
    A \= E,
    B \= C,
    B \= D,
    B \= E,
    C \= D,
    C \= E,
    D \= E.


isK33(G):-
    hasPath(G, A, D),
    hasPath(G, A, E),
    hasPath(G, A, F),
    hasPath(G, B, D),
    hasPath(G, B, E),
    hasPath(G, B, F),
    hasPath(G, C, D),
    hasPath(G, C, E),
    hasPath(G, C, F),
    A \= B,
    A \= C,
    A \= D,
    A \= E,
    A \= F,
    B \= C,
    B \= D,
    B \= E,
    B \= F,
    C \= D,
    C \= E,
    C \= F,
    D \= E,
    D \= F,
    E \= F.


% isomorfos(G1,G2) – verdadeiro se os grafos G1 e G2 são isomórficos.
%

isomorfos(G1, G2):-
    nodos_do_grafo(G1, N1),
    nodos_do_grafo(G2, N2),
    length(N1, LN1),
    length(N2, LN2),
    LN1 == LN2,
    arestas_do_grafo(G1, A1),
    arestas_do_grafo(G2, A2),
    length(A1, LA1),
    length(A2, LA2),
    LA1 == LA2,
    iso(G1, N1, G2, N2).

iso(G1, N1, G2, N2):-
    permutation(N2, PN2),
    writeln(PN2),
    retractall(equiv(G1,_,G2,_)),
    addEquiv(G1, N1, G2, PN2),
    verifyEquiv(G1, N1, G2).

verifyEquiv(_,[],_).
verifyEquiv(G1, [H|T], G2):-
    (
        findall(X, arco_bi(G1, H, X), L1),
        getEquiv(G1, H, G2, N2),
        findall(Y, arco_bi(G2, N2, Y), L2),
        compareEquivList(G1, L1, G2, L2)
    ),
    verifyEquiv(G1, T, G2).

addEquiv(_,[],_,[]).
addEquiv(G1, [H1|T1], G2, [H2|T2]):-
    asserta(equiv(G1, H1, G2, H2)),
    addEquiv(G1, T1, G2, T2).

getEquiv(G1, N1, G2, N2):-
    equiv(G1, N1, G2, N2).

getEquivList(_,[],_,[]).
getEquivList(G1, [H|T], G2, [N2|R]):-
    getEquiv(G1, H, G2, N2),
    getEquivList(G1, T, G2, R).

compareEquivList(G1, L1, G2, L2):-
    getEquivList(G1, L1, G2, LR),
    %writeln(L1 + L2 + LR),
    sort(LR, LRS),
    sort(L2, L2S),
    LRS == L2S.
