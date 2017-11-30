

%arco(g1, b, a).
arco(g1, a, c).
arco(g1, c, g).
%arco(g1, a, e).
%arco(g1, e, b).
%arco(g1, e, d).
%arco(g1, f, e).
%arco(g1, g, f).

nodos_do_grafo(G, L):-
    nodosGrafo(G, [], L).

%nodosGrafo(_, [], L):-
%    var(L) -> L = [].
nodosGrafo(G, E, [N|L]):-
    arco(G, N, _),
    dontHasNode(E, N), !,
    nodosGrafo(G, [N|E], L).

dontHasNode([], _).
dontHasNode([H|T], N):-
    H \= N,
    dontHasNode(T, N).
