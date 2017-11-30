

% (a) percurso_pre(A, L) – verdadeiro se A é uma árvore e L é uma lista que contém os elementos armazenados em cada nodo da árvore A ordenada na forma de um percurso pré- ordem da árvore A. Percursos pré-ordem são definidos recursivamente da seguinte maneira:- Visite a raiz da árvore- Percorra o ramo (subárvore) da esquerda em pré-ordem - Percorra o ramo (subárvore) da direita em pré-ordem
%

% arvbin(V, E, D).
% A1:- arvbin(*,
%           arvbin(-,
%                 arvbin(x, nil, nil),
%                 nil),
%           arvbin(+,
%                 arvbin(y, nil, nil),
%                 arvbin(2, nil, nil))).
%
% arvbin(*, arvbin(-, nil, arvbin(x, nil, nil)), arvbin(+, arvbin(y, nil, nil), arvbin(2,
% nil, nil)))
%

isBinTree(nil).
isBinTree(arvbin(_, E, D)) :-
    isBinTree(E),
    isBinTree(D).

% [*, -, x, +, y, 2]
percurso_pre(A, L):-
    isBinTree(A),
    perpre(A, L).

perpre(nil, []).
perpre(arvbin(V, E, D), [V | L]):-
    perpre(E, L1),
    perpre(D, L2),
    concatena(L1, L2, L).

concatena([], L2, L2).
concatena([H|L1], L2, [H|L3]):-
	concatena(L1, L2, L3).



% (b) percurso_pos(A, L) – verdadeiro se A é uma árvore e L é uma lista que contém os elementos armazenados em cada nodo da árvore A ordenada na forma de um percurso pós- ordem da árvore A. Percursos pós-ordem são definidos recursivamente da seguinte maneira:- Percorra o ramo (subárvore) da esquerda em pós-ordem - Percorra o ramo (subárvore) da direita em pós-ordem- Visite a raiz da árvore
%

% [x, -, y, 2, +, *]
percurso_pos(A, L):-
    isBinTree(A),
    perpos(A, L).

perpos(nil, []).
perpos(arvbin(V, E, D), L):-
    perpos(E, L1),
    perpos(D, L2),
    concatena(L1, L2, L3),
    addlast(L3, V, L).


addlast([], E, [E]).
addlast([H|T], E, [H|L2]):-
    addlast(T, E, L2).



% (c) percurso_sim(A, L) – verdadeiro se A é uma árvore e L é uma lista que contém os elementos armazenados em cada nodo da árvore A ordenada na forma de um percursosimétrico da árvore A. Percursos simétricos são definidos recursivamente da seguinte maneira:
% - Percorra o ramo (subárvore) da esquerda em ordem simétrica
% - Visite a raiz da árvore
% - Percorra o ramo (subárvore) da direita em ordem simétrica
%

% [x, -, *, y, +, 2]
percurso_sim(A, L):-
    isBinTree(A),
    persim(A, L).

persim(nil, []).
persim(arvbin(V, E, D), L):-
    persim(E, L1),
    addlast(L1, V, L3),
    persim(D, L2),
    concatena(L3, L2, L).

% questao d val(X, Y) onde L = [val(x, 10), val(y, -3), val(a, 2.3)]
% fazer o calc(L, A, V), onde L foi definido acima,
%  A eh uma arvore de expressoes
%  V eh o valor numerico calculado para a expressao
%

isList([]).
isList([H|T]):-
    H = val(_, _),
    isList(T).

calc(L, A, V):-
    isList(L),
    isBinTree(A),
    convertTree(A, L, AR),
    calculate(AR, V).

calculate(nil, 0).
calculate(arvbin(V, nil, nil), V).
calculate(arvbin(V, E, D), R):-
    calculate(E, R1),
    calculate(D, R2),
    operate(V, R1, R2, R).

operate(O, V1, V2, R):-
    (   O = +, R is V1 + V2);
    (   O = -, R is V1 - V2);
    (   O = *, R is V1 * V2);
    (   O = /, R is V1 / V2);
    (   O = ^, R is V1 ^ V2).

convertTree(nil, _, nil).
convertTree(arvbin(V, E, D), L, arvbin(N, ARE, ARD)):-
    (isOperation(V), N = V;
    number(V), N = V;
    isVariable(L, V, N)),
    convertTree(E, L, ARE),
    convertTree(D, L, ARD).


isVariable([], _, _):- false.
isVariable([H|T], V, N):-
    H = val(V, N);
    isVariable(T, V, N).

isOperation(V):-
    V = +;
    V = -;
    V = *;
    V = /;
    V = '^'.


% questão e, arvore binaria de busca balanceada
% Ve eh menor que V
% Vd eh maior que V
% V pode ser comparado e posto em ordem especifica
% insere_abb(A1, V, A2)
% A1 arvore binaria de busca, pode ser nil
% V eh um valor
% A2 arvore binaria de busca resultante da insercao de V em A1

