%% lista de exercicios de Tecnicas de programacao
%% Felipe de Morais

%% (a) pertence(E, L) â verdadeiro se o elemento E pertence a lista L, falso caso contrario.

pertence(E, [E|_]).
pertence(E, [_|T]):-
	pertence(E, T).


%% (b) somatorio(L,S) â verdadeiro se a lista eh formada por valores numericos e se S eh o
%% resultado da soma destes valores.

somatorio([], 0).
somatorio([H|T], S):-
	somatorio(T, X),
	S is H+X.

%% (c) indice(E, L, I) â verdadeiro se o elemento E esta na posicao de indice I da lista L,
%% falso caso contrario. Faca uma implementacao nao-deterministica deste predicado, ou seja,
%% se o indice I eh deixado livre e o elemento E esta fixo (unificado com um valor), este
%% predicado retorna o indice do elemento na lista L, se ele existir na lista, por outro lado,
%% se o elemento E eh livre e o indice I esta fixo entao retorna em E o elemento que esta
%% na posicao I.

indice(E, L, I):-
	var(I) -> achaI(E, L, I);
	achaE(E, L, I).

achaI(E, [E|_], 0):- !.
achaI(E, [_|T], I):-
	achaI(E, T, X),
	I is X+1.

achaE(E, [E|_], 0):- !.
achaE(E, [_|T], I):-
	X is I-1,
	achaE(E, T, X).


% (d) altera(E, L1, I, L2) – verdadeiro se a lista L2 for igual a lista
% L1, exceto (possivelmente) pela posição de índice I, que deverá ser
% igual ao elemento E. Este predicado efetivamente altera o elemento
% armazenado na posição de índice I da lista L1, resultando na lista
% L2.

altera(E,[_|T],0, [E|T]):-!.
altera(E, [H|T], I, [H|L2]):-
	X is I-1,
	altera(E,T,X, L2).

% (e) inverso(L1, L2) – verdadeiro se a lista L2 é o inverso da lista L1
% (ou vice-versa).

inverso(L1, L2):-
	inv(L1, L2).

inv([],[]).
inv([H|T],L2):-
	inv(T,L3),
	concatena(L3,[H],L2).


% (f) concatena(L1, L2, L3) – verdadeiro se a lista L3 é feita pela concatenação
% das listas L1 e L2. Faça uma implementação não-determinística de forma que
% se L1 e L2 estão fixados e L3 está livre, então retorna em L3 a concatenação
% de L1 com L2, por outro lado se L1 (ou L2) está livre e L2 (ou L1) e L3 estão
% fixos, então L2 deve ser sufixo (ou L1 deve ser prefixo) de L3 e L1 retorna o
% prefixo (ou L2 retorna o sufixo) da lista L3.

concatena([], L2, L2).
concatena([H|L1], L2, [H|L3]):-
	concatena(L1, L2, L3).


% (g) insere_elem(L1, E, I, L2) – verdadeiro se o elemento E for inserido na posição
% de índice I da lista L1, resultando na lista L2. Falso caso contrário. Note que
% este predicado também deverá poder ser usado para eliminar um elemento da posição
% de índice I da lista L2, se esta lista for dada como fixa, enquanto L1 e E estiverem livres.

insere_elem(T,E,0,[E|T]).
insere_elem([H|T],E,0,[E|[H|T]]).
insere_elem([H|T], E, I, [H|L2]):-
	X is I-1,
	insere_elem(T, E, X, L2).


% remove([_|T], 0, T).
% remove([H|T], I, [H|T1]):-
	% X is I-1,
	% remove(T, X, T1).




















