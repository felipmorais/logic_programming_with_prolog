
% (a) indice(E,I,J,T) – verdadeiro se o elemento E está na linha I e coluna J da tabela T, falso
% caso contrário.

indice(E, I, J, T):-
    ind(E, I, J, T).

ind(E, 0, 0, [E|_]):-!.

ind(E, 0, J, [L|_]):-
    ind(E, 0, J, L).

ind(E, 0, J, [_|T]):-
     C is J-1,
    ind(E, 0, C, T).


ind(E, I, J, [_|T]):-
    L is I-1,
    ind(E, L, J, T).


% (b) altera(E, T1, I, J, T2) – verdadeiro se a tabela T1 for igual a tabela T1, exceto
% (possivelmente) pela posição que está na linha I e coluna J, que deverá ser igual ao elemento E. Este predicado efetivamente altera o elemento armazenado na posição que está na linha I e coluna J da tabela T1, resultando na tabela T2.

altera(E, T1, I, J, T2):-
    alt(E, T1, I, J, T2).


alt(E, [H|T], 0, J, [T2|T]):-
    alt(E, H, 0, J, T2).

alt(E, [_|T], 0, 0, [E|T]).

alt(E, [H|T], 0, J, [H|T2]):-
    C is J-1,
    alt(E, T, 0, C, T2).

alt(E, [H|T], I, J, [H|T2]):-
    L is I-1,
    alt(E, T, L, J, T2).


% (c) transposta(M1,M2) – verdadeira se M1 e M2 são matrizes e M2 é a matriz transposta de M1, falso caso contrário. Por exemplo, supondo M1 = [[a1, a2], [b1, b2], [b1, b2]], então transposta(M1,M2) deve resultar na unificação M2=[[a1, b1, c1], [a2, c2, b2]].
%
% M1 = [[a1, a2], [b1, b2], [b1, b2]]
% M2=[[a1, b1, c1], [a2, c2, b2]].
%
transposta(M1, M2):-
    trans(M1, M2).

trans([[]|_], []).
trans(M1, [F|C]) :-
    translin(M1, F, M2),
    trans(M2, C).

translin([], [], []).
translin([[M1|M1C]|FC], [M1|R], [M1C|M2]) :-
    translin(FC, R, M2).

% (d) insere_lin(T1, I, L,T2) – verdadeiro se a linha L for inserida na linha de índice I na tabela T1, resultando na tabela T2. Falso caso contrário. Note que, da mesma forma que nocaso do predicado insere_elem(), este predicado também deverá poder ser usado para eliminar uma linha na posição de índice I da tabela T2, se esta tabela for dada como fixa, enquanto T1 e L estiverem livres.
%

insere_lin(T1, I, L, T2):-
    ins(T1, I, L, T2).

ins(T1, 0, L, [L|T1]).
ins([H|T], I, L, [H|T2]):-
    X is I-1,
    ins(T, X, L, T2).


