/* 3. Árvores Binárias em Prolog */

testeInsere(AFinal):-
    insereabb(nil, 1, A10),
    insereabb(A10 , 2, A9),
    insereabb(A9 , 3, A8),
    insereabb(A8 , 4, A7),
    insereabb(A7 , 5, A6),
    insereabb(A6 , 6, A5),
    insereabb(A5 , 7, A4),
    insereabb(A4 , 8, A3),
    insereabb(A3 , 9, A2),
    insereabb(A2 , 10, A1),
    AFinal = A1.



teste(V):-
    A1 = arvbin(*, arvbin(-, arvbin(5, nil, nil), nil), arvbin(+, arvbin(x, nil, nil), arvbin(2, nil, nil) ) ),
    writef('%w', [A1]),nl,
    calc([val(x, 7), val(y, -3), val(a, 2.3)], A1, V).

/*(a) percurso_pre(A, L) – verdadeiro se A é uma árvore e L é uma lista que contém os elementos armazenados em cada nodo da árvore A ordenada na forma de um percurso pré-
ordem da árvore A. Percursos pré-ordem são definidos recursivamente da seguinte maneira:
- Visite a raiz da árvore
- Percorra o ramo (subárvore) da esquerda em pré-ordem
- Percorra o ramo (subárvore) da direita em pré-ordem*/

percurso_pre(A, L):-
    isarvore(A),
    perpre(A, L).

perpre(nil, []).
perpre(arvbin(V, E, D), [V | L]):-
    perpre(E, L1),
    perpre(D, L2),
    concat(L1, L2, L).

isarvore(nil).
isarvore(arvbin(_,L,R)) :- isarvore(L), isarvore(R).

concat([], L, L).
concat([X | L1], L2, [X | L3]):-
        concat(L1, L2, L3).

/*(b) percurso_pos(A, L) – verdadeiro se A é uma árvore e L é uma lista que contém os elementos armazenados em cada nodo da árvore A ordenada na forma de um percurso pósordem da árvore A. Percursos pós-ordem são definidos recursivamente da seguinte maneira:
- Percorra o ramo (subárvore) da esquerda em pós-ordem
- Percorra o ramo (subárvore) da direita em pós-ordem
- Visite a raiz da árvore*/
percurso_pos(A, L):-
    isarvore(A),
    perpos(A, L).

perpos(nil, []).
perpos(arvbin(V, E, D), L):-
    perpos(E, L1),
    perpos(D, L2),
    concat(L1, L2, L3),
    concat(L3, [V], L).


/*(c) percurso_sim(A, L) – verdadeiro se A é uma árvore e L é uma lista que contém os elementos armazenados em cada nodo da árvore A ordenada na forma de um percurso simétrico da árvore A. Percursos simétricos são definidos recursivamente da seguinte maneira:
- Percorra o ramo (subárvore) da esquerda em ordem simétrica
- Visite a raiz da árvore
- Percorra o ramo (subárvore) da direita em ordem simétrica*/
percurso_sim(A, L):-
    isarvore(A),
    persim(A, L).

persim(nil, []).
persim(arvbin(V, E, D), L):-
    persim(E, L1),
    persim(D, L2),
    concat(L1, [V | L2], L).


/* (d) Suponha que L é uma lista formada por termos com o seguinte formato: val(X,V) onde X são identificadores de variáveis formatos por átomos do Prolog (p.ex. x, y, z, x1, x2, a, b ,c, ...) e V são valores numéricos. Dessa forma a lista L serve como uma memória armazenando os valores de um conjunto de variáveis. Por exemplo, se L fosse igual a lista: L = [ val(x, 10), val(y, -3), val(a, 2.3) ] isso representaria o fato que o valor da variável x é 10, o valor de y é -3 e o valor da variável a é 2.3. Agora implemente o predicado:
calc(L,A,V) onde L é uma lista de valores, tal como definida acima, A é uma árvore de expressões aritméticas e V o valor numérico calculado para esta expressão. Cada nodo da árvore A pode conter:
- um identificador de variável ou
- um valor numérico ou
- um identificador de operação aritmética que pode ser:
'+' (soma), '-' (subtração ou complemento), '*' (multiplicação),
'/' (divisão), '^' (exponenciação) */

calc(L,A,V):-
    calcular(L, A, V).

calcular( _ , nil, nil).
calcular(L, arvbin(V, E, D), Result):-
    calcular(L, E, L1),
    calcular(L, D, L2),
    (   L1=nil
    ->  Result = V
    ;   (valor(L, L1, V1),
        (   complemento(L1, L2)
        ->  (Result is -V1)
        ;   (valor(L, L2, V2),
            aritmetica(V1, V2, V, Result))
        ))
    ).


complemento(L1, L2):-
    L1\=nil,
    L2=nil.

valor(L , Var, Val):-
    (number(Var) -> Val is Var ; memoria(L, Var, Val)).

memoria([val(Variable,Value) | _], Variable, Value).
memoria([_ | C], Variable, Value):-
    memoria(C, Variable, Value).

aritmetica(A, B, + ,Value):- Value is A+B.
aritmetica(A, B, * ,Value):- Value is A*B.
aritmetica(A, B, - ,Value):- Value is A-B.
aritmetica(A, B, / ,Value):- Value is A/B.
aritmetica(A, B, ^ ,Value):- Value is A^B.


/*(e) Uma árvore binária de busca é uma árvore binária, onde os valores contidas em cada nodo podem ser comparados e postos em uma ordem específica (ou seja, existe um operador de comparação que permite saber se um valor é maior, menor ou igual à outro). Além disso para cada valor V de qualquer nodo de uma árvore binária de busca é garantido que todos os valores Ve  contidos nos nodos do ramo (subárvore) esquerdo são menores que V e que todos os valores Vd  contidos nos nodos do ramo (subárvore) direito são maiores que V. Agora implemente o predicado:
- insere_abb(A1, V, A2) onde A1 é uma árvore binária de busca (possivelmente vazia, isto é, pode ser apenas nil), V é um valor e A2 é a arvore binária de busca correspondente à inserção do valor B na posição correta da árvore A1.*/

insereabb(A1, V, A2):-
    insert(A1, V, A2) %Arvore Balanceada
    %,ins(A1, V, A2) %Arvore Não Balanceada
    .

ins(nil, V, arvbin(V, nil, nil)).
ins(arvbin(V1, E1, D1), V, arvbin(V2, E2, D2)):-
    (   V < V1
    ->  ins(E1, V, E2),
        V2 = V1,
        D2 = D1
    ;   V > V1
    ->  ins(D1, V, D2),
        E2 = E1,
        V2 = V1
    ;   E2 = E1,
        V2 = V1,
        D2 = D1
    ).

insert(nil, X, arvbin(X, nil,nil)).
insert(arvbin(Key, E1, D1), X, arvbin(KeyReturn, EReturn, DReturn)):-
    (   X < Key
    ->  (insert(E1, X, Eintermediate),Dintermediate = D1)
    ;   (   X > Key
        ->  (insert(D1, X, Dintermediate),Eintermediate = E1)
        ;   (Eintermediate = E1, Dintermediate = D1)
        )
    )
    ,balance(arvbin(Key, Eintermediate, Dintermediate), arvbin(KeyReturn, EReturn, DReturn))
    %,(EReturn = Eintermediate, KeyReturn = Key, DReturn = Dintermediate)
    .

balance(arvbin(V1, E1, D1), arvbin(Vret, Eret, Dret)):-
        getFactor(arvbin(V1, E1, D1), Factor),
        (   Factor = -2
        ->  rotationRight(arvbin(V1, E1, D1), arvbin(Vret, Eret, Dret))
        ;   (   Factor = 2
            ->  rotationLeft(arvbin(V1, E1, D1), arvbin(Vret, Eret, Dret))

            ;   (Vret = V1, Eret = E1, Dret = D1)
            )
        ).

rotationRight(arvbin(V1, E1, D1), arvbin(Vret, Eret, Dret)):-
             getFactor(E1, Factor),
             (   Factor < 0
             ->  doRightRotation(arvbin(V1, E1, D1), arvbin(Vret, Eret, Dret))
             ;   doDoubleRightRotation(arvbin(V1, E1, D1), arvbin(Vret, Eret, Dret))
             ).

rotationLeft(arvbin(V1, E1, D1), arvbin(Vret, Eret, Dret)):-
             getFactor(D1, Factor),
             (   Factor > 0
             ->  doLeftRotation(arvbin(V1, E1, D1), arvbin(Vret, Eret, Dret))
             ;   doDoubleLeftRotation(arvbin(V1, E1, D1), arvbin(Vret, Eret, Dret))
             ).

doRightRotation(nil, nil).
doRightRotation(arvbin(V1,
                    arvbin(EV1,EE1,ED1),
                    AD
                   ),
                arvbin(V2, E2, D2)):-
    V2 = EV1, E2 = EE1,
    K2 = arvbin(V1, ED1, AD),
    D2 = K2.

doLeftRotation(nil, nil).
doLeftRotation(arvbin(V1,
                    AE,
                    arvbin(DV1,DE1,DD1)
                   ),
                arvbin(V2, E2, D2)):-
    V2 = DV1, D2 = DD1,
    K1 = arvbin(V1, AE, DE1),
    E2 = K1.

doDoubleRightRotation(arvbin(V1, E1, D1), A2):-
    doLeftRotation(E1, Return),
    doRightRotation(arvbin(V1, Return, D1), A2).

doDoubleLeftRotation(arvbin(V1, E1, D1), A2):-
    doRightRotation(D1, Return),
    doLeftRotation(arvbin(V1, E1, Return), A2).

getFactor(nil, 0).
getFactor(arvbin(_, E, D), Valor):-
    getHeight(E, Esquerda),
    getHeight(D, Direita),
    Valor is(Direita - Esquerda).

getHeight(nil, 0).
getHeight(arvbin(_,E,D), Valor):-
    (   E = nil
    ->  Esquerda = 0
    ;   getHeight(E, Esquerda)
    ),
    (   D = nil
    ->  Direita = 0
    ;   getHeight(D, Direita)
    ),
    maior(Esquerda,Direita,Maior),
    Valor is 1 + Maior.

maior(E, D, M):-
    (   E > D
    ->  M = E
    ;   M = D
    ).





























