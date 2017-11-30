
% arvores genericas - EAP
% arv(V, L)
% V eh um valor armazenado no nodo
% L eh uma lista de nodos filhos
% objetos terminais sao representados como
% objterm(O, R, C, T)
% O identifica o objetivo
% R eh uma lista de recursos
% C eh valor numerico com estimativa de custo para atingir o objeto
% T estimativa em dias do tempo para alcancar o objetivo
% objint(O, SP)
% O identifica o objetivo
% SP identifica se os subojetivo deste objetivo podem ser buscados em
% par - paralalelo ou
% seq - sequancia.
%

% arv(objint(root, par), [arv(objterm(left, [r1, r2], 540, 23), []), arv(objterm(right, [r2, r3], 324, 14), [])])
%
% % arv(objint(root, par), [arv(objint(root2, seq), [arv(objterm(left, [r1, r2], 540, 23), []), arv(objterm(right, [r2, r3], 324, 14), [])])])


isTree(arv(V, L)):-
    (
      ( V = objterm(_, R, C, T),
        ( R = []; R = [_|_] ),
        number(C),
        number(T)
      );
      ( V = objint(_, SP),
        ( SP = par ; SP = seq)
      )
    ),
    isTreeList(L).

isTreeList([]).
isTreeList([H|L]):-
    isTree(H),
    isTreeList(L).


% a) total_recursos(P, R)
% P eh uma arvore generica que representa a EAP
% R eh a lista de recursos unicos de todo o projeto
%

total_recursos(P, R):-
    isTree(P),
    totRec(P, [], R).

totRec(arv(V, L), E, R):-
    (
       V = objterm(_ , R1, _, _),
       addResources(R1, E, R)
    );
    totRecList(L, E, R).

totRecList([], E, E).
totRecList([H|T], E, R):-
    totRec(H, E, R1),
    totRecList(T, R1, R).

addResources([], E, E).
addResources([H|T], E, R):-
    addUnique(H, E, R1),
    addResources(T, R1, R).

addUnique(V, [], [V]).
addUnique(V, [H|T], [H|L]):-
    V == H, L = T;
    addUnique(V, T, L).

% b) recursos_obj(P, O, R) – verdadeiro se P é uma árvore genérica
% que representa a EAP de um projeto, O é um ob% jetivo deste projeto e
% R a lista de recursos deste objetivo e de seus subobjetivos.
%

recursos_obj(P, O, R):-
    isTree(P),
    findObj(P, O, NP),
    total_recursos(NP, R).

findObj(arv(V, L), O, NP):-
    (
       V = objterm(O , _, _, _),
       NP = arv(V, L)
    );
    (
       V = objint(O , _),
       NP = arv(V, L)
    );
    findObjList(L, O, NP).

findObjList([], _, _):- false.
findObjList([H|T], O, NP):-
    findObj(H, O, NP);
    findObjList(T, O, NP).


% c) (c) custo_total(P, C) – verdadeiro se P é uma árvore genérica
% que representa a EAP de um projeto e C é o so matório dos custos
% dos objetivos deste projeto.
%


custo_total(P, C):-
    isTree(P),
    custTot(P, 0, C).

custTot(arv(V, L), E, R):-
    (
       V = objterm(_ , _, C, _),
       R is E + C
    );
    custTotList(L, E, R).

custTotList([], E, E).
custTotList([H|T], E, R):-
    custTot(H, E, R1),
    custTotList(T, R1, R).

% (d) custo_obj(P, O, C) – verdadeiro se P é uma árvore genérica que
% representa a EAP de um projeto, O é um objetivo deste projeto e C
% é o somatório dos custos para atingir este objetivo.
%

custo_obj(P, O, C):-
    isTree(P),
    findObj(P, O, NP),
    custo_total(NP, C).


% (e) prazo_total(P, O, T) – verdadeiro se P é uma árvore genérica
% que representa a EAP de um projeto e T é o prazo total estimado para
% atingir os objetivos deste projeto.
%

prazo_total(P, T):-
    isTree(P),
    prazTot(P, 0, T).

prazTot(arv(V, L), E, R):-
    (
       V = objterm(_ , _, _, T),
       R is E + T
    );
    prazTotList(L, E, R).

prazTotList([], E, E).
prazTotList([H|T], E, R):-
    prazTot(H, E, R1),
    prazTotList(T, R1, R).


% (f) prazo_obj(P, O, T) – verdadeiro se P é uma árvore genérica que
% representa a EAP de um projeto, O é um objetivo deste projeto e T é
% o prazo total estimado para atingir este objetivo.
%

prazo_obj(P, O, T):-
    isTree(P),
    findObj(P, O, NP),
    prazo_total(NP, T).



% adicionado para a lista extra
%

getObjterm(AG, LO):-
    isTree(AG),
    findTerm(AG,[], LO).

findTerm(arv(V, L), E, R):-
    V = objterm(_,_,_,_),
    R = [V|E];
    findTermList(L, E, R).

findTermList([], E, E).
findTermList([H|T], E, R):-
    findTerm(H, E, R1),
    findTermList(T, R1, R).
