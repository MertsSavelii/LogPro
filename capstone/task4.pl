:-['task2.pl'].


husb(Y, X):-parent(X, T), parent(Y, T), sex(Y, m).
zolovka(Y, X):-husb(T, X), sister(Y, T).

sister(Sistr, X):-parent(Par, Sistr), parent(Par, X), sex(X, f), (Sistr\=X),!.
brother(Bro, X):-parent(Par, Bro), parent(Par, X), sex(X, m), (Bro\=X),!.

mother(Mom, X):-parent(Mom, X), sex(Mom, f), !.
father(Dad, X):-parent(Dad, X), sex(Dad, m), !.

son(X, Parent):-parent(Parent, X), sex(X, m).
daughter(X, Parent):-parent(Parent, X), sex(X, f).

% Отношения между двумя людьми (близкие на 1 поколение)
connection(father, Father, Child):-
    father(Father, Child).

connection(mother, Mother, Child):-
    mother(Mother, Child).

connection(husband, Husband, Wife):-
    parent(Husband,Child),
    parent(Wife, Child),
    Husband \= Wife,
    sex(Husband, m).

connection(wife, Wife, Husband):-
    parent(Husband,Child),
    parent(Wife, Child),
    Husband \= Wife,
    sex(Wife, f).

connection(brother, Brother, X):-
    brother(X,Brother).

connection(sister, Sister, Y):-
    sister(Y, Sister).

connection(parent, Parent, Child):-
    parent(Parent, Child).

connection(child, Child, Parent):-
    parent(Parent, Child).

connection(son, Child, Parent):-
    son(Child, Parent).

connection(daughter, Child, Parent):-
    daughter(Child, Parent).

  chain_of_relation(X):-
      member(X, [father, mother, sister, brother, son, daughter, husband, wife]).


 % Поиск в ширину степени родства (Аналогично поиску в лабораторной №3)
relative_thread(X, Y, Res):-
    bfs_search(X, Y, Res).

ask_relative(X, Y, Res):-
    chain_of_relation(Res), !,
    connection(Res, X, Y).

relative(X, Y, Res):-
    bfs_search(X, Y, Res1), !,
    transform(Res1, Res).

transform([_],[]):-!.
transform([First, Second|Tail], ResList):-
    connection(Relation, First, Second),
    ResList = [Relation|Tmp],
    transform([Second|Tail], Tmp),!.

prolong([X|T], [Y,X|T]):-
    move(X, Y),
    not(member(Y, [X|T]))\+.

move(X,Y):-
    connection(_, X, Y).

bfs_search(X, Y, P):-
    bfs([[X]],Y, L),
    reverse(L, P).

bfs([[X|T]|_], X, [X|T]).
bfs([P|QI], X, R):-
    findall(Z, prolong(P,Z), T),
    append(QI, T, Q0),
    bfs(Q0, X, R),!.

bfs([_|T], Y, L):-
    bfs(T, Y, L).

