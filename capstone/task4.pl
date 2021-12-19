:-['task2.pl'].



relative(X, Y, Res):-
    search_bfs(X, Y, Res1), !,
    transform(Res1, Res).

search_bfs(X,Y,P):-
    bfs([[X]],Y,L),
    reverse(L,P).

bfs([[X|T]|_],X,[X|T]).
bfs([P|QI],X,R):-
    findall(Z,prolong(P,Z),T),
    append(QI,T,Q0),
    bfs(Q0,X,R),!.

bfs([_|T],Y,L):- bfs(T,Y,L).

prolong([X|T],[Y,X|T]):-
    move(X,Y),
    \+ member(Y,[X|T]).

move(X,Y):-
    check_link(_,X,Y).

transform([_],[]):-!. % переделевает цепочку родственников в цепочку родства
transform([First,Second|Tail],ResList):-
    check_link(Relation,First,Second),
    ResList = [Relation|Tmp],
    transform([Second|Tail],Tmp),!.

sibling(Person, Sibling):-
    child(Person, P),
    child(Sibling, P),
    Person \= Sibling.

check_link(husband, Husband, Wife):-
    child(Child, Husband),
    child(Child, Wife),
    Husband \= Wife,
    male(Husband).

check_link(wife, Wife, Husband):-
    child(Child, Husband),
    child(Child, Wife),
    Husband \= Wife,
    female(Wife).

check_link(brother, Brother, Y):-
    sibling(Brother, Y),
    male(Brother).

check_link(sister, Sister, Y):-
    sibling(Sister, Y),
    female(Sister).

check_link(father, Father, Child):-
    child(Child, Father),
    male(Father).

check_link(mother, Mother, Child):-
    child(Child, Mother),
    male(Mother).

check_link(parent, Parent, Child):-
    child(Child, Parent).

check_link(son, Child, Parent):-
    child(Child, Parent),
    male(Child).

check_link(daughter, Child, Parent):-
    child(Child, Parent),
    female(Child).

check_link(child, Child, Parent):-
    child(Child, Parent).

check_relation(X):-
    member(X, [father, mother, sister, brother, son, daughter, husband, wife]).    