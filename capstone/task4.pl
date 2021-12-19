:-['task2.pl'].

relative(X, Y, Res):-
    search_bfs(X, Y, Res1), !,
    transform(Res1, Res).

%поиск из 3 лабораторной работы
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
    not(member(Y,[X|T])).

move(X,Y):- relationship(_,X,Y).

% переделевает цепочку родственников в цепочку родства
transform([_],[]):-!. 
transform([First,Second|Tail],Res):-
    relationship(Relation,First,Second),
    Res = [Relation|Tmp],
    transform([Second|Tail],Tmp),!.

% вспомогательный предикат поиска родного брата/сестры
geschwister(Person, Geschwister):-
    child(Person, P),
    child(Geschwister, P),
    not(Person = Geschwister).

relationship(husb, Husb, Wife):-
    child(Child, Husb),
    child(Child, Wife),
    not(Husb = Wife),
    male(Husb).

relationship(wife, Wife, Husb):-
    child(Child, Husb),
    child(Child, Wife),
    not(Husb = Wife),
    female(Wife).

relationship(brother, Brother, I):-
    geschwister(Brother, I),
    male(Brother).

relationship(sister, Sister, I):-
    geschwister(Sister, I),
    female(Sister).

relationship(father, Father, Child):-
    child(Child, Father),
    male(Father).

relationship(mother, Mother, Child):-
    child(Child, Mother),
    male(Mother).

relationship(parent, Parent, Child):-
    child(Child, Parent).

relationship(son, Child, Parent):-
    child(Child, Parent),
    male(Child).

relationship(daughter, Child, Parent):-
    child(Child, Parent),
    female(Child).

relationship(child, Child, Parent):- child(Child, Parent).
    