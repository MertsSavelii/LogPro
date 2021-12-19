:-['task4.pl'].

% who is *name* brother/sister?
task([A, B, C, D, E, F]):-
    member(A, [who, "Who"]),
    member(B, [is]),
    (male(C); female(C)),
    member(D, ["'s"]),
    check_relation(E),
    member(F, ['?']), !,
    relationship(E, Res, C),
    write(Res), write(" is "), write(C), write("'s "), write(E). 

% is *name* *name* s son/daughter?
task([A, B, C, D, E, F]):-
    member(A,[is]),
    (male(B); female(B)),
    (male(C); female(C)),
    member(D, ["'s"]),
    check_relation(E),
    member(F, ['?']),
    relationship(E,B,C), !. %'

% проверка на корректную степень родства в запросе
check_relation(X):-
    member(X, [father, mother, sister, brother, son, daughter, husband, wife]).
