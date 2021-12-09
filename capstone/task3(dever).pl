:-['task2.pl'].

hus(Fem,Male):-
    female(Fem), male(Male),
    child(Son,Fem), child(Son,Male).
bro(Male,Bro):-
    male(Male), male(Bro),
    child(Male, Paren), child(Bro, Paren),
    not(Male = Bro).
dever(Fem,Answ):-
    hus(Fem,Hus),
    bro(Hus,Answ).