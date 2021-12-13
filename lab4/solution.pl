pristix("без").
pristix("бес").
pristix("в").
pristix("вс").
pristix("во").
pristix("вы").
pristix("за").
pristix("ис").
pristix("из").
pristix("до").
pristix("над").
pristix("на").
pristix("о").
pristix("об").
pristix("от").
pristix("пере").
pristix("по").
pristix("под").
pristix("про").
pristix("пре").
pristix("при").
pristix("рез").
pristix("рас").
pristix("с").
pristix("со").
pristix("через").
pristix("чрес").

suffix("онн").
suffix("ова").
suffix("ева").
suffix("ыва").
suffix("ива").
suffix("ен").
suffix("ан").

ending("у", rod("НЕТ"), chislo("ед")).
ending("ю", rod("НЕТ"), chislo("ед")).
ending("ете", rod("НЕТ"), chislo("мн")).
ending("ите", rod("НЕТ"), chislo("мн")).
ending("ишь", rod("НЕТ"), chislo("ед")).
ending("ил", rod("М"), chislo("ед")).
ending("ел", rod("М"), chislo("ед")).
ending("ал", rod("М"), chislo("ед")).
ending("ела", rod("Ж"), chislo("ед")).
ending("ила", rod("Ж"), chislo("ед")).
ending("ала", rod("Ж"), chislo("ед")).
ending("ило", rod("С"), chislo("ед")).
ending("или", rod("НЕТ"), chislo("мн")).
ending("ем", rod("НЕТ"), chislo("мн")).
ending("ет", rod("НЕТ"), chislo("ед")).
ending("ит", rod("НЕТ"), chislo("ед")).
ending("ют", rod("НЕТ"), chislo("мн")).
ending("ут", rod("НЕТ"), chislo("мн")).
ending("ат", rod("НЕТ"), chislo("мн")).
ending("ят", rod("НЕТ"), chislo("мн")).
ending("им", rod("НЕТ"), chislo("мн")).
ending("ешь", rod("НЕТ"), chislo("ед")).


an_morf(Slov, morf(prist(Y), kor(Ost), end(Z), Rod, Chislo)):-
    f_prist(Slov, prist(Y), Slov1), !,
    (Y == "" -> Slov1 = Slov; Slov1 = Slov1),
    f_ending(Slov1, end(Z), Rod, Chislo, Slov2), !,
    (Z == "" -> Slov2 = Slov1; Slov2 = Slov2),
    f_suffix(Slov2, suf(X), Ost), !,
    (X == "" -> Ost = Slov2; Ost = Ost).

f_prist([_|[]], prist(""), _):-!.
f_prist([H|Slov], prist(H), Slov):-
    pristix(H).
f_prist([H, H1|Slov], prist(Prist), OstSlova):-
    string_concat(H, H1, NewPrist),
    append([NewPrist], Slov, NewSlov),
    f_prist(NewSlov, prist(Prist), OstSlova).

f_suffix([_|[]], suf(""), _):-!.
f_suffix(Slov, suf(Suffix), Ost):-
    last(Slov, Suffix),
    dellast(Slov, Ost),
    suffix(Suffix).
f_suffix(Slov, suf(S), OstSlova):-
    last(Slov, Suffix),
    dellast(Slov, Ost),
    last(Ost, Suffix2),
    dellast(Ost, Ost2),
    string_concat(Suffix2, Suffix, NewSuffix),
    append(Ost2, [NewSuffix], NewSlov),
    f_suffix(NewSlov, suf(S), OstSlova).

f_ending([_|[]], end(""), rod("М"), chislo("ед"), _):-!.
f_ending(Slov, end(End), Rod, Chislo, Ost):-
    last(Slov, End),
    dellast(Slov, Ost),
    ending(End, Rod, Chislo).
f_ending(Slov, end(E), Rod, Chislo, OstSlova):-
    last(Slov, End),
    dellast(Slov, Ost),
    last(Ost, End2),
    dellast(Ost, Ost2),
    string_concat(End2, End, NewEnd),
    append(Ost2, [NewEnd], NewSlov),
    f_ending(NewSlov, end(E), Rod, Chislo, OstSlova).

dellast([_],[]):-!.
dellast([X|T],[X|Y]):-dellast(T,Y).
