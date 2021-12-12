% поиск в глубину
search_DFS(A, B):-
    write('Старт'),nl,
    get_time(DFS),
    dfs([A], B, L),
    print_Res(L),
    get_time(DFS1),
    write('Конец'), nl, nl,
    T1 is DFS1 - DFS,
    write('Время '), write(T1), nl, nl.

dfs([X|T], X, [X|T]).
dfs(P,F,L):-
    prolong(P, P1),
    dfs(P1,F,L).

% поиск в ширину
search_BFS(X, Y):-
    write('Старт'), nl,
    get_time(BFS),
    bfs([[X]], Y, L),
    print_Res(L),
    get_time(BFS1),
    write('Конец'), nl, nl,
    T1 is BFS1 - BFS,
    write('Время '), write(T1), nl, nl.

bfs([[B|T]|_], B, [B|T]).
bfs([H|QT], X, R):-
    findall(Z, prolong(H, Z), T),
    append(QT, T, OutQ), !,
    bfs(OutQ, X, R).
bfs([_|T], X, R):- bfs(T, X, R).

% поиск с итерационным углублением
search_IDS(Start, Finish):-
    write('Старт'), nl,
    get_time(ITER),
    int(DepthLimit),
    ids([Start], Finish, Res, DepthLimit),
    print_Res(Res),
    get_time(ITER1),
    write('Конец'), nl, nl,
    T1 is ITER1 - ITER,
    write('Время '), write(T1), nl, nl.
search_IDS(Start,Finish,Path):-
    int(Level),
    search_IDS(Start,Finish,Path,Level).

ids([Finish|T], Finish, [Finish|T], 0).
ids(Path, Finish, R, N):-
    N > 0,
    prolong(Path, NewPath),
    N1 is N - 1,
    ids(NewPath, Finish, R, N1).

int(1).
int(X):-
    int(Y),
    X is Y + 1.

print_Res([]).
print_Res([A|T]):-
    print_Res(T),
    write(A), nl.

prolong([X|T], [Y, X|T]):-
    move(X, Y),
    not(member(Y, [X|T])).

%переход из начального состояния, возможны 3 вариации
move(s([Item1,Item2,Item3],'L',[]), s([Item1,Item2],'R',[Item3])):- not(check_exept([Item1,Item2])).
move(s([Item1,Item2,Item3],'L',[]), s([Item1,Item3],'R',[Item2])):- not(check_exept([Item1,Item3])).
move(s([Item1,Item2,Item3],'L',[]), s([Item2,Item3],'R',[Item1])):- not(check_exept([Item2,Item3])).
%если лодка на правом берегу и на нем 2 объекта которые неопасно оставлять вместе то лодка переходит на левый берег
move(s([Left|T],'R',Right), s([Left|T],'L',Right)):- not(check_exept(Right)).
move(s(Left,'R',[Item1,Item2]), s(Out,'L',[Item2])):-
    check_exept([Item1, Item2]),
    add(Item1, Left, Out).
% перемещение объектов 
move(s([L|LT],'L',[R|RT]),s(LT,'R',Out)):- add(L, [R|RT], Out).
move(s([X,L|LT],'L',[R|RT]),s([X|LT],'R',Out)):- add(L, [R|RT], Out).

add(E, [], [E]).
add(E, [H|T], [H|T1]):- add(E, T, T1).

% проверка исключений
check_exept([Item1, Item2]):- excep(Item1, Item2).

% исключения
excep('Коза', 'Волк').
excep('Коза', 'Капуста').
excep('Волк', 'Коза').
excep('Капуста', 'Коза').
