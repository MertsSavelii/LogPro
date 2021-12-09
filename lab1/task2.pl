% Task 2: Relational Data
% Для работы необходимо подключить task1.pl и базу данных one.pl
:-['one.pl'].
:-['task1.pl'].

% Вариант 2
%    - Напечатать средний балл для каждого предмета - h1().
%    - Для каждой группы, найти количество не сдавших студентов - h2().
%    - Найти количество не сдавших студентов для каждого из предметов - h3().

% задание 2/1
% функция возращает среднее значение по оценкам по каждому предмету
h1() :-
    findall(X, subject(X, _), L),
    h1_out(L).

h1_out([]).
h1_out([H|T]) :-
    average_grade(H, GRADE),
    subject(H, PRED),
    write(PRED),        tab(1),
    write('---'),       tab(1),
    write(GRADE),       nl,
    h1_out(T).

% возвращает среднюю оценку по предметам
average_grade(SUB, X) :-
    findall(PIV, grade(_, SUB, PIV), GRADES),
    length_l(LEN, GRADES),
    sum_l(GRADES, SUM),
    X is SUM / LEN.

% задание 2/2
% возвращает кол во несдавших студентов по каждой группе
h2() :-
    findall(X, student(_, X), STUDENTS),    % - список студентов
    findall(Y, student(Y, _), GROUP1),
    rem_repetittions(GROUP1, GROUP2),                   % - группы без повторов
    naive_sort(GROUP2, GROUP), !,           % - группы отсортированы
    h2_out(GROUP, STUDENTS).

% возвращает список без повторов
rem_repetittions([], []) :- !.
rem_repetittions([L],[L]) :- !.
rem_repetittions([H|T], [H|T1]) :-
    remove_l(H, T, T2),
    rem_repetittions(T2, T1), !.


% получает список групп и студентов и пишет тех кто не сдал
h2_out([],_) :- !.
h2_out([H1|T], P) :-
    h2_student_in_group(H1,P,INGROUP),
    h2_num_of_faild_exam(H1, INGROUP, COUNT),
    write('В '),            tab(1),
    write(H1),              tab(1),
    write(' не сдало '),    tab(1),
    write(COUNT),           tab(1),
    write('человек'),       nl,
    h2_out(T, P),!.             % - рекурсиваня проверка след группы

h2_student_in_group(_, [], []) :- !. 
h2_student_in_group(G, [H|T], L) :-
    h2_5(G, H, I),
    h2_student_in_group(G, T, L1),
    append_l(I, L1, L).
h2_5(G, H, [H]) :- student(G, H).
h2_5(_, _, []).

h2_num_of_faild_exam(_,[],0).
h2_num_of_faild_exam(G,[H|T],RES) :-        % G - группа [H|T] - массив студентов
    h2_num_of_faild_exam(G, T, R),          % счиатется их кол во рекурсивно от хвоста
    h2_6(H, U),             % проверка на плохую оценку H - текущий студент
    RES is (R + U).

h2_6(H, 1) :-
    grade(H, _, 2), !.
h2_6(_, 0) :- !.


% задание 2/3
% возвращает кол во несдавших студентов по каждому предмету
h3() :-
    findall(X, student(_, X), STUDENTS),    % - список студентов
    findall(Y, subject(Y, _), SUB),         % - список предметов абрв
    h3_out(SUB, STUDENTS), !.

h3_out([],_) :- !.
h3_out([H|T], P) :-
    h3_num_of_faild_exam(H, P, COUNT),
    subject(H, H1),
    write(H1),              tab(1),
    write('не сдало'),      tab(1),
    write(COUNT),           tab(1),
    write('человек'),       nl,
    h3_out(T, P).             % - рекурсиваня проверка след группы

h3_num_of_faild_exam(_,[],0).
h3_num_of_faild_exam(G, [H|T], RES) :-      % G - предмет [H|T] - массив студентов
    h3_num_of_faild_exam(G, T, R),          % счиатется их кол во рекурсивно от хвоста
    h3_bad_grade_chek(H, G, U),          % проверка на плохую оценку H - текущий студент
    RES is (R + U).

h3_bad_grade_chek(H, G, 1) :-
    grade(H, G, 2), !.
h3_bad_grade_chek(_,_,0) :- !.



% сумма числового списка
sum_l([], 0).
sum_l([H|T],Sum) :-
  	sum_l(T,Sum1),
  	Sum is H + Sum1.

% сортировка
naive_sort(List,Sorted) :-
  	perm_l(List,Sorted),
  	is_sorted(Sorted).
is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|T]) :-
  	X =< Y,
  	is_sorted([Y|T]).

