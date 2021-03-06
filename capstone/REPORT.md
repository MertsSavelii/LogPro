# Отчет по курсовому проекту
## по курсу "Логическое программирование"

### студент: <Мерц Савелий Павлович>

## Результат проверки

Вариант задания:

 - [ ] стандартный, без NLP (на 3)
 - [x] стандартный, с NLP (на 3-4)
 - [ ] продвинутый (на 3-5)
 
| Преподаватель     | Дата         |  Оценка       |
|-------------------|--------------|---------------|
| Сошников Д.В. |              |               |
| Левинская М.А.|              |               |

> *Комментарии проверяющих (обратите внимание, что более подробные комментарии возможны непосредственно в репозитории по тексту программы)*

## Введение

В результате выполнения курсового проекта я получил следующие навыки и знания:

1. Углубил свои познания в теоретической части логического программирования и теории игр. В процессе написания эссе на тему: "Как использовать логические языки чтобы научить компьютер играть в шахматы/шашки", я узнальше больше о самих шахматах, истории развития шахмат как игры, развитие шахматных программ и развитии алгоритмов оптимизации поиска решений. Смог достаточно подробно описать алгоритм программирования игры "шахматы" на языке "Prolog".

2. Смог составить и обрабтать данные в формате GEDCOM. Если в будущем мне понадобится обрабатывать генеалогические деревья, то мне не придется разбираться с их структурой хранения.

3. Углубил познания в языке Python, а также написал парсер, обрабатывающий файл в формате GEDCOM.

4. Закрепил навыки, полученные в лабораторной работе номер 3, связанные с поиском в пространстве состояний. Мне они пригодились для реализации поиска в ширину, который, в свою очередь, был использован для получения цепочки родства.

5. Закрепил навыки, полученные в лабораторной работе номер 4, связанные с обработкой списка символов как осмысленной строки или слова.

## Задание

 1. Создать родословное дерево своего рода на несколько поколений (3-4) назад в стандартном формате GEDCOM с использованием сервиса MyHeritage.com 
 2. Преобразовать файл в формате GEDCOM в набор утверждений на языке Prolog, используя следующее представление:  с использованием предиката `child(ребенок, родитель)`, `male(человек)`, `female(человек)`.
 3. Реализовать предикат проверки/поиска деверя (брат мужа).
 4. Реализовать программу на языке Prolog, которая позволит определять степень родства двух произвольных индивидуумов в дереве
 5. [На оценки хорошо и отлично] Реализовать естественно-языковый интерфейс к системе, позволяющий задавать вопросы относительно степеней родства, и получать осмысленные ответы. 

## Получение родословного дерева

Своё родословное дерево с помощью сайта [MyHeritage.com](www.myheritage.com) и рисунка его рисунка, который я делал ещё в школе, в нём 17 индивидуумов. Затем я скачал его в формате GEDCOM через встроенную в сервис функцию. 

## Конвертация родословного дерева

Я решил использовать, не изученный мной, язык python для конвертации, потому что он популярен для решения подобных задач, а значит смогу найти больше колличество информации для её решения. Я, конечно, слышал, что "питон" делает многое за программиста, но не думал, что будет всё "из коробки" и на столько несложно. После того, как я разобрался в структуре GEDCOM файла, алгоритм программы сам появился в голове. В цикле считываем строчку, разбиваем на служеные слова и инфрмацию, считываем факты, имеющие зависимость, и записывем их в словарь, для последующего дополнения, факты пола сразу пишу в выходной файл, добравшись до информации о родственных связях записываю в выходной файл факты, используя информацию из словаря.

## Предикат поиска родственника

Согласно варианту мне необходимо реализовать поиск девера (брат мужа). Для этого я написал ещё два дополнительных:

Поиск мужа
```prolog
hus(Fem,Male):-
    female(Fem), male(Male),
    child(Son,Fem), child(Son,Male).
```
Поиск брата
```prolog
bro(Male,Bro):-
    male(Male), male(Bro),
    child(Male, Paren), child(Bro, Paren),
    not(Male = Bro).
```
Ну и сам предикат поиска девера благодаря дополнительным предикатам лучился коротким и понятным
```prolog
dever(Fem,Answ):-
    hus(Fem,Hus),
    bro(Hus,Answ).
```
## Определение степени родства

Для того, чтобы найти наикратчайшее отношение родства использовал поиск в ширину. С помощью него и реализованного пердиката relationship, которое определяет прямые связи между людьми, раскручиваем цепочку связей. Потом меняем список с именами на список родства.

Результаты работы
```prolog
?- relative('Савелий Мерц','Екатерина Мачкова',X).
X = [son].

?- relative('Савелий Мерц','Алексей Мерц',X).
X = [son, brother].

?- relative('Екатерина Мачкова','Алексей Мерц',X).
X = [wife, brother].
```
## Естественно-языковый интерфейс

Обработка запросов весьма проста. Считываем его как списко слов, с их помощью проверяем запрос на ликвидность, вызываем предикат из 4 задания. Конечно программа ограничена в количестве распозноваемых слов, но тх всегда можно добавить.

Листинг программы
```prolog
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
```

Результаты работы
```prolog
?- task([who,is,'Валентин Мачков',"'s",daughter,?]).
Екатерина Мачкова is Валентин Мачков's daughter
true ;
Елена Мачкова is Валентин Мачков's daughter
true ;
Татьяна Мачкова is Валентин Мачков's daughter
true ;
Татьяна Мачкова is Валентин Мачков's daughter
true.

?- task([is,'Екатерина Мачкова','Валентин Мачков',"'s",X,?]).
X = daughter.

?- task([is,'Савелий Мерц','Павел Мерц',"'s",son,?]).
true.
```

## Выводы

Развил свою логику, наверное это главный результат данной работы. Познакомившись на практике с типичными задачами, в которых можно применять язык прораммирования Prolog, становится понятным, что всё это можно было написать и на друих языках менее удобно. Познакомится с логическим программированием необходимо, но я бы изменил сотношение сложности лабораторных к количеству часов занятий. Не думаю, что смогу применить полученные за эту работу навыки в первые годы работы по професии, но может ког-нибудь и пригодится. После этого курса меня заинтересовало функциональное программирование.
