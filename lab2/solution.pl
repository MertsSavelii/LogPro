% 16. Комендант переселял студентов на время ремонта общежития.
% Дело это не простое. Судите сами. На очередную комнату было 8 кандидатов,
% а поселить в нее можно было только четырех. Пошел комендант расспрашивать студентов,
% кто с кем жить хочет. Вот, что он услышал: Андрей согласен на любых соседей.
% Борис без Кости не переселится. Костя не хочет жить в одной комнате с Василием.
% Василий согласен жить с кем угодно. Дима не будет переселяться без Юры.
% Федя не будет без Гриши жить в одной комнате с Димой,
% а без Димы не будет жить в одной комнате с Костей. Гриша не хочет,
% чтобы его соседями были Борис и Костя вместе, а кроме того он не желает
% жить в одной комнате ни с Андреем, ни с Василием.
% Юра даст согласие переехать в новую комнату, если туда же переберутся либо Борис,
% либо Федя. Кроме того, Юра не будет жить в одной комнате с Костей,
% если туда не переедет Гриша, и не желает жить в одной комнате ни с Андреем,
% ни с Василием. Каким образом комендант смог учесть все пожелания? 

data(L):- member(boris,L), not(member(kostya,L)), !, fail.
data(L):- member(kostya,L), member(vasiliy,L), !, fail.
data(L):- member(dima,L), not(member(yura,L)), !, fail.
data(L):- member(fedya,L), member(dima,L), not(member(grisha,L)), !, fail.
data(L):- member(fedya,L), member(kostya,L), not(member(dima,L)), !, fail.
data(L):- member(grisha,L), member(kostya,L), member(boris,L), !, fail.
data(L):- member(grisha,L), member(andrey,L), !, fail.
data(L):- member(grisha,L), member(vasiliy,L), !, fail.
data(L):- member(yura,L), not(member(boris,L)), not(member(fedya,L)), !, fail.
data(L):- member(yura,L), member(kostya,L), not(member(grisha,L)), !, fail.
data(L):- member(yura,L), member(andrey,L), !, fail.
data(L):- member(yura,L), member(vasiliy,L), !, fail.
data(_).

solve(L):-
    permutation([X1,X2,X3,X4,X5,X6,X7,X8],[andrey,boris,kostya,vasiliy,grisha,dima,fedya,yura]),
    data([X1,X2,X3,X4]),
    L=[X1,X2,X3,X4], !.