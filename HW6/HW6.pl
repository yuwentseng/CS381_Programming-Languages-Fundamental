% CS381 HW5
% Team Members: YuWen Tseng 933652910
% Chekai Chang 933612783
% Hong- Yuan Huang 933669671
% PoSheng Hsu 933620817

% Exercise 1
when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,cov216).
where(399,dear118).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,398).

% (a)
schedule(S,P,T) :- enroll(S,C), (where(C,P), when(C,T), nl).

% (b)
usage(P,T) :- when(C,T), where(C,P), nl.

% (c)
conflict(X,Y) :- when(X,T), when(Y,T), where(X,P), where(Y,P), X \= Y.

% (d)
meet(A,B) :- enroll(A,CA), enroll(B,CB), A \= B, where(CA,P), where(CB,P), when(CA,T1), when(CB,T2), (T1 = T2 ; (T1 is T2 + 1; meet(B,A))).

% Exercise 2
% (a)
rdup([X|Xs],M) :- rdup(Xs,P), member(X,P), M = P.
rdup([X|Xs],M) :- rdup(Xs,P), \+ member(X,P), append([X],P,M).
rdup([],[]).

% Answer that is technically valid but probably not intended
rdup2(X,M) :- list_to_set(X,M).

% (b)
flat([X|Xs],F) :- is_list(X), flat(X,G), flat(Xs,H), append(G,H,F).
flat([X|Xs],F) :- \+ is_list(X), flat(Xs,G), append([X],G,F).
flat([],[]).

% (c)
project(P,E,L) :- project(P,E,L,0).
project([P|Ps],[E|Es],L,C) :- D is C + 1, P = D, project(Ps,Es,M,D), append([E],M,L).
project([P|Ps],[_|Es],L,C) :- D is C + 1, \+ P = D, project([P|Ps],Es,L,D).
project(_,_,[],_).