/*assaf lovton 209844414 assaflovton@campus.technion.ac.il eden dembinsky 212227888 edendem@campus.technion.ac.il*/

card(Number,Suit):- Number in 2..14, (Suit=clubs;Suit=hearts;Suit=spades;Suit=diamonds).
smallsuit(clubs,clubs).
smallsuit(clubs,hearts).
smallsuit(clubs,spades).
smallsuit(clubs,diamonds).
smallsuit(hearts,hearts).
smallsuit(hearts,spades).
smallsuit(hearts,diamonds).
smallsuit(spades,spades).
smallsuit(spades,diamonds).
smallsuit(diamonds,diamonds).
smaller(card(N,S),card(N2,S2)):- N#=N2,smallsuit(S,S2).
smaller(card(N,_),card(N2,_)):- N#<N2.
lowest_aux([],Low,X):- X=Low,!.
lowest_aux([Y|YS],Low,X):- smaller(Y,Low),lowest_aux(YS,Y,X),!.
lowest_aux([_|YS],Low,X):- lowest_aux(YS,Low,X),!.
lowest([Y],Low):-Low=Y,!.
lowest([Y1,Y2|YS],X):- smaller(Y1,Y2),lowest_aux(YS,Y1,X),!.
lowest([_,Y2|YS],X):- X = Y2,lowest_aux(YS,Y2,X),!.

filter_aux(C,[Y|YS],Tmp,Res):-smaller(Y,C),filter_aux(C,YS,Tmp,Res),!.
filter_aux(C,[Y|YS],Tmp,Res):-filter_aux(C,YS,[Y|Tmp],Res),!.
filter_aux(_,[],Tmp,Res):- Tmp=Res.
filter(C,[Y|YS],X):-filter_aux(C,[Y|YS],[],X),!.
filter(_,[],X):-X=[].

winner_aux(L1,L2,X):- lowest(L1,A1),lowest(L2,A2),A1=A2,X=0,!.
winner_aux(L1,L2,X):- lowest(L1,A1),lowest(L2,A2),smaller(A1,A2),X=1,!.
winner_aux(_,_,X):- X=2,!.
winner(L1,L2,C,X):-filter(C,L1,Z1),filter(C,L2,Z2),winner_aux(Z1,Z2,X).

replace([],_,_,X):- X=[],!.
replace([_|YS],0,Value,[Value|YS]).
replace([Y|YS],Index,Value,[Y|X]):-I is Index - 1,I #>= 0, replace(YS,I,Value,X),!.

action(_,_,_,_,h).
action(_,_,_,_,l).
action(_,_,_,_,r).
tape(_,Index):- Index #> 0.

find([action(Ps,Ns,Tin,Output,D)|_],Ps,Tin,Res):- Res = action(Ps,Ns,Tin,Output,D),!.
find([action(_,_,_,_,_)|AS],PS,Tin,Res):- find(AS,PS,Tin,Res),!.
findin([I|_],0,Res):- Res = I,!.
findin([_|IS],Index,Res):- Ind is Index - 1, findin(IS,Ind,Res),!.
newindex(h,Index,Res) :- Res is Index ,!.
newindex(r,Index,Res) :- Res is Index + 1,!.
newindex(l,Index,Res) :- Res is Index - 1,!.

turing(Ps,Alist,tape(Tlist,Index),X):- findin(Tlist,Index,Tin),
    find(Alist,Ps,Tin,action(_,Ns,_,Output,D)),replace(Tlist,Index,Output,TmpList),D \= h ,
    newindex(D,Index,NewIndex),turing(Ns,Alist,tape(TmpList,NewIndex),X),!.
turing(Ps,Alist,tape(Tlist,Index),X):- findin(Tlist,Index,Tin),
    find(Alist,Ps,Tin,action(_,_,_,_,h)),X = tape(Tlist,Index).
change(Num,X):-change100(Num,X).

change1(Num,[1|X]):-Num>0 ,N1 is Num-1,change1(N1,X). 
change1(Num,X):-Num=0,X=[].

change5(Num,[5|X]):-Num>0 ,N5 is Num-5,change5(N5,X). 
change5(Num,[1|X]):-Num>0 ,N1 is Num-1,change1(N1,X). 
change5(Num,X):-Num=0,X=[].

change10(Num,[10|X]):-Num>0 ,N10 is Num-10,change10(N10,X). 
change10(Num,[5|X]):-Num>0 ,N5 is Num-5,change5(N5,X). 
change10(Num,[1|X]):-Num>0 ,N1 is Num-1,change1(N1,X). 
change10(Num,X):-Num=0,X=[].

change50(Num,[50|X]):-Num>0 ,N50 is Num-50,change50(N50,X). 
change50(Num,[10|X]):-Num>0 ,N10 is Num-10,change10(N10,X). 
change50(Num,[5|X]):-Num>0 ,N5 is Num-5,change5(N5,X). 
change50(Num,[1|X]):-Num>0 ,N1 is Num-1,change1(N1,X). 
change50(Num,X):-Num=0,X=[].

change100(Num,[100|X]):-Num>0 ,N100 is Num-100,change100(N100,X). 
change100(Num,[50|X]):-Num>0 ,N50 is Num-50,change50(N50,X). 
change100(Num,[10|X]):-Num>0 ,N10 is Num-10,change10(N10,X). 
change100(Num,[5|X]):-Num>0 ,N5 is Num-5,change5(N5,X). 
change100(Num,[1|X]):-Num>0 ,N1 is Num-1,change1(N1,X). 
change100(Num,X):-Num=0,X=[].



