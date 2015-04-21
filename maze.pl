mazeSize(5,9).
barrier(1, 8).
barrier(2, 1).
barrier(2, 2).
barrier(2, 4).
barrier(2, 5).
barrier(3, 4).
barrier(3, 7).
barrier(3, 9).
barrier(4, 4).
barrier(4, 7).
barrier(4, 8).
barrier(4, 9).
barrier(5, 2).
%________________________________________
%
solve(From,To,Path):-
	mazeSize(X,Y),
	road(From,To,[From],Snake),
	border(Y),nl,
	build(X,Y,[],Snake,R),
	rev(Snake,Path),
	rev(R, Res),
	printG(Y, Y,Res),
	border(Y).

rev([],[]).
rev([H|T], R):- rev(T,K), append(K,[H],R).

%_________________________________________
%BUILDING THE STRUCTURE OF THE MAZE

build(0,_,[],_,_).
build(X,Y,[],Snake,Res):-
	buildRow(X,Y,[],Snake,R),
	A is X - 1,
	build(A,Y,[],Snake,F),
	append(R,F,Res).

buildRow(_,0,[],_,[]).
buildRow(X,Y,[],Snake,K):-
	barrier(X,Y),
	B is Y - 1,
	buildRow(X,B,[],Snake,M),
	append([x],M,K).

buildRow(X,Y,[],Snake,K):-
	member([X,Y],Snake),
	B is Y - 1,
	buildRow(X,B,[],Snake,M),
	append([o],M,K).

buildRow(X,Y,[],Snake,K):-
	B is Y - 1,
	buildRow(X,B,[],Snake,M),
	append([.],M,K).

%__________________________________________________
%FINDING THE PATH BETWEEN THE TWO POINTS

road([X,Y],[X,Y],R,R).

road([A,B],[C,D],L,R):-
	mazeSize(X,_),
	K is A + 1, K =< X,
	not(barrier(K,B)),
	not(member([K,B],L)),
	append([[K,B]],L,M),
	road([K,B],[C,D],M,R).

road([A,B],[C,D],L,R):-
	mazeSize(_,X),
	K is B + 1, K =< X,
	not(barrier(A,K)),
	not(member([A,K],L)),
	append([[A,K]],L,M),
	road([A,K],[C,D],M,R).

road([A,B],[C,D],L,R):-
	K is A - 1, K > 0,
	not(barrier(K,B)),
	not(member([K,B],L)),
	append([[K,B]],L,M),
	road([K,B],[C,D],M,R).

road([A,B],[C,D],L,R):-
	K is B - 1, K > 0,
	not(barrier(A,K)),
	not(member([A,K],L)),
	append([[A,K]],L,M),
	road([A,K],[C,D],M,R).


%______________________________________________________________________
%PRINTING THE MAZE

printG(_,_,[]).
printG(X,1,[H|T]):- write(H),write('|'),nl,printG(X,X,T).
printG(X,X,[H|T]):- write('|'), write(H),write(' '),A is X - 1, printG(X,A,T).
printG(X,Y,[H|T]):- write(H),write(' '),A is Y - 1, printG(X,A,T).

border(X):- write(+), makeLine(X), write(+).
makeLine(Y):- X is 2*Y-1, drawLine(X).
drawLine(0).
drawLine(X):- write('-'), Z is X-1, drawLine(Z).





