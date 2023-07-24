%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRÀCTICA 2, CLASSE 2

% xifres(L,N) escriu les maneres d'obtenir N a partir de +, -, * dels
% elements de la llista L

% exemple:
% ?- xifres( [4,9,8,7,100,4], 380 ).
%    4 * (100-7) + 8         <-------------
%    ((100-9) + 4 ) * 4
%    ...

xifres(L,N):-
    subcjt(L,S),          % S = [4,8,7,100]
    permutation(S,P),     % P = [4,100,7,8]
    expressio(P,E),       % E = 4 * (100-7) + 8 
    N is E,
    write(E), nl, fail.


% E = ( 4  *  (100-7) )    +    8
%            +
%          /   \
%         *     8
%        / \
%       4   -
%          / \
%        100  7


expressio([X],X).
expressio( L, E1 + E2 ):- 
			  append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expressio( L1, E1 ),
			  expressio( L2, E2 ).
expressio( L, E1 - E2 ):- 
			  append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expressio( L1, E1 ),
			  expressio( L2, E2 ).
expressio( L, E1 * E2 ):- 
	   		  append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expressio( L1, E1 ),
			  expressio( L2, E2 ).

% Com afegir la divisió entera?

expressio( L, E1 // E2 ):- 
	 		  append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expressio( L1, E1 ),
			  expressio( L2, E2 ),
			  K is E2, K\=0. % evitem divisions per zero


% der(E,V,D)  == "la derivada de E respecte de V és D"

der(X,X,1):- var(X), !. 
% ! es fa servir per aturar la cerca de solucions
der(C,_,0):- 
	number(C).
der(A+B,X,U+V):- 
	der(A,X,U), 
	der(B,X,V). 
der(A*B,X,A*V+B*U):- 
	der(A,X,U), 
	der(B,X,V). 
% ...

% OPERADOR DE TALL (!)

% 1.
% Ja hem vist aplicacions de l'operador de tall, com ara:

fact(0,1) :- !.
fact(N,F):- N1 is N-1,fact(N1,F1), F is N * F1.

% Provem a eliminar-lo i consultar 
% fact(0,F), write(F), false.

% 2.
% També podem refer el predicat reunio amb el tall

% reunio([],L,L).
% reunio([X|L1],L2,U) :-     
%	member(X,L2),   
%	reunio(L1,L2,U), !.
% reunio([X|L1],L2,[X|U]) :-  	
%	reunio(L1,L2,U).

% 3.
% El tall elimina l'exploració posterior de l'objectiu
% però també el backtracking dels subobjectius anteriors de l'objectiu
% Per exemple:

p(1).
p(2).

q(a).
q(b).

r(3,4,5).
r(X,Y,Z):- p(X), q(Y), !, s(Z). 
% el tall ! treu de la pila les alternatives per a p(), q() i r()

r(5,6,7).

s(3).
s(4).

h(X,Y,Z):- r(X,Y,Z).
% A B C      A B C  

h(a,b,c).

%% Es comporta així:
%% ?- h(A,B,C), write([A,B,C]), nl, false.
%% [3,4,5]
%% [1,a,3]
%% [1,a,4]
%% [a,b,c]
%% false.


% DISJUNCIÓ

% suposem que volem definir la propietat "X és pare o mare de Y", diguem-ne
% progenitor(X,Y) (en anglès parent(X,Y))
% Faríem

progenitor(X,Y) :- 
	pare(X,Y).

progenitor(X,Y) :- 
	mare(X,Y).

% Es pot fer el següent per incorporar la disjunció, tot i que
% si es pot, és millor evitar-lo per legibilitat

progenitor(X,Y) :-
	pare(X,Y);
	mare(X,Y).


% operadors lògics

and(A,B) :- A, B.

or(A,B) :- A; B.

neg(A) :- A, !, false.
neg(_).

implies(A,B) :- A, !, B.
implies(_,_).

% EXEMPLE DE L'OPERADOR DE TALL: INTERVAL
% Escriure un predicat interval que generi tots els enters entre 
% una fita inferior (1r argument) i una fita superior (2n argument).
% El resultat ha de ser una llista d'enters (3r argument).
% Si la fita inferior és més gran que la superior, retornar [].
% Exemples:
% ?- interval(3,11,X).
% X = [3,4,5,6,7,8,9,10,11]
%
% ?- interval(7,4,X).
% X = []

interval(A,B,[]) :-
	B < A.

interval(A,A,[A]).

interval(A,B,[A|X]) :-
	B > A,
	C is A + 1,
	interval(C,B,X).

rang(A,B) :-
	interval(A,B,X),
	write(X).

% Provar a treure l'operador de tall
% Provar amb interval i amb rang

% De la 1a sessió de la P2, que necessitem per la 2a

subcjt([],[]).
subcjt([_|L],S):- 
	subcjt(L,S).
subcjt([X|L],[X|S]):- 
	subcjt(L,S).