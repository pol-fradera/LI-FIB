% 1. Escribe un predicado prod(L,P) que signifique: “P es el producto de los elementos de la lista
% de enteros dada L”. Debe poder generar la P y tambien comprobar una P dada.

prod([X|L],P):- prod(L,P1), P is P1*X.
prod([],1).

% 2. Escribe un predicado pescalar(L1,L2,P) que signifique: “P es el producto escalar de los
% vectores L1 y L2”, donde los vectores se representan como listas de enteros. El predicado debe
% fallar si los dos vectores tienen longitudes distintas.

pescalar([],[],0).
pescalar([X|L1],[Y|L2],P):- pescalar(L1,L2,P1), P is (X*Y)+P1.

% 3. Representando conjuntos con listas sin repeticiones, escribe predicados para las operaciones de
% interseccion y union de conjuntos dados.

interseccion([],_,[]).
interseccion([X|L1],L2,[X|L3]):- member(X,L2),!,interseccion(L1,L2,L3).
interseccion([_|L1],L2,L3):- interseccion(L1,L2,L3).

union([],L,L).
union([X|L1],L2,[X|L3]):- \+ member(X,L2),!,union(L1,L2,L3).
union([_|L1],L2,L3):- union(L1,L2,L3).

% 4. Usando append, escribe un predicado para calcular el  ́ultimo elemento de una lista dada, y otro
% para calcular la lista inversa de una lista dada.

ultimo(L,R):- append(_,[R],L).

inverso([],[]).
inverso([X|L],RL):- inverso(L,RL2), append(RL2,[X],RL).

% 5. Escribe un predicado fib(N,F) que signifique: “F es el N-ésimo número de Fibonacci para la
% N dada”. Estos números se definen así: fib(1) = 1, fib(2) = 1, y si N > 2 entonces fib(N) =
% fib(N − 1) + fib(N − 2).

fibonacci(1,1).
fibonacci(2,1).
fibonacci(N,F):- N > 2, N1 is (N-1), N2 is (N-2), fibonacci(N1,F1), fibonacci(N2,F2), F is F1+F2.

% 6. Escribe un predicado dados(P,N,L) que signifique: “la lista L expresa una manera de sumar
% P puntos lanzando N dados”. Por ejemplo: si P es 5 y N es 2, una solución sería [1,4] (nótese
% que la longitud de L es N). Tanto P como N vienen instanciados. El predicado debe ser capaz de
% generar todas las soluciones posibles.

dados(0,0,[]):- !.
% dados(_,0,_):- !, fail. % Suprimint la condició N > 0.
dados(P,N,[X|L]):- N > 0, member(X,[1,2,3,4,5,6]), N1 is (N-1), PR is (P-X), dados(PR,N1,L).

% 7. Escribe un predicado suma_demas(L) que, dada una lista de enteros L, se satisface si existe algún
% elemento en L que es igual a la suma de los demás elementos de L, y falla en caso contrario.

sumaLlista([],0):- !.
sumaLlista([X|L],S):- sumaLlista(L,S1), S is S1+X. 

eliminar_nth(X,L,R):- append(L1,[X|L2],L), append(L1,L2,R).

suma_demas(L):- eliminar_nth(X,L,R), sumaLlista(R,X), !.

% 8. Escribe un predicado suma_ants(L) que, dada una lista de enteros L, se satisface si existe algún
% elemento en L que es igual a la suma de los elementos anteriores a él en L, y falla en caso
% contrario.

suma_ants(L):- append(L1,[X|_],L), sumaLlista(L1,X), !.

% 9. Escribe un predicado card(L) que, dada una lista de enteros L, escriba la lista que, para cada
% elemento de L, dice cuántas veces aparece este elemento en L. Por ejemplo, si hacemos la consulta
% card( [1,2,1,5,1,3,3,7] ) el intérprete escribiría: [[1,3],[2,1],[5,1],[3,2],[7,1]].

count(_,[],0):- !.
count(X,[Y|L],C):- X == Y, !, count(X,L,C1), C is C1+1.
count(X,[_|L],C):- count(X,L,C).

remove(_,[],[]):- !.
remove(X,[X|L],R):- remove(X,L,R), !.
remove(X,[Y|L],[Y|R]):- remove(X,L,R).

card([],[]).
card([X|L],[[X,C]|RC]):- count(X,L,C1), C is C1+1, remove(X,L,R), card(R,RC).  % C is C1+1 porque en count(X,L,C1), L no contiene la primera X.

card(L):- card(L,C), write(C).

/* Sol·lució oficial */

% Si al calcular la cardinalitat d'un numero, ja l'haviem calculat previament, l'eliminem del resultat i afegim 1.
% En cas contrari, vol dir que es el primer cop que el calculem i per tant afegirem [X,1].

% car([],[]).
% car( [X|L] , [ [X,N1] |Cr] ):-car(L,C),eliminar_nth([X,N],C,Cr),!,N1 is N+1.
% car( [X|L] , [ [X,1]   |C] ):-car(L,C).

% car(L):-car(L,C),write(C).

% 10. Escribe un predicado esta ordenada(L) que signifique: “la lista L de números enteros està ordenada de menor a mayor”. Por ejemplo, a la consulta:
% ?-esta ordenada([3,45,67,83]).
% el intérprete responde yes, y a la consulta:
% ?-esta ordenada([3,67,45]).
% responde no.

ordenada([]).
ordenada([_]):- !.
ordenada([X,Y|L]):- X =< Y, ordenada([Y|L]).

esta_ordenada(L):- ordenada(L), !, write('yes'), nl.
esta_ordenada(_):- write('no'), nl.

% 11. Escribe un predicado ord(L1,L2) que signifique: “L2 es la lista de enteros L1 ordenada de menor a mayor”. Por ejemplo: si L1 es [4,5,3,3,2] 
% entonces L2 será [2,3,3,4,5]. Hazlo en una línea, usando sólo los predicados permutacion y ordenada.

% Genera totes les possibles permutacions d'una llista de tamany n (n!).
permutacion([],[]).
permutacion(L,[X|P]) :- eliminar_nth(X,L,R), permutacion(R,P).

ord(L1,L2):- permutacion(L1,L2), ordenada(L2), !.

% 12. Escribe un predicado diccionario(A,N) que, dado un alfabeto A de sımbolos y un natural N, escriba todas las palabras de N sımbolos, 
% por orden alfabetico (el orden alfabetico es segun el alfabeto A dado). Por ejemplo, diccionario( [ga,chu,le],2) escribira:
% gaga gachu gale chuga chuchu chule lega lechu lele.

diccionario(A,N):- permutarNsimbolos(A,N,R), escribir(R), fail.

permutarNsimbolos(_,0,[]):- !.
permutarNsimbolos(A,N,[W|R]):- member(W,A), N1 is N-1, permutarNsimbolos(A,N1,R).

escribir([]):- write(' '), nl.
escribir([X|L]):- write(X), escribir(L).

% 13. Escribe un predicado palindromos(L) que, dada una lista de letras L, escriba todas las per-
% mutaciones de sus elementos que sean palindromos (capicuas). Por ejemplo, con la consulta
% palindromos([a,a,c,c]) se escribe [a,c,c,a] y [c,a,a,c].

% Si no queremos que escriba repetidos se puede usar setof. 
palindromos(L):- setof(L1,(permutacion(L,L1), inverso(L1,L1)),S), write(S).

% 14. Encuentra mediante un programa Prolog, usando el predicado permutacion, que 8 digitos difer-
% entes tenemos que asignar a las letras S, E, N, D, M, O, R, Y, de manera que se cumpla la suma
% siguiente:

%   S E N D
% + M O R E
%----------
% M O N E Y

suma([],[],[],C,C).
suma([X1|L1],[X2|L2],[X3|L3],C,M):- X3 is (X1 + X2 + C) mod 10, C2 is (X1 + X2 + C) div 10, suma(L1,L2,L3,C2,M), !.

sendmoremoney:- 
    L = [S, E, N, D, M, O, R, Y, _, _],
    permutacion([0,1,2,3,4,5,6,7,8],L),
    suma([D,N,E,S],[E,R,O,M],[Y,E,N,O],0,M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.


								%%%% EJERCICIO 15 %%%%
% Escribe un predicado simplifica que pueda usarse en combinacion con el programa de calcular
% derivadas.

simplifica(E,E1):- unpaso(E,E2),!, simplifica(E2,E1).
simplifica(E,E).

unpaso(A+B,A+C):- unpaso(B,C),!.
unpaso(B+A,C+A):- unpaso(B,C),!.
unpaso(A*B,A*C):- unpaso(B,C),!.
unpaso(B*A,C*A):- unpaso(B,C),!.
unpaso(0*_,0):-!.
unpaso(_*0,0):-!.
unpaso(1*X,X):-!.
unpaso(X*1,X):-!.
unpaso(0+X,X):-!.
unpaso(X+0,X):-!.
unpaso(N1+N2,N3):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*N2,N3):- number(N1), number(N2), N3 is N1*N2,!.
unpaso(N1*X+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*X+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.

								%%%% EJERCICIO 16 %%%%

p([],[]).
p(L,[X|P]) :- select(X,L,R), p(R,P).

ok([]):- !.
ok([_]):- !.
ok([f(_,T),f(H,T2)|R]):- T = H, ok([f(H,T2)|R]), !.
ok([f(_,T),f(H,T2)|R]):- T = T2, ok([f(T2,H)|R]), !.

dom(L) :- p(L,P), ok(P), write(P), nl.
dom( ) :- write('no hay cadena'), nl.


								%%%% EJERCICIO 17 %%%%

readclauses([[1],[-1, 2], [-2, 3], [2], [-3, 1, 2, -4], [-5], [-1, 4]]).

p:- readclauses(F), sat([],F).
p:- write('UNSAT'),nl.

sat(I,[]):- write('IT IS SATISFIABLE. Model: '), write(I),nl,!.
sat(I,F):- decision_lit(F,Lit), simplif(Lit,F,F1), sat([Lit|I],F1).

% Unit cluase -> clause with only one literal
decision_lit([C|_],Lit):- member(Lit,C), length(C,1), !. % Select unit clause if any; 
% Arbitrary
decision_lit([C|_],Lit):- member(Lit,C). %otherwise, an arbitrary one.

simplif(_, [], []):- !.
% Lit belongs to clause
simplif(Lit,[X|F], F1):- member(Lit, X), simplif(Lit, F, F1), !.
% not(Lit) belongs to clause
simplif(Lit,[X|F], F1):-
    PositiveLit is 0 - Lit,
    member(PositiveLit, X),!,
    eliminar_nth(PositiveLit, X, X1),
    simplif(Lit, F, F2),
    append([X1], F2, F1).
% Lit does not belong to clause
simplif(Lit,[X|F], F1):-
    simplif(Lit, F, F2),
    append([X], F2, F1).

								%%%% EJERCICIO 18 %%%%

% Consider two groups of 10 people each. In the first group, as expected, the percentage of people
% with lung cancer among smokers is higher than among non-smokers. In the second group, the
% same is the case. But if we consider the 20 people of the two groups together, then the situation
% is the opposite: the proportion of people with lung cancer is higher among non-smokers than
% among smokers! Can this be true? Write a little Prolog program to find it out.

cancer :- 
	between(0, 10, SWC1), % Smoker With Cancer (group 1)
	between(0, 10, SWC2), % Smoker With Cancer (group 2)
	between(0, 10, SWNC1), % Smoker With No Cancer (group 1)
	between(0, 10, SWNC2), % Smoker With No Cancer (group 2)
	between(0, 10, NSWC1), % Non-Smoker With Cancer (group 1)
	between(0, 10, NSWC2), % Non-Smoker With Cancer (group 2)
	between(0, 10, NSWNC1), % Non-Smoker With No Cancer (group 1)
	between(0, 10, NSWNC2), % Non-Smoker With No Cancer (group 2)
	10 is (SWC1 + SWNC1 + NSWC1 + NSWNC1),
	10 is (SWC2 + SWNC2 + NSWC2 + NSWNC2),
	(SWC1+SWNC1) > 0, (NSWC1+NSWNC1) > 0,
	(SWC1/(SWC1+SWNC1)) > (NSWC1/(NSWC1+NSWNC1)),
	(SWC2+SWNC2) > 0, (NSWC2+NSWNC2) > 0,
	(SWC2/(SWC2+SWNC2)) > (NSWC2/(NSWC2+NSWNC2)),
	((NSWC1+NSWC2)/(NSWC1+NSWC2+NSWNC1+NSWNC2)) > ((SWC1+SWC2)/(SWC1+SWC2+SWNC1+SWNC2)),
	write(SWC1','SWC2','SWNC1','SWNC2','NSWC1','NSWC2','NSWNC1','NSWNC2), !.

								%%%% EJERCICIO 19 %%%%

% Supongamos que tenemos una maquina que dispone de monedas de valores [X1,...Xn] y tiene
% que devolver una cantidad C de cambio utilizando el mınimo numero de monedas. Escribe un
% programa Prolog maq(L,C,M) que, dada la lista de monedas L y la cantidad C, genere en M la
% lista de monedas a devolver de cada tipo. Por ejemplo, si L es [1,2,5,13,17,35,157], y C es
% 361, entonces una respuesta es [0,0,0,1,2,0,2] (5 monedas).

maq(L,C,M):- maq_auxiliar(L,C,M,1).

maq_auxiliar(L,C,M,N):- length(L,A), perm_monedas(A,N,M), pescalar(L,M,C), !.
maq_auxiliar(L,C,M,N):- N1 is N+1, maq_auxiliar(L,C,M,N1).

perm_monedas(0,0,[]):- !.
perm_monedas(0,S,[S]):- !.
perm_monedas(A,N,[X|P]):- between(0,N,X), A1 is A - 1, N1 is N - X, perm_monedas(A1,N1,P).

								%%%% EJERCICIO 20 %%%%
% Write in Prolog a predicate flatten(L,F) that “flattens” (cast.: “aplana”) the list F as in the
% example:
% ?-flatten( [a,b,[c,[d],e,[]],f,[g,h]], F ).
% F=[a,b,c,d,e,f,g,h]?

flatten([],[]).
flatten([X|L],F):- is_list(X), flatten(X,X1), flatten(L,F1), append(X1,F1,F), !.
flatten([X|L],[X|F]):- flatten(L,F).

								%%%% EJERCICIO 21 %%%%

% Escribe un predicado Prolog log(B,N,L) que calcula la parte entera L del logaritmo en base B
% de N, donde B y N son naturales positivos dados. Por ejemplo, ?- log(2,1020,L). escribe L=9?
% Podeis usar la exponenciacion, como en 125 is 5**3. El programa (completo) no debe ocupar
% mas de 3 lineas.

log(B,N,L):- logAux(B,N,0,L).
logAux(B,N,I,L):- X is B**I, (X > N), L is I-1, !.
logAux(B,N,I,L):- I1 is I + 1, logAux(B,N,I1,L).

								%%%% EJERCICIO 22 %%%%

% Supongamos que N estudiantes (identificados por un n ́umero entre 1 y N) se quieren matricular
% de LI, pero solo hay espacio para M, con M < N. Ademas nos dan una lista L de pares de estos
% estudiantes que son incompatibles entre sı (por ejemplo, porque siempre se copian). Queremos
% obtener un programa Prolog li(N,M,L,S) que, para N, M y L dados, genere un subconjunto S
% con M de los N estudiantes tal que si [x, y] ∈ L entonces {x, y}̸⊆ S. Por ejemplo, una solucion de
% li( 20, 16, [[8,11],[8,15],[11,6],[4,9],[18,13],[7,9],[16,8],[18,10],[6,17],[8,20]], S )
% es [20,19,17,16,15,14,13,12,11,10,7,5,4,3,2,1] .
% Escribe una version lo mas sencilla que puedas, aunque sea ineficiente, del estilo “generar una
% solucion (total) y despues comprobar si es correcta”.

li(N,M,L,S):- findall(X, between(1, N, X), N1), perm_estudiantes_M(N1,M,S), compatible(S,L).

perm_estudiantes_M(_,0,[]).
perm_estudiantes_M(N,M,[ES|S]):- member(ES,N), eliminar_nth(ES,N,N1), M1 is M - 1, perm_estudiantes_M(N1,M1,S).

compatible(_,[]):- true, !.
compatible(S,[[ES1,ES2]|L]):- \+ (member(ES1,S), member(ES2,S)), !, compatible(S,L).

								%%%% EJERCICIO 23 %%%%

% Given a list of integers L, and a maximum sum K, write the subsets Sm of L such that:
% 	· sum(Sm) =< K, and
% 	· no element in L \Sm can be added to Sm without exceeding the sum K.
% For the example below, a correct output would be the following (or in another order):
% 	[2,5,-2,1]
% 	[2,-2,2,3,1]
% 	[2,-2,2,4]
% 	[2,-2,4,1]
% 	[5,-2,2,1]
% 	[5,-2,3]
% 	[7,-2,1]
% 	[-2,2,4,1]
% 	[-2,3,4,1]
% Hint: you can use the predicate sum list(L, X), which is true if X is the sum of the numbers
% in L; e.g., sum list([1,2,3], 6) holds.

%% Example:
numbers([2,5,7,-2,2,9,3,4,1]).
maxSum(6).

%% subsetWithRest(L, Subset, Rest) holds
%% if Subset is a subset of L and Rest is the rest of the elements.
subsetWithRest([], [], []).
subsetWithRest([X|L],[X|S],R):- subsetWithRest(L,S,R).
subsetWithRest([X|L],S,[X|R]):- subsetWithRest(L,S,R).  

%% maxSubset(K, L, Sm) holds
%% if Sm is a subset of numbers of L such that it sums at most K
%% and if we try to add any other element, the sum exceeds K.
maxSubset(K, L, Sm):-
	subsetWithRest(L, Sm, Rest),
	sum_list(Sm,S),
	S =< K,
	forall(member(X,Rest), S+X > K).

%% Main
main :-
	numbers(L), maxSum(K),
	maxSubset(K, L, Sm),
	write(Sm), nl, fail.
main:- halt.