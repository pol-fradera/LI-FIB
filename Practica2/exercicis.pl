% Lista de ejercicios de Prolog (Practica 2)

% 1. Escribe un predicado prod(L,P) que signifique: “P es el producto de los elementos de la lista
% de enteros dada L”. Debe poder generar la P y tambien comprobar una P dada.

prod([X], X) :- !.
prod([X|L], Q) :- prod(L, P), Q is P*X.


% 2. Escribe un predicado pescalar(L1,L2,P) que signifique: “P es el producto escalar de los
% vectores L1 y L2”, donde los vectores se representan como listas de enteros. El predicado debe
% fallar si los dos vectores tienen longitudes distintas.

pescalar([], [], 0).
pescalar([X|XS], [Y|YS], Q) :- pescalar(XS, YS, P), Q is P + X*Y.


% 3. Representando conjuntos con listas sin repeticiones, escribe predicados para las operaciones de
% interseccion y union de conjuntos dados.

% interseccion( L1, L2, I ) == "I es la interseccion de L1 con L2"
interseccion([], _, []).
interseccion([X|L1], L2, [X|I]):- member(X,L2), !, interseccion(L1, L2, I).
interseccion([_|L1], L2, I) :- interseccion(L1, L2, I).

% union( L1, L2, U ) == "U es la union de L1 con L2 (como conjuntos, sin repeticiones)"
union( [],     L,  L ).
union( [X|L1], L2, U     ):- member(X,L2),!, union( L1, L2, U ).
union( [X|L1], L2, [X|U] ):- union( L1, L2, U ).


% dados(P,N,L):- genera_daus(N,L), msort(L,L), suma_daus(L,S), S is P, write(L), nl, fail.

% genera_daus(0,[]):-!.
% genera_daus(N,)

% guanya(_,[],0):-!.
% guanya([X|L1],[Y|L2],S):- X>Y, !, guanya([X|L1],L2,L), S is 1+L.
% guanya(L1,[_|L2],L):- guanya(L1,L2,L).


% 4. Usando append, escribe un predicado para calcular el último elemento de una lista dada, y otro
% para calcular la lista inversa de una lista dada.

ultim(L, U):- append(_, [U], L), !. 

% append(X, [Last], [a,b,c]).
% X = [a,b],
% Last = c.
   
revers([], []).
revers([X|L], RL):- revers(L, RL2), append(RL2, [X], RL).  
% concatena la subllista del reves (RL2) amb el primer element de la llista original (X) i ho deixa a RL


% 5. Escribe un predicado fib(N,F) que signifique: “F es el N-esimo numero de Fibonacci para la
% N dada”. Estos n´umeros se definen as´ı: fib(1) = 1, fib(2) = 1, y si N > 2 entonces fib(N) =
% fib(N − 1) + fib(N − 2).

fib(1, 1):-!.
fib(2, 1):-!.
fib(N, F):- N1 is N-1, N2 is N-2, fib(N1, F1), fib(N2, F2), F is F1 + F2.


% 6. Escribe un predicado dados(P,N,L) que signifique: “la lista L expresa una manera de sumar
% P puntos lanzando N dados”. Por ejemplo: si P es 5 y N es 2, una solucion serıa [1,4] (notese
% que la longitud de L es N). Tanto P como N vienen instanciados. El predicado debe ser capaz de
% generar todas las soluciones posibles.

dados(0, 0, []):-!.
dados(_,0,_):- !, fail. 
dados(P, N, [X|L]):- member(X,[1,2,3,4,5,6]), Q is P-X, M is N-1, dados(Q,M,L).


% 7. Escribe un predicado suma demas(L) que, dada una lista de enteros L, se satisface si existe algun
% elemento en L que es igual a la suma de los demas elementos de L, y falla en caso contrario.

suma_llista([], 0):-!.
suma_llista([X|L], S):- suma_llista(L, T), S is T+X.

suma_demas(L):- suma_llista(L,R), member(X,L), X2 is 2*X, R == X2, !.


% 8. Escribe un predicado suma_ants(L) que, dada una lista de enteros L, se satisface si existe algun
% elemento en L que es igual a la suma de los elementos anteriores a el en L, y falla en caso
% contrario.

suma_ants([],_):- !, false.
suma_ants([X|_], S):- X == S, !.
suma_ants([X|L], S):- S2 is S + X, suma_ants(L, S2).

suma_ants(L):- suma_ants(L,0).


% 9. Escribe un predicado card(L) que, dada una lista de enteros L, escriba la lista que, para cada
% elemento de L, dice cuantas veces aparece este elemento en L. Por ejemplo, si hacemos la consulta
% card( [1,2,1,5,1,3,3,7] ) el interprete escribira: [[1,3],[2,1],[5,1],[3,2],[7,1]].

count([],_,0):-!.
count([X|L],Y,R):- X == Y, !, count(L,Y,R2), R is R2 + 1.
count([_|L],Y,R):- count(L,Y,R).

remove([], _, []):-!.
remove([X|L], Y, R):- X == Y, !, remove(L, Y, R).
remove([X|L], Y, R):- remove(L, Y, R2), R = [X|R2]. %append([X], R2, R).   %%%% no funciona R is [X|R2]  %% is només s'utilitza per evaluar una expressio aritmetica

card([],[]):-!.
card([X|L], [R|R2]):- count([X|L], X, N), R = [X,N], remove(L,X,LR), card(LR, R2).  %%% no funciona posar R is [X,N]  %% es podria posar card([X|L], [[X,N]|R2]):-

card(L):-card(L,C),write(C).


% 10. Escribe un predicado esta ordenada(L) que signifique: “la lista L de numeros enteros esta
% ordenada de menor a mayor”. Por ejemplo, a la consulta:
% ?-esta ordenada([3,45,67,83]).
% el interprete responde yes, y a la consulta:
% ?-esta ordenada([3,67,45]).
% responde no.

% esta_ordenada([],_):- !, write("yes").
% esta_ordenada([X|_], Y):- Y > X, !, write("no").
% esta_ordenada([X|L], _):- esta_ordenada(L,X).

% esta_ordenada([X|L]):- esta_ordenada(L,X).

esta_ordenada([]):- !, write("yes").
esta_ordenada([_]):- !, write("yes").
esta_ordenada([X,Y|_]):- X > Y, !, write("no").  %% [X,Y|L] es una llista amb minim dos valors X i Y concatenats amb la llista L (que pot ser buida)
esta_ordenada([_,Y|L]):- esta_ordenada([Y|L]).


% 11. Escribe un predicado ord(L1,L2) que signifique: “L2 es la lista de enteros L1 ordenada de
% menor a mayor”. Por ejemplo: si L1 es [4,5,3,3,2] entonces L2 sera [2,3,3,4,5]. Hazlo en
% una lınea, usando solo los predicados permutacion y esta ordenada.

pert_con_resto(X,L,R) :- append(L1,[X|L2],L), append(L1,L2,R).  

permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).

esta_ordenada2([]):-!.
esta_ordenada2([_]):-!.
esta_ordenada2([X,Y|_]):- X > Y, !, fail.  %% [X,Y|L] es una llista amb minim dos valors X i Y concatenats amb la llista L (que pot ser buida)
esta_ordenada2([_,Y|L]):- esta_ordenada2([Y|L]).

ord(L1, L2):- esta_ordenada2(L2), permutacion(L2, P), P == L1, !.


% 12. Escribe un predicado diccionario(A,N) que, dado un alfabeto A de sımbolos y un natural N,
% escriba todas las palabras de N simbolos, por orden alfabetico (el orden alfabetico es segun el
% alfabeto A dado). Por ejemplo, diccionario( [ga,chu,le],2) escribira:
% gaga gachu gale chuga chuchu chule lega lechu lele.

diccionario(A,N):-  nperts(A,N,S), escribir(S), fail.

pert(X,[X|_]).
pert(X,[_|Y]) :- pert(X,Y). 

nperts(_,0,[]):-!.
nperts(L,N,[X|S]):- pert(X,L), N1 is N-1, nperts(L,N1,S).

escribir([]):-write(' '),nl,!.
escribir([X|L]):- write(X), escribir(L).


% 13. Escribe un predicado palindromos(L) que, dada una lista de letras L, escriba todas las permutaciones
% de sus elementos que sean pal´ındromos (capicuas). Por ejemplo, con la consulta
% palindromos([a,a,c,c]) se escribe [a,c,c,a] y [c,a,a,c].

palindromos(L) :- setof(P,(permutation(L,P), es_palindromo(P)),S), write(S). 

es_palindromo([]):-!.
es_palindromo([_]) :- !.
es_palindromo([X|L]) :- append(L1,[X],L), es_palindromo(L1).   % [c,a,a,b], [X] is [c], L is [a,a,b], no existeix cap L1 que concatenat amb [X] sigui L
                                                               % [c,a,a,c], [X] is [c], L is [a,a,c], L1 es [a,a], es fa el cas recursiu eliminant els extrems


% 14. Encuentra mediante un programa Prolog, usando el predicado permutacion, que 8 dıgitos diferentes
% tenemos que asignar a las letras S, E, N, D, M, O, R, Y, de manera que se cumpla la suma
% siguiente:

suma([],[],[],C,C).
suma([X1|L1],[X2|L2],[X3|L3],Cin,Cout) :-
	X3 is (X1 + X2 + Cin) mod 10,
	C  is (X1 + X2 + Cin) //  10,
	suma(L1,L2,L3,C,Cout).


send_more_money1 :-

	L = [S, E, N, D, M, O, R, Y, _, _],
	permutacion(L, [0,1,2,3,4,5,6,7,8,9]),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

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


send_more_money2 :-

	L = [0,1,2,3,4,5,6,7,8,9],
	pert_con_resto(M,  [0,1], _),
	pert_con_resto(M,  L,  L0),
	pert_con_resto(O, L0, L1),
	pert_con_resto(R, L1, L2),
	pert_con_resto(Y, L2, L3),
	pert_con_resto(S, L3, L4),
	pert_con_resto(E, L4, L5),
	pert_con_resto(N, L5, L6),
	pert_con_resto(D, L6, _),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

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



% 15. Escribe un predicado simplifica que pueda usarse en combinacion con el programa de calcular
% derivadas.

sim(A,B):- unpas(A,C),!, sim(C,B).
sim(A,A):-!.

unpas(A,B):- pasSuperior(A,B).
unpas(A,B):- pasSubExpressio(A,B).

pasSubExpressio(A,B):-
    A=..[F|La],
    append(L1,[Ea|L2],La),
    unpas(Ea,Eb),
    append(L1,[Eb|L2],Lb),
    B=..[F|Lb].

pasSuperior(A+0,A).
pasSuperior(0+B,B).
pasSuperior(A-0,A).
pasSuperior(0-B,-B).
pasSuperior(A-A,0).
pasSuperior(A*1,A).
pasSuperior(1*B,B).
pasSuperior(A/1,A).
pasSuperior(_*0,0).
pasSuperior(0*_,0).
pasSuperior(0/_,0).
pasSuperior(A+B,N ):- integer(A), integer(B), N is A+B.
pasSuperior(A*B,N ):- integer(A), integer(B), N is A*B.
pasSuperior(A-B,N ):- integer(A), integer(B), N is A-B.
pasSuperior(A//B,N):- integer(A), integer(B), B\=0, N is A//B.


% 16. Queremos obtener en Prolog un predicado dom(L) que, dada una lista L de fichas de domin´o (en
%   el formato de abajo), escriba una cadena de domin´o usando todas las fichas de L, o escriba “no
%   hay cadena” si no es posible. Por ejemplo,
%       ?- dom( [ f(3,4), f(2,3), f(1,6), f(2,2), f(4,2), f(2,1) ] ).
%   escribe la cadena correcta:
%       [ f(2,3), f(3,4), f(4,2), f(2,2), f(2,1), f(1,6) ].
%   Tambien podemos girar alguna ficha como f(N,M), reemplaz´andola por f(M,N). As´ı, para:
%       ?- dom ([ f(4,3), f(2,3), f(1,6), f(2,2), f(2,4), f(2,1) ]).
%   solo hay cadena si se gira alguna ficha (por ejemplo, hay la misma cadena que antes).
%   El siguiente programa Prolog a´un no tiene en cuenta los posibles giros de fichas, ni tiene implementado
%   el predicado ok(P), que significa: “P es una cadena de domin´o correcta (tal cual, sin
%   necesidad ya de girar ninguna ficha)”:

%   p([],[]).
%   p(L,[X|P]) :- select(X,L,R), p(R,P).

%   dom(L) :- p(L,P), ok(P), write(P), nl.
%   dom( ) :- write(’no hay cadena’), nl.
%   (a) ¿Que significa el predicado p(L,P) para una lista L dada?
    %% genera totes les possibles permutacions de L.
%   (b) Escribe el predicado ok(P) que falta.
%   (c) Extiende el predicado p para que el programa tambien pueda hacer cadenas girando alguna
%       de las fichas de la entrada.

ok([]):-!.
ok([_]):-!.
ok([f(_,Y),f(X2,Y2)|P]):- Y == X2, ok([f(X2,Y2)|P]).

p([],[]).
p(L,[f(X,Y)|P]) :- select(f(X,Y),L,R), p(R,P).
p(L,[f(X,Y)|P]) :- select(f(Y,X),L,R), p(R,P).

dom(L) :- p(L,P), ok(P), write(P), nl.
dom(_) :- write('no hay cadena'), nl.


% 17. Complete the following backtracking procedure for SAT in Prolog. Program everything, except
%   the predicate readclauses(F), which reads a list of clauses, where each clause is a list of integers.
%   For example, p3 ∨ ¬p6 ∨ p2 is represented by [3,-6,2]. Do things as simple as possible.

readclauses(F):- F.

p:- readclauses(F), sat([],F).
p:- write('UNSAT'),nl.


decision_lit([X|_], Lit):- length(X, 1), !, member(Lit, X).
decision_lit([X], Lit):- member(Lit, X), !.
decision_lit([_|L], Lit):- decision_lit(L, Lit).


simplif(_,[],[]):-!.
simplif(Lit,[X|L],F1):- member(Lit,X), !, simplif(Lit,L,F1).
simplif(Lit,[X|_],_):- NLit is Lit*(-1), member(NLit,X), length(X,1), !, fail.
simplif(Lit,[X|L],[Z|F1]):- NLit is Lit*(-1), member(NLit,X), !, select(NLit,X,Z), simplif(Lit,L,F1),!.
simplif(Lit,[X|L],[X|F1]):- simplif(Lit,L,F1).


sat(I,[]):- write('IT IS SATISFIABLE. Model: '), write(I),nl,!.
sat(I,F):-    
    decision_lit(F,Lit), % Select unit clause if any; otherwise, an arbitrary one.
    simplif(Lit,F,F1), % Simplifies F. Warning: may fail and cause backtracking
	sat([Lit|I], F1).


% 18. Consider two groups of 10 people each. In the first group, as expected, the percentage of people
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


% 19. Supongamos que tenemos una m´aquina que dispone de monedas de valores [X1,...Xn] y tiene
% que devolver una cantidad C de cambio utilizando el minimo numero de monedas. Escribe un
% programa Prolog maq(L,C,M) que, dada la lista de monedas L y la cantidad C, genere en M la
% lista de monedas a devolver de cada tipo. Por ejemplo, si L es [1,2,5,13,17,35,157], y C es
% 361, entonces una respuesta es [0,0,0,1,2,0,2] (5 monedas).
% Note: greedy algorithms (starting with the largest coin, etc.) do not always work!

maq(L,C,M):- maq_auxiliar(L,C,M,1).

maq_auxiliar(L,C,M,N):- length(L,A), perm_monedes(A,N,M), pescalar(L,M,C), !.
maq_auxiliar(L,C,M,N):- N1 is N+1, maq_auxiliar(L,C,M,N1).

perm_monedes(0,0,[]):- !.
perm_monedes(0,_,_):- !,fail.
perm_monedes(A,N,[X|P]):- between(0,N,X), A1 is A-1, N1 is N-X, perm_monedes(A1,N1,P).


% 20. Write in Prolog a predicate flatten(L,F) that “flattens” (cast.: “aplana”) the list F as in the
% example:
% ?-flatten( [a,b,[c,[d],e,[]],f,[g,h]], F ).
% F=[a,b,c,d,e,f,g,h]?

flatten([],[]):-!.
flatten([X|L],[X|F]):- not(is_list(X)), ! , flatten(L,F).
flatten([X|L],FR):- flatten(X,Fx), flatten(L,F), append(Fx,F,FR).

% 21. Escribe un predicado Prolog log(B,N,L) que calcula la parte entera L del logaritmo en base B
% de N, donde B y N son naturales positivos dados. Por ejemplo, ?- log(2,1020,L). escribe L=9?
% Podeis usar la exponenciacion, como en 125 is 5**3. El programa (completo) no debe ocupar
% mas de 3 lineas.

% sense exponenciació
%log_aux(_,N,N,I,I):-!.
log_aux(_,N,M,I,L):- M > N, !, L is I-1.
log_aux(B,N,M,I,L):- M1 is M*B, I1 is I+1, log_aux(B,N,M1,I1,L).

log(B,N,L):- log_aux(B,N,1,0,L).

% amb exponenciació
log2_aux(B,N,I,L):- M is B**I, M > N, !, L is I-1.
log2_aux(B,N,I,L):- I1 is I+1, log2_aux(B,N,I1,L).

log2(B,N,L):- log2_aux(B,N,0,L).


% 22. Supongamos que N estudiantes (identificados por un n´umero entre 1 y N) se quieren matricular
% de LI, pero s´olo hay espacio para M, con M < N. Adem´as nos dan una lista L de pares de estos
% estudiantes que son incompatibles entre s´ı (por ejemplo, porque siempre se copian). Queremos
% obtener un programa Prolog li(N,M,L,S) que, para N, M y L dados, genere un subconjunto S
% con M de los N estudiantes tal que si [x, y] ∈ L entonces {x, y} ̸⊆ S. Por ejemplo, una soluci´on de
% li( 20, 16, [[8,11],[8,15],[11,6],[4,9],[18,13],[7,9],[16,8],[18,10],[6,17],[8,20]], S )
% es [20,19,17,16,15,14,13,12,11,10,7,5,4,3,2,1] .
% Escribe una versi´on lo m´as sencilla que puedas, aunque sea ineficiente, del estilo “generar una
% soluci´on (total) y despu´es comprobar si es correcta”.

pos_assig(_,0,[]):-!.
pos_assig(0,_,[]):-!,fail.
pos_assig(N,M,[N|L]):- N1 is N-1, M1 is M-1, pos_assig(N1,M1,L).
pos_assig(N,M,L):- N1 is N-1, pos_assig(N1,M,L).

comprova([],_):-!.
comprova([X|_],S):- nth0(0,X,E1), nth0(1,X,E2), member(E1,S), member(E2,S), !, fail.
comprova([_|L],S):- comprova(L,S).

li(N,M,L,S):- pos_assig(N,M,S), comprova(L,S), !.


% 23. Given a list of integers L, and a maximum sum K, write the subsets Sm of L such that:
%    sum(Sm) =< K, and
%    no element in L \ Sm can be added to Sm without exceeding the sum K.
% For the example below, a correct output would be the following (or in another order):
% [2,5,-2,1]
% [2,-2,2,3,1]
% [2,-2,2,4]
% [2,-2,4,1]
% [5,-2,2,1]
% [5,-2,3]
% [7,-2,1]
% [-2,2,4,1]
% [-2,3,4,1]
% Hint: you can use the predicate sum list(L, X), which is true if X is the sum of the numbers
% in L; e.g., sum list([1,2,3], 6) holds.
% Complete:

%% Example:
numbers([2,5,7,-2,2,9,3,4,1]).
maxSum(6).

%% subsetWithRest(L, Subset, Rest) holds
%% if Subset is a subset of L and Rest is the rest of the elements.
subsetWithRest([], [], []).
subsetWithRest([X|L],[X|Sm],Rest):- subsetWithRest(L,Sm,Rest).
subsetWithRest([X|L],Sm,[X|Rest]):- subsetWithRest(L,Sm,Rest).

%% maxSubset(K, L, Sm) holds
%% if Sm is a subset of numbers of L such that
%% it sums at most K
%% and if we try to add any other element, the sum exceeds K.

maxSubset(K, L, Sm):- subsetWithRest(L, Sm, Rest), sum_list(Sm,S), S =< K, sort(Rest,SRest), nth0(0,SRest,X), S+X > K.

main :-
numbers(L), maxSum(K),
maxSubset(K, L, Sm),
write(Sm), nl, fail.
main:- halt.


% 24. Given a graph declared as in the example below, write all its cliques of size at least minCliqueSize.
% Remember, a clique is a complete subgraph: a subset {textttS of the vertices such that for all
% U,V in S there is an edge U-V.
% For the example below, a correct output would be the following (or in another order):
% [2,4,5,7,9]
% [2,4,5,7]
% [2,4,5,9]
% [2,4,7,9]
% [2,4,8,9]
% [2,5,7,9]
% [4,5,7,9]

% Complete:

%%==== Example: ========================================================
numVertices(10).
minCliqueSize(4).
vertices(Vs):- numVertices(N), findall(I,between(1,N,I),Vs).
vertex(V):- vertices(Vs), member(V,Vs).
edge(U,V):- edge1(U,V).
edge(U,V):- edge1(V,U).
edge1(9,8).
edge1(8,2).
edge1(7,4).
edge1(5,7).
edge1(4,2).
edge1(5,2).
edge1(2,7).
edge1(7,9).
edge1(2,9).
edge1(4,8).
edge1(4,9).
edge1(9,5).
edge1(4,5).
%%==========================================================

main2:- vertices(Vs), subconjunto( Vs, S), minCliqueSize(T), length(S,Sl), Sl >= T, isClique(S), write(S), nl, fail.
main2:- halt.

isClique(S):- forall((member(U,S),member(V,S), U > V), edge(U,V)).

subconjunto([],[]):-!.
subconjunto([V|Vs], [V|S]):- subconjunto(Vs,S).
subconjunto([_|Vs], S):- subconjunto(Vs,S).


% 25. Complete the following predicate in prolog.
% nthRoot( N, K, R ) === "Given positive integers N and K, the integer part of the Nth root of K is R".
% Example: the integer part of the 2th root (square root) of 16 is 4.
% Example: the integer part of the 3rd root (cubic root) of 8 is 2.
% Example: the integer part of the 4th root of 16 is 2.
% Example: the integer part of the 4th root of 15 is 1.
nthRoot( N, K, R ):- between(1,K,R), R**N >= K, !.


% 26. Complete the following predicate in prolog.
% allSSSS(L) (allSquareSummingSubSequences) ===
% "Given a sequence of positive integers L, write all non-empty subsequences of L
% whose sum is a perfect square, in the following format":
% ?- allSSSS([6,3,4,5,6,9,8,5,2,3,4]).
% 9-[6,3]
% 49-[3,4,5,6,9,8,5,2,3,4]
% 4-[4]
% 9-[4,5]
% 9-[9]
% 9-[2,3,4]
% 4-[4]
allSSSS(L):- subconjunto(L,SS), SS \= [], sum_list(SS,Sum), nthRoot(2, Sum, R), R*R =:= Sum, write(Sum-SS).

% La diferencia entre R is 4+5 i R = 4+5, es que amb is s'evalua i per tant R val 9,
% en canvi, amb = unifica termes i R s'unificara amb 5+4 sense evaluar
% Si a R es vol assignar una llista es fa R = [X|L] perque is es nomes per fer operacions
% aritmetiques

% per negar: not o \+

% between(1,6,N), write(N), nl, fail
% 1
% 2
% 3
% 4
% 5
% 6

% fins que falli

% between(1,6,N).
% N = 1;
% N = 2;

% s'ha de posar ; perquè ensenyi la següent alternativa

% between(1,6,N), N =< 4, write(N), nl, fail.
% 1
% 2
% 3
% 4