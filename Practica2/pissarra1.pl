
% INTRODUCCIÓ a Prolog: document p6.pdf

% Per executar l'intèrpret:
% swipl
% apareix el prompt ?-
% escrivim [fitxer]. (en aquest cas, [prolog]) i ja
% podem fer consultes

% Interacció: enter (o .), espai (o ;), a (abortar execució)

% Exemple

mes_gran(elefant,cavall).
mes_gran(cavall,ruc).
mes_gran(ruc,gos).
mes_gran(ruc,gat).

% Podem fer consultes:
%   ?- mes_gran(ruc,gos).
%   true.
%   ?- mes_gran(gat,ruc).
%   false.
%   ?- mes_gran(elefant,gat).
%   false.

% L'última consulta falla perquè al sistema li falta informació sobre el món.
% Una possible manera de solucionar-ho és afegint:

mes_gran(X,Y) :- mes_gran(X,Z), mes_gran(Z,Y).

% Però si provem
%   ?- mes_gran(gat,cavall).
% Prolog fa una recursió "infinita"
% És millor

mesGran(X,Y) :- mes_gran(X,Y).
mesGran(X,Y) :- mes_gran(X,Z), mesGran(Z,Y).

% ara podem preguntar
% ?- mesGran(X,gat).
% X = ruc;
% X = elefant;
% X = cavall;
% false.

% en canvi, podem provar mes_gran(X,gat) i veurem que és molt diferent


% SINTAXI

% TERMES (O PREDICATS)
%
% àtom: elefant, a, k_234, una_altra_ronda,
% predicat predefinit: atom/1
%   es pot consultar si un terme és un àtom amb
%   ?- atom(X).
%   false.
%   ?- atom(atom).
%   true.

% nombre: 1, -4, 6.88
% predicat predefinit: number/1

% variable: A, Elefant, _ (variable sense nom)
% predicat predefinit: variable/1

% terme compost: àtom + arguments entre parèntesis
% mes_gran(cavall,X), f(g(X,_),7)
% germa(pere,albert), tieta(X,Y)

% predicat predefinit: compound/1
% ?- compound(f(x)).
% true.
% ?- compound(f).
% false.

% CLÀUSULES
%
% Fet: predicat seguit de punt:
% 	mes_gran(balena,_). 
% 	la_vida_es_bella.

% Regla: consta d'un cap (predicat) i un cos
% (seqüència de predicats separats per comes)

% PROGRAMA: seqüència de clàusules


% UNIFICACIÓ

% Com executa l'intèrpret un programa Prolog?
% Busca la primera clàusula el cap de la qual sigui
% unificable amb l'objectiu

% "unificar" = donades dues expressions, donar valors a les seves
% variables perquè siguin iguals

%  el símbol = vol dir "és unificable"

% | ?- f(X,a) = f(b,Y).
% | ?- f(f(X),a)  =  f(Y,Y).
% | ?- f(f(X),a) \=  f(Y,Y).
% | ?- f(f(X),a)  =  f(Y,Z).

% Seria certa la igualtat següent?
% estima(maria,joan) = estima(Joan,Maria).

% Un altre exemple d'unificació
% f(a,g(X,Y)) = f(X,Z), Z = g(W,h(X)).

% EXECUCIÓ D'OBJECTIUS
% Quan un objectiu s'unifica amb el cap d'una regla,
% es fan les instanciacions de variables dins del cos de la regla
% i això dona subobjectius que cal satisfer.
% Prolog sempre prova d'unificar l'objectiu en l'ordre
% en què apareixen les regles.

% L'argument clàssic
% Tots els homes són mortals
% Sòcrates és una home
% -----
% Per tant, Sòcrates és mortal
%
% es tradueix en Prolog com

mortal(X) :- home(X).
home(socrates).

% Ara, si preguntem
% ?- mortal(socrates).
% true.
%
% com fa Prolog la recerca d'una demostració?


% LLISTES: seqüències de termes entre parèntesis quadrats
% [elefant, cavall, ruc, gos]
% Poden ser més complexes:
% [elefant, [], X, pare(joan,Y), [a, b], f(67)]
% [a, b, f(a), g(f(a,X)), [c], X]

% Notació
% [] és la llista buida
% [ a, b | L ] 
% la | separa els primers elements de la resta de la llista

% ?- [a,b,c] = [X|L].

% nota: en realidad una lista [a,b,c] es una notación elegante para el término:   .( a, .(b, .(c,[]) ) )

% Prolog és programació declarativa, no imperativa
% Això fa que els programes siguin versàtils: la mateixa definició
% serveix per molts tipus de consultes.
% Per exemple, podem declarar (definir) què és pertànyer a una llista:
% pert(X,L): X pertany a la llista L
% predefinit: member/2

pert(X,[X|_]).
pert(X,[_|L]):- pert(X,L).

% concat(L1,L2,L3) = "L3 és la concatenació de L1 amb L2"     
% predefinit: append/3        

concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

% últim d'una llista
% predefinit: last/2

ultim(X,Y) :- concat(_,[Y],X).

% llista inversa

invers([],[]).
invers(X,[Y|Z]) :-
    append(T,[Y],X),
    invers(T,Z).

% permutacio(L,P) = "P és una permutació de la llista L"
% (predefinit: permutation/2)

permutacio([],[]).
permutacio([X|L], P):- 
	permutacio(L,P1),     
	concat(Pa,Pb,P1),
	concat(Pa,[X|Pb],P).

% podem generar totes les permutacions de [a,b,c] amb la consulta
% permutacio([a,b,c],S), write(S), nl, false.

% ARITMÈTICA

% Podem provar el següent:
% ?- 7 + 4 = 11.
% false.

% Prolog ha provat d'unificar 7 + 4 amb 11 i no és possible
% Si provem el següent
% ?- 1 + 3 = 1 + N.
% N = 3.
% veiem que Prolog mira d'unificar els dos termes
% (són termes compostos: ?- compound(1+3). dona true.)

% Per tant, 1 + 3 \= 3 + 1.

% Si el que volem és avaluar expressions, fem servir l'operador is

% Var is Expressió   = "unifica el resultat d'avaluar Expressió amb Var"

% X is 7 + 4.
% X = 11.

% Ara podem definir, per exemple, el factorial:
% fact(N,F) = "F és el factorial de N"  
% F serà  N * (N-1) * ... * 1

fact(0,1):- !.
fact(N,F):- N1 is N-1,  fact(N1,F1), F is N * F1.

% Notem que N no es pot deixar sense instanciar perquè si no
% l'operador "is" no podria avaluar el segon argument

% mida(L,N) = "la mida de L és N"       
% predefinit: length/2

mida([],0).
mida([_|L],N):- mida(L,N1), N is N1+1.

% CONJUNTS
% subcjt(L,S) = "S és un subconjunt de L"
% un conjunt de n elements té 2^n subconjunts
% predefinit (amb els arguments intercanviats): subset/2

subcjt([],[]).
subcjt([_|L],S):- 
	subcjt(L,S).
subcjt([X|L],[X|S]):- 
	subcjt(L,S).

% podem generar tots els subconjunts de [a,b,c,d,e,f] amb
% subcjt([a,b,c,d,e,f],S), write(S), write(" "), false.

% reunio(L1,L2,R) == "R es la reunió de L1 amb L2"
% (com a conjunts, sense repeticions)
% predefinit: union/2

reunio([],L,L).
reunio([X|L1],L2,U) :-     
	pert(X,L2),   
	reunio(L1,L2,U).
reunio([X|L1],L2,[X|U]) :- 
	not(pert(X,L2)),  	
	reunio(L1,L2,U).
