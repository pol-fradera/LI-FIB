%% Operadores matemáticos: +, -, *, /, mod, div, ** -> exponenciacio  8 is 2**3
% / -> divisió amb decimals
% div -> divisió amb enters (elimina els decimals)


% negació not o \+

%% Comparadors aritmètics (per fer if)  (numerics)
%   =:= Igualdad Aritmética.
%   =\= Distinto
%   > Mayor que
%   < Menor que
%   >= Mayor o igual
%   =< Menor o igual

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% * Restriccions aritmètiques
% E1 #= E2 força a que E1 sigui igual a E2
% E1 #\= E2 força a que E1 sigui diferent a E2
% E1 #< E2 ...
% E1 #> E2
% E1 #=< E2
% E1 #>= E2

% * Restriccions booleanes
% 0 fals
% 1 cert
% #\ E no E
% E1 #/\ E2 E1 and E2
% E1 #\/ E2 E1 or E2
% E1 #==> E2 E1 implica E2
% E1 #<==> E2 E1 equivalent a E2

all_different(List). % força a que totes les variables de List preguin valors diferents.

all_distinct(List).  % també força a que totes les variables de List
                    % preguin valors diferents, però fa una propagació
                    % més potent (i més costosa). Per exemple,
                    % si tenim X in 1..2, Y in 1..2, Z in 1..3,
                    % all_distinct([X,Y,Z]) és capaç d’eliminar els valors 1 i 2
                    % del domini de Z, i all_different no.

element(I, L, X). % força a que X sigui igual al I−èsim element
                 % (començant per 1) de la llista L.

sum(+Vars, +Rel, ?Expr).
% sum([A,B,C], #=, 100)  

scalar_product(+Cs, +Vs, +Rel, ?Expr).
%scalar_product(Weight, Vars, #=<, Capacity)

labeling(_,Vars).
% The variable selection strategy lets you specify which variable of Vars is labeled next and is one of:
leftmost % Label the variables in the order they occur in Vars. This is the default.
ff % First fail. Label the leftmost variable with smallest domain next, in order to detect infeasibility early. This is often a good strategy.
ffc %Of the variables with smallest domains, the leftmost one participating in most constraints is labeled next.
min % Label the leftmost variable whose lower bound is the lowest next.
max % Label the leftmost variable whose upper bound is the highest next.

%The value order is one of:
up % Try the elements of the chosen variable's domain in ascending order. This is the default.
down %Try the domain elements in descending order.

% The branching strategy is one of:
step % For each variable X, a choice is made between X = V and X #\= V, where V is determined by the value ordering options. This is the default.
enum % For each variable X, a choice is made between X = V_1, X = V_2 etc., for all values V_i of the domain of X. The order is determined by the value ordering options.
bisect % For each variable X, a choice is made between X #=< M and X #> M, where M is the midpoint of the domain of X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Unificador '='
%% Mirar que no siguin unificables '/='
%% Mirar que no siguin iguals '\='  (no necessariament numerics)

% -A,-B   -A o -B   no es poden satisfer els dos alhora
% A i B -> C      -(A i B) o C       -A o -B o C    -A,-B,C

member(X,L). % Verifica si un elemento pertenece a una lista.
member(2, [1,2,3]). -> true

append(L1,L2,LA). % Concatena dos listas en una tercera lista.
append([1,2], [3,4], LA). -> LA = [1,2,3,4]

length(L,N). % Calcula la longitud de una lista.
length([1,2,3], N). -> N = 3

reverse(L,LR). % Invierte el orden de una lista.
reverse([1,2,3], LR). -> LR = [3,2,1]

nth0(I,L,X). % Obtiene el elemento en la posición dada de una lista. Los índices comienzan en 0.
nth0(2, [1,2,3], X). -> X = 3

nth1(I,L,X). % Obtiene el elemento en la posición dada de una lista. Los índices comienzan en 1.
nth1(2, [1,2,3], X). -> X = 2

last(L,X). % Obtiene el último elemento de una lista.
last([1,2,3], X). -> X = 3

delete(L,X,LD). % Elimina todas las ocurrencias de un elemento de una lista.
delete([1,2,3,2], 2, LD). -> LD = [1,3]

select(X,L,LS). % Elimina la primera ocurrencia de un elemento de una lista.
select(2, [1,2,3,2], LS). -> LS = [1,3,2]

subset(L1,L2). % Verifica si una lista es un subconjunto de otra lista.
subset([1,2], [1,2,3]). -> true

union(L1,L2,LU). % Obtiene la unión de dos listas.
union([1,2], [2,3], LU). -> LU = [1,2,3]

intersection(L1,L2,LI). % Obtiene la intersección de dos listas.
intersection([1,2], [2,3], LI). -> LI = [2]

permutacion(L,LP). % Comprueba si una lista es una permutación de otra lista.
permutation([1,2,3], [2,3,1]). -> true

sort(L,LS). % Ordena una lista en orden ascendente, eliminando duplicados.
sort([3,2,1,2,3,4,1], L). -> L = [1, 2, 3, 4]

is_list(X). % Verifica si un término es una lista.
is_list([1,2,3]). -> true

sum_list(L,Sum). % Suma los elementos de una lista.
sum_list([1,2,3], Sum). -> Sum = 6

flatten(L,LF). % Aplana una lista de listas en una lista simple.
flatten([1,[2,3],[4,[5,[6]]]], LF). -> Flat = [1, 2, 3, 4, 5, 6]

pescalar(L1,L2,P). % Calcula el producto escalar de dos listas numéricas del mismo tamaño
pescalar([1,2,3], [4,5,6], P) -> P = 32.

forall(Condition, Acction). % Verifica si una determinada condición es verdadera para todos los elementos de una lista.
forall(member(X,Rest), S+X > K).

findall(Template, Condition, List). % Permite encontrar todas las soluciones posibles que satisfacen una consulta, y almacenarlas en una lista.
persona(juan, 25, hombre).
persona(maria, 30, mujer).
findall(Nombre, persona(Nombre, _, _), ListaPersonas). -> ListaPersonas = [juan, maria, pedro, laura].

between(Lower, Upper, Var). % Genera números enteros en un rango dado.
between(1, 5, X). -> X = 1 ; X = 2 ; X = 3 ; X = 4 ; X = 5.

setof(Template, Goal, Set) % Genera una lista ordenada de soluciones únicas para una consulta dada, eliminando las soluciones duplicadas.
setof(P,(permutation(L,P), es_palindromo(P)),S).