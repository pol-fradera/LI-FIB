programa(P):- append([[begin],L,[end]],P), instrucciones(L).

instrucciones(L) :- instruccion(L).
%instrucciones(L) :- append(L1,L2,L), L2 = [;|L3], instruccion(L1),  instrucciones(L3).
%instrucciones(L) :- append(L1,[;|L3],L), instruccion(L1),  instrucciones(L3).
instrucciones(L) :- append([L1,[;],L2],L), instruccion(L1),  instrucciones(L2).

instruccion(L):- L = [V1,=,V2,+,V3], variable(V1), variable(V2), variable(V3).
instruccion(L):- append([[if],[V1],[=],[V2],[then],L1,[else],L2,[endif]],L), variable(V1), variable(V2), V1 = V2,
                 instrucciones(L1), instrucciones(L2).

variable(x).
variable(y).
variable(z).

% programa( [begin, x, =, x, +, z, end] ).
% programa( [begin, x, =, x, +, y, ;, z, =, z, +, z, ;, x, =, y, +, x, end])
% programa( [begin, x, =, y, +, z, ;, if, z, =, z, then, x, =, x, +, z, ;, y, =, y, +, z, else, z, =, z, +, y, endif, ;, x, =, x, +, z, end])