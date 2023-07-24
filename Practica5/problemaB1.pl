% estat: [litres en el cub de 5, listres en el cub de 8]

main:- EstadoInicial = [0,0],     EstadoFinal = [0,4],
    between(1,1000,CosteMax),            % Buscamos solución de coste 0; si no, de 1, etc.
    camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
    reverse(Camino, Camino1), write(Camino1), write(' con coste '), write(CosteMax), nl, halt.

camino( 0, E,E, C,C ).              % Caso base: cuando el estado actual es el estado final.
camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    CosteMax>0,
    unPaso( CostePaso, EstadoActual, EstadoSiguiente ),  % En B.1 y B.2, CostePaso es 1.
    \+member( EstadoSiguiente, CaminoHastaAhora ),
    CosteMax1 is CosteMax-CostePaso,
    camino( CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal ).

% transicions
unPaso(1,[N,M], [5,M]):- N < 5.   % omplir el cub de 5 litres (si no està ple)
unPaso(1,[N,M], [N,8]):- M < 8.   % omplir el cub de 8 litres (si no està ple)
unPaso(1,[N,M], [0,M]):- N > 0.   % buidar el cub de 5 litres (si no està buit)
unPaso(1,[N,M], [N,0]):- M > 0.   % buidar el cub de 8 litres (si no està buit)
unPaso(1,[N,M], [5,M1]):- N < 5, M >= (5-N), M1 is M - (5 - N).   % omplir completament el cub de 5 amb el de 8
unPaso(1,[N,M], [N1,8]):- M < 8, N >= (8-M), N1 is N - (8 - M).   % omplir completament el cub de 8 amb el de 5
unPaso(1,[N,M], [N1,0]):- N < 5, M < (5-N), N1 is N + M.   % omplir parcialment el cub de 5 amb el de 8
unPaso(1,[N,M], [0,M1]):- M < 8, N < (8-M), M1 is M + N.   % omplir parcialment el cub de 8 amb el de 5