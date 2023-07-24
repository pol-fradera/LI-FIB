% estat: [P1, P2, P5, P8, llanterna] 0(esq) 1(dre)

main:- EstadoInicial = [0,0,0,0,l],     EstadoFinal = [1,1,1,1,r],
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
unPaso(8, [P1,P2,P5,0,l], [P12,P22,P52,1,r]):- between(0,1,P12), P12 >= P1, between(0,1,P22), P22 >= P2, 
                                               between(0,1,P52), P52 >= P5, X is P12-P1+P22-P2+P52-P5, X =< 1.

unPaso(5, [P1,P2,0,P8,l], [P12,P22,1,P82,r]):- P8 = P82, between(0,1,P12), P12 >= P1, between(0,1,P22), P22 >= P2, 
                                               X is P12-P1+P22-P2, X =< 1.          

unPaso(2, [P1,0,P5,P8,l], [P12,1,P52,P82,r]):- P8 = P82, P5 = P52, between(0,1,P12), P12 >= P1,
                                               X is P12-P1, X =< 1.   

unPaso(1, [0,P2,P5,P8,l], [1,P22,P52,P82,r]):- P8 = P82, P5 = P52, P2 = P22.    


unPaso(8, [P12,P22,P52,1,r], [P1,P2,P5,0,l]):- between(0,1,P1), P12 >= P1, between(0,1,P2), P22 >= P2, 
                                               between(0,1,P5), P52 >= P5, X is P12-P1+P22-P2+P52-P5, X =< 1.

unPaso(5, [P12,P22,1,P82,r], [P1,P2,0,P8,l]):- P8 = P82, between(0,1,P1), P12 >= P1, between(0,1,P2), P22 >= P2, 
                                               X is P12-P1+P22-P2, X =< 1.          

unPaso(2, [P12,1,P52,P82,r], [P1,0,P5,P8,l]):- P8 = P82, P5 = P52, between(0,1,P1), P12 >= P1,
                                               X is P12-P1, X =< 1.   

unPaso(1, [1,P22,P52,P82,r], [0,P2,P5,P8,l]):- P8 = P82, P5 = P52, P2 = P22.