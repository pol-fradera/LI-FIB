% estat: [missioners, canibals, costat]
% estat: [missioners_esq, canibals_esq, barca]

main:- EstadoInicial = [3,3,l],     EstadoFinal = [0,0,r], %[3,3,r],
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

% transicions estat: [missioners, canibals, costat]
% unPaso(1,[M0,C0,l], [M1,C1,r]):- between(0,3,M1), Mb is M1 - 3 + M0, Mb >= 0, between(0,3,C1), Cb is C1 - 3 + C0, Cb >= 0,
%                                  B is Mb + Cb, (B = 1 ; B = 2),
%                                  Ml is M0 - Mb, Cl is C0 - Cb, 
%                                  (Ml = 0 ; Ml >= Cl),
%                                  (M1 = 0; M1 >= C1).
% unPaso(1,[M0,C0,r], [M1,C1,l]):- between(0,3,M1), Mb is M1 - 3 + M0, Mb >= 0, between(0,3,C1), Cb is C1 - 3 + C0, Cb >= 0,
%                                  B is Mb + Cb, (B = 1 ; B = 2),
%                                  Ml is M0 - Mb, Cl is C0 - Cb, 
%                                  (Ml = 0 ; Ml >= Cl),
%                                  (M1 = 0; M1 >= C1).

% transicions estat: [missioners_esq, canibals_esq, barca]
unPaso(1,[M0,C0,l], [M1,C1,r]):- between(0,3,M1), M1 =< M0, Mb is M0-M1, between(0,3,C1), C1 =< C0, Cb is C0-C1,
                                B is Mb + Cb, (B = 1 ; B = 2),
                                Mr is 3 - M1, Cr is 3 - C1, 
                                (M1 = 0; M1 >= C1),
                                (Mr = 0 ; Mr >= Cr).
                                
unPaso(1,[M0,C0,r], [M1,C1,l]):- between(0,3,M1), M1 >= M0, Mb is M1-M0, between(0,3,C1), C1 >= C0, Cb is C1-C0,
                                B is Mb + Cb, (B = 1 ; B = 2),
                                Mr is 3 - M1, Cr is 3 - C1, 
                                (M1 = 0; M1 >= C1),
                                (Mr = 0 ; Mr >= Cr).