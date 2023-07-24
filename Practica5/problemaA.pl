solucio( SOL ) :-
    SOL = [ [1,_C1,_P1,_A1,_B1,_N1],  % [numcasa,color,profesión,animal,bebida,pais]
            [2,_C2,_P2,_A2,_B2,_N2],
            [3,_C3,_P3,_A3,_B3,_N3],
            [4,_C4,_P4,_A4,_B4,_N4],
            [5,_C5,_P5,_A5,_B5,_N5] ],
    member( [_,roja,_,_,_,peru], SOL), % 1 - El que vive en la casa roja es de Perú
    member( [_,_,_,perro,_,francia], SOL), % 2 - Al francés le gusta el perro
    member( [_,_,pintor,_,_,japon], SOL), % 3 - El pintor es japonés
    member( [_,_,_,_,ron,china], SOL), % 4 - Al chino le gusta el ron
    member( [1,_,_,_,_,hungria], SOL), % 5 - El húngaro vive en la primera casa
    member( [_,verde,_,_,conac,_], SOL), % 6 - Al de la casa verde le gusta el coñac
    member( [NV,verde,_,_,_,_], SOL), % 7 - La casa verde está justo a la izquierda de la blanca
    member( [NB,blanca,_,_,_,_], SOL), % 7 - La casa verde está justo a la izquierda de la blanca
    NB is NV+1,
    member( [_,_,escultor,caracoles,_,_], SOL), % 8 - El escultor crı́a caracoles
    member( [_,amarilla,actor,_,_,_], SOL), % 9 - El de la casa amarilla es actor
    member( [3,_,_,_,cava,_], SOL), % 10 - El de la tercera casa bebe cava
    member( [NA,_,actor,_,_,_], SOL), % 11 - El que vive al lado del actor tiene un caballo
    member( [NC,_,_,caballo,_,_], SOL), % 11 - El que vive al lado del actor tiene un caballo
    (NC is NA+1 ; NC is NA -1),
    member( [NH,_,_,_,_,hungria], SOL), % 12 - El húngaro vive al lado de la casa azul
    member( [NAZ,azul,_,_,_,_], SOL), % 12 - El húngaro vive al lado de la casa azul
    (NH is NAZ+1 ; NH is NAZ-1),
    member( [_,_,notario,_,whisky,_], SOL), % 13 - Al notario la gusta el whisky
    member( [NAR,_,_,ardilla,_,_], SOL), % 14 - El que vive al lado del médico tiene un ardilla,
    member( [NM,_,medico,_,_,_], SOL), % 14 - El que vive al lado del médico tiene un ardilla,
    (NAR is NM+1 ; NAR is NM-1),
    writeSol(SOL), !.

    % ; per dir que un predicat o l'atre s'ha de satisfer (es una or), s'han de fer servir parentesis
    % , es una and

    writeSol(Sol):-
        member(Casa, Sol), write(Casa), nl, fail.
    writeSol(_).