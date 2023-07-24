:- use_module(library(clpfd)).

ejemplo(0,   26, [1,2,5,10] ).  % Solution: [1,0,1,2]
ejemplo(1,  361, [1,2,5,13,17,35,157]).

main:- 
    ejemplo(0,Amount,Coins),
    nl, write('Paying amount '), write(Amount), write(' using the minimal number of coins of values '), write(Coins), nl,nl,
    % 1. vars + domain:
    length(Coins,N), 
    % 1a. variables
    length(Vars,N), % get list of N prolog vars    
    % 1b. domain:
    Vars ins 0..Amount,
    % 2. contraints:
    expr_prod_escalar(Vars, Coins, Expr),
    Amount #= Expr,
    % 3. lebaling
    expr_suma(Vars,ExprSuma),
    labeling([min(ExprSuma)],Vars),
    % 4. write soluction
    nl, write(Vars), nl,nl, halt.


expr_prod_escalar([X], [C], X*C).
expr_prod_escalar([X|LX], [C|LC], X*C+Expr):- expr_prod_escalar(LX,LC,Expr).

% sum_list (no es pot utilitzar si no estan tots els elements instanciats)
expr_suma([X], X).
expr_suma([X|LX], X+SX):- expr_suma(LX,SX).