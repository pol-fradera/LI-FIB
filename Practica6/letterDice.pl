:- use_module(library(clpfd)).

%% A (6-sided) "letter dice" has on each side a different letter.
%% Find four of them, with the 24 letters abcdefghijklmnoprstuvwxy such
%% that you can make all the following words: bake, onyx, echo, oval,
%% gird, smug, jump, torn, luck, viny, lush, wrap.

%Some helpful predicates:

word( [b,a,k,e] ).
word( [o,n,y,x] ).
word( [e,c,h,o] ).
word( [o,v,a,l] ).
word( [g,i,r,d] ).
word( [s,m,u,g] ).
word( [j,u,m,p] ).
word( [t,o,r,n] ).
word( [l,u,c,k] ).
word( [v,i,n,y] ).
word( [l,u,s,h] ).
word( [w,r,a,p] ).

num(X,N):- nth1( N, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y], X ).


main:-
    %1: Variables + Domini:
    length(D1,6),
    length(D2,6),
    length(D3,6),
    length(D4,6),

    append([D1,D2,D3,D4], D),
    D ins 1..24,
    
    %2: Constraints:
    all_different(D),
    % sorted(D1),
    % sorted(D2),
    % sorted(D3),
    % sorted(D4),

    findall(W, word(W), Words),
    different_dice(Words,D1,D2,D3,D4),

    %3: Labelling:
    label(D),

    %4: Escriure sol:
    writeN(D1), 
    writeN(D2), 
    writeN(D3), 
    writeN(D4), halt.
    
writeN(D):- findall(X,(member(N,D),num(X,N)),L), write(L), nl, !.

sorted([X1,X2,X3,X4,X5,X6]):- X1 #< X2, X2 #< X3, X3 #< X4, X4 #< X5, X5 #< X6.


different_dice([],_,_,_,_).
different_dice([W|Words],D1,D2,D3,D4):-
    noRepeatedLetters(W,D1),
    noRepeatedLetters(W,D2),
    noRepeatedLetters(W,D3),
    noRepeatedLetters(W,D4),
    different_dice(Words,D1,D2,D3,D4).

noRepeatedLetters([W1,W2,W3,W4],[S1,S2,S3,S4,S5,S6]):-
    num(W1,L1), num(W2,L2), num(W3,L3), num(W4,L4), 
    binaryCheck(S1,S2,L1,L2,L3,L4),
    binaryCheck(S1,S3,L1,L2,L3,L4),
    binaryCheck(S1,S4,L1,L2,L3,L4),
    binaryCheck(S1,S5,L1,L2,L3,L4),
    binaryCheck(S1,S6,L1,L2,L3,L4),
    binaryCheck(S2,S3,L1,L2,L3,L4),
    binaryCheck(S2,S4,L1,L2,L3,L4),
    binaryCheck(S2,S5,L1,L2,L3,L4),
    binaryCheck(S2,S6,L1,L2,L3,L4),
    binaryCheck(S3,S4,L1,L2,L3,L4),
    binaryCheck(S3,S5,L1,L2,L3,L4),
    binaryCheck(S3,S6,L1,L2,L3,L4),
    binaryCheck(S4,S5,L1,L2,L3,L4),
    binaryCheck(S4,S6,L1,L2,L3,L4),
    binaryCheck(S5,S6,L1,L2,L3,L4).

binaryCheck(S1,S2,L1,L2,L3,L4):-
    S1 #\= L1  #\/  S2 #\= L2,
    S1 #\= L1  #\/  S2 #\= L3,
    S1 #\= L1  #\/  S2 #\= L4,

    S1 #\= L2  #\/  S2 #\= L1,
    S1 #\= L2  #\/  S2 #\= L3,
    S1 #\= L2  #\/  S2 #\= L4,

    S1 #\= L3  #\/  S2 #\= L1,
    S1 #\= L3  #\/  S2 #\= L2,
    S1 #\= L3  #\/  S2 #\= L4,

    S1 #\= L4  #\/  S2 #\= L1,
    S1 #\= L4  #\/  S2 #\= L2,
    S1 #\= L4  #\/  S2 #\= L3.