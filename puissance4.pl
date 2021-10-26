:- dynamic col/1.

% COL correspond à la colonne qui vient d etre jouée
% P correspond au joueur

initCol(COL) :- assert(col(COL,[0,0,0,0,0,0])).
initGame :- initCol(1), initCol(2), initCol(3), initCol(4), initCol(5), initCol(6), initCol(7).

% jouer un coup en précisant la colonne et le joueur

replaceWhenZeroFound([A|Y], P, R, R2) :- A==0, append(R,[P],R1), append(R1,Y,R2).
replaceWhenZeroFound([A|Y], P, R, R2) :- A\==0, append(R,[A],R1), replaceWhenZeroFound(Y, P, R1, R2).
addPlayerCoin(X,P,R) :- reverse(X,Y), replaceWhenZeroFound(Y,P,[],R1), reverse(R1,R).
playInCol(COL, P) :- not(isColFull(COL)), col(COL,X), addPlayerCoin(X,P,Y), retract(col(COL,X)), assert(col(COL,Y)).


% gagner avec une colonne, tous les cas possibles :
% en 3 fois

%winner(COL,P) :- col(COL,X), X = [P,Q,R,S,_,_], P==Q, Q==R, R==S, P\==0.
%winner(COL,P) :- col(COL,X), X = [_,P,Q,R,S,_], P==Q, Q==R, R==S, P\==0.
%winner(COL,P) :- col(COL,X), X = [_,_,P,Q,R,S], P==Q, Q==R, R==S, P\==0.

% ganer avec une colonne, tous les cas possibles :
% en 1 fois
% contrainte : il faut toujours préciser le joueur

winner(COL,P) :- col(COL,X), X = [A,B,C,D,E,F], (
                 (P==A, A==B, B==C, C==D, A\==0);
                 (P==B, B==C, C==D, D==E, B\==0);
                 (P==C, C==D, D==E, E==F, C\==0)
                 ).



% gagner avec une ligne en bas à gauche de la colonne COL :
% on pourrait faire comme ceci pour cahque ligne gagnante mais ce serait très long...

%winner(COL,P) :- col(COL,W),  W = [_,_,_,_,_,P], COL1 is COL-1,
%                 col(COL1,X), X = [_,_,_,_,_,Q], COL2 is COL1-1,
%                 col(COL2,Y), Y = [_,_,_,_,_,R], COL3 is COL2-1,
%                 col(COL3,Z), Z = [_,_,_,_,_,S],
%                 P==Q, Q==R, R==S, P\==0.


% gagner avec une ligne, tous les cas possibles :
% en 1 fois
% contrainte : il faut toujours préciser la colonne et le joueur

winner(COL,P) :- P\== 0, (
                 (COL1 is COL+1, COL2 is COL+2, COL3 is COL+3);
                 (COL1 is COL-1, COL2 is COL+1, COL3 is COL+2);
                 (COL1 is COL-2, COL2 is COL-1, COL3 is COL+1);
                 (COL1 is COL-3, COL2 is COL-2, COL3 is COL-1)),
                 col(COL,W),  W = [A1,A2,A3,A4,A5,A6],
                 col(COL1,X), X = [B1,B2,B3,B4,B5,B6],
                 col(COL2,Y), Y = [C1,C2,C3,C4,C5,C6],
                 col(COL3,Z), Z = [D1,D2,D3,D4,D5,D6], (
                 (P==A1, A1==B1, B1==C1, C1==D1);
                 (P==A2, A2==B2, B2==C2, C2==D2);
                 (P==A3, A3==B3, B3==C3, C3==D3);
                 (P==A4, A4==B4, B4==C4, C4==D4);
                 (P==A5, A5==B5, B5==C5, C5==D5);
                 (P==A6, A6==B6, B6==C6, C6==D6)
                 ).


% gagner avec une diagonale, tous les cas possibles :
% en 1 fois
% contrainte : il faut toujours préciser la colonne et le joueur

winner(COL,P) :- P\==0, (
                (COL1 is COL+1, COL2 is COL+2, COL3 is COL+3);
                (COL1 is COL-1, COL2 is COL+1, COL3 is COL+2);
                (COL1 is COL-2, COL2 is COL-1, COL3 is COL+1);
                (COL1 is COL-3, COL2 is COL-2, COL3 is COL-1)),
                col(COL,W),  W = [A1,A2,A3,A4,A5,A6],
                col(COL1,X), X = [_,B2,B3,B4,B5,_],
                col(COL2,Y), Y = [_,C2,C3,C4,C5,_],
                col(COL3,Z), Z = [D1,D2,D3,D4,D5,D6], (
                (P==A1, A1==B2, B2==C3, C3==D4);
                (P==A2, A2==B3, B3==C4, C4==D5);
                (P==A3, A3==B4, B4==C5, C5==D6);
                (P==A4, A4==B3, B3==C2, C2==D1);
                (P==A5, A5==B4, B4==C3, C3==D2);
                (P==A6, A6==B5, B5==C4, C4==D3)
                ).


% vérifier si une colonne est totalement remplie

noZeroFound([]).
noZeroFound([A|X]) :- A\==0, noZeroFound(X).
isColFull(COL) :- col(COL,X), noZeroFound(X).

% vérifier si le jeu est totalement rempli

isGameFull :- isColFull(1), isColFull(2), isColFull(3), isColFull(4), isColFull(5), isColFull(6), isColFull(7).
