:- dynamic col/1.

isCol(1).
isCol(2).
isCol(3).
isCol(4).
isCol(5).
isCol(6).
isCol(7).

% COL correspond à la colonne qui vient d etre jouée
% P correspond au joueur

initCol(COL) :- assert(col(COL,[0,0,0,0,0,0])).
initGame :- initCol(1), initCol(2), initCol(3), initCol(4), initCol(5), initCol(6), initCol(7).


% afficher le jeu

printColElement(COL, LINE) :- col(COL,X), nth0(LINE,X,Val), Val==0, write('-').
printColElement(COL, LINE) :- col(COL,X), nth0(LINE,X,Val), Val\==0, write(Val).
printLine(LINE) :- printColElement(1, LINE),
                   printColElement(2, LINE),
                   printColElement(3, LINE),
                   printColElement(4, LINE),
                   printColElement(5, LINE),
                   printColElement(6, LINE),
                   printColElement(7, LINE).

displayGame :-  writeln('       '),
                writeln('1234567'),
                writeln('       '),
                printLine(0), writeln(''),
                printLine(1), writeln(''),
                printLine(2), writeln(''),
                printLine(3), writeln(''),
                printLine(4), writeln(''),
                printLine(5), writeln(''),
                writeln('       ').

% jouer un coup en précisant la colonne et le joueur

replaceWhenZeroFound([A|Y], P, R, R2) :- A==0, append(R,[P],R1), append(R1,Y,R2).
replaceWhenZeroFound([A|Y], P, R, R2) :- A\==0, append(R,[A],R1), replaceWhenZeroFound(Y, P, R1, R2).
addPlayerCoin(X,P,R) :- reverse(X,Y), replaceWhenZeroFound(Y,P,[],R1), reverse(R1,R).
playInCol(COL, P) :- col(COL,X), addPlayerCoin(X,P,Y), retract(col(COL,X)), assert(col(COL,Y)).


% gagner avec une colonne :

winner(COL,P) :- col(COL,X), X = [A,B,C,D,E,F], (
                 (P==A, A==B, B==C, C==D, A\==0);
                 (P==B, B==C, C==D, D==E, B\==0);
                 (P==C, C==D, D==E, E==F, C\==0)
                 ).




% gagner avec une ligne :

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


% gagner avec une diagonale :

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


% faire un coup pour un joueur

play(P,C) :- (C==1;(C==2, P==1)), displayGame,
           write('Le joueur '), write(P), writeln(' doit jouer.'),
           write('Colonne choisie : '), read(COL),
           chooseCol(COL, P,C ).
% Partie IA        
play(P, C) :- ((C==2, P==2); C==3), displayGame,
           write('L\' IA '), write(P), writeln(' doit jouer.'),
           write('Colonne choisie : '), ia(P, _, C).


chooseCol(COL,P, C) :-((C==2, P==2); C==3), (not(isCol(COL)); isColFull(COL)), writeln('Impossible de jouer sur cette colonne.'), write('Colonne choisie : '), ia(P, _, C).
% Partie joueur humain
chooseCol(COL,P, C) :- isCol(COL), not(isColFull(COL)), playInCol(COL,P), continueGame(COL, P, C).
chooseCol(COL,P, C) :- (C==1;(C==2, P==1)), (not(isCol(COL)); isColFull(COL)), writeln('Impossible de jouer sur cette colonne.'), write('Colonne choisie : '), read(COL1), chooseCol(COL1, P).


ia(P, ChosenCol, C) :- repeat, ChosenCol is random(7),  writeln(ChosenCol), chooseCol(ChosenCol, P, C), !.

continueGame(_,_,_) :- isGameFull, displayGame, writeln('Pas de vainqueur.'), resetGame.
continueGame(COL,P,_) :- not(isGameFull), winner(COL,P), displayGame, write('Le joueur '), write(P), writeln(' a gagné'), resetGame.
continueGame(COL,P,C) :- not(isGameFull), not(winner(COL,P)), changePlayer(P,P1), play(P1,C).
changePlayer(P,P1) :- P==1, P1 = 2.
changePlayer(P,P1) :- P==2, P1 = 1.
resetCol(COL) :- col(COL,X), retract(col(COL,X)).
resetGame :- resetCol(1), resetCol(2), resetCol(3), resetCol(4), resetCol(5), resetCol(6), resetCol(7).


 

% lancer la partie

playGame :- initGame, choix(C), play(1,C).
choix(C) :- writeln('CHOIX 1 : 2 joueurs humains ?'),  writeln('CHOIX 2 : 1 joueur humain contre une IA ?'),writeln('CHOIX 3 : 2 IA ?'), writeln('Tapez votre choix : '), read(C).