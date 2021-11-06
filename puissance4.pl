:- module(puissance4, [
  playInCol/2,
  cancelPlayInCol/1,
  isColFull/1,
  isGameFull/0,
  changePlayer/2,
  getGameBoard/1,
  checkWinning/1,
  maxDepth/1,
  col/2
]).

:- use_module(negaMax).
:- use_module(alphaBeta).
:- use_module(adjHeuristic).

:- dynamic col/2.
:- dynamic maxDepth/1.

isCol(1).
isCol(2).
isCol(3).
isCol(4).
isCol(5).
isCol(6).
isCol(7).

% COL correspond à la colonne qui vient d etre jouée
% P correspond au joueur

initCol(COL) :- assert(col(COL,[0,0,0,0,0,0])), !.
initGame :- initCol(1), initCol(2), initCol(3), initCol(4), initCol(5), initCol(6), initCol(7), !.


% afficher le jeu

printColElement(COL, LINE) :- col(COL,X), nth0(LINE,X,Val), Val==0, write(' - '), !.
printColElement(COL, LINE) :- col(COL,X), nth0(LINE,X,Val), Val\==0, write(' '), write(Val), write(' '), !.
printLine(LINE) :- printColElement(1, LINE),
                   printColElement(2, LINE),
                   printColElement(3, LINE),
                   printColElement(4, LINE),
                   printColElement(5, LINE),
                   printColElement(6, LINE),
                   printColElement(7, LINE), !.

displayGame :-  writeln('                     '),
                writeln(' 1  2  3  4  5  6  7 '),
                writeln('---------------------'),
                printLine(0), writeln(''),
                printLine(1), writeln(''),
                printLine(2), writeln(''),
                printLine(3), writeln(''),
                printLine(4), writeln(''),
                printLine(5), writeln(''),
                writeln('---------------------'),
                writeln('                     '), !.

% jouer un coup en précisant la colonne et le joueur

replaceWhenZeroFound([A|Y], P, R, R2) :- A==0, append(R,[P],R1), append(R1,Y,R2), !.
replaceWhenZeroFound([A|Y], P, R, R2) :- A\==0, append(R,[A],R1), replaceWhenZeroFound(Y, P, R1, R2), !.
addPlayerCoin(X,P,R) :- reverse(X,Y), replaceWhenZeroFound(Y,P,[],R1), reverse(R1,R), !.
playInCol(COL, P) :- col(COL,X), addPlayerCoin(X,P,Y), retract(col(COL,X)), assert(col(COL,Y)), !.

replaceFirstNotZero([A|Y], R, R2) :- A\==0, append(R,[0],R1), append(R1,Y,R2), !.
replaceFirstNotZero([A|Y], R, R2) :- A==0, append(R,[A],R1), replaceFirstNotZero(Y, R1, R2), !.
removePlayerCoin(X,R) :- replaceFirstNotZero(X,[],R), !.
cancelPlayInCol(COL) :- col(COL,X), removePlayerCoin(X,Y), retract(col(COL,X)), assert(col(COL,Y)), !.

% gagner avec une colonne :

winner(COL,P) :- col(COL,X), X = [A,B,C,D,E,F], (
                 (P==A, A==B, B==C, C==D, A\==0);
                 (P==B, B==C, C==D, D==E, B\==0);
                 (P==C, C==D, D==E, E==F, C\==0)
                 ), !.


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
                 ), !.


% gagner avec une diagonale :

winner(COL,P) :- P\==0, (
                (COL1 is COL, COL2 is COL+1, COL3 is COL+2, COL4 is COL+3);
                (COL1 is COL-1, COL2 is COL, COL3 is COL+1, COL4 is COL+2);
                (COL1 is COL-2, COL2 is COL-1, COL3 is COL, COL4 is COL+1);
                (COL1 is COL-3, COL2 is COL-2, COL3 is COL-1, COL4 is COL)),
                col(COL1,W),  W = [A1,A2,A3,A4,A5,A6],
                col(COL2,X), X = [_,B2,B3,B4,B5,_],
                col(COL3,Y), Y = [_,C2,C3,C4,C5,_],
                col(COL4,Z), Z = [D1,D2,D3,D4,D5,D6], (
                (P==A1, A1==B2, B2==C3, C3==D4);
                (P==A2, A2==B3, B3==C4, C4==D5);
                (P==A3, A3==B4, B4==C5, C5==D6);
                (P==A4, A4==B3, B3==C2, C2==D1);
                (P==A5, A5==B4, B4==C3, C3==D2);
                (P==A6, A6==B5, B5==C4, C4==D3)
                ), !.


% vérifier si une colonne est totalement remplie

noZeroFound([]).
noZeroFound([A|X]) :- A\==0, noZeroFound(X), !.
isColFull(COL) :- col(COL,X), noZeroFound(X), !.

% vérifier si le jeu est totalement rempli

isGameFull :- isColFull(1), isColFull(2), isColFull(3), isColFull(4), isColFull(5), isColFull(6), isColFull(7), !.


% faire un coup pour un joueur

% Partie joueur humain

play(P,C,IA1,IA2) :- (C==1;(C==2, P==1)), displayGame,
           write('Le joueur '), write(P), writeln(' doit jouer.'),
           write('Colonne choisie : '), read(COL),
           chooseCol(COL, P,C,IA1,IA2), !.

% Partie IA

play(P, C,IA1,IA2) :- ((C==2, P==2); C==3), displayGame,
           write('L\' IA '), write(P), writeln(' doit jouer.'),
           write('Colonne choisie : '), ia(P, C,IA1,IA2), !.


chooseCol(COL,P, C,IA1,IA2) :- isCol(COL), not(isColFull(COL)), playInCol(COL,P), continueGame(COL, P, C, IA1,IA2), !.
chooseCol(COL,P, C,IA1,IA2) :- (not(isCol(COL)); isColFull(COL)), writeln('Impossible de jouer sur cette colonne.'), write('Colonne choisie : '), read(COL1), chooseCol(COL1, P, C,IA1,IA2), !.

ia(P, C,IA1,IA2) :- (P==1, playIA(IA1,P,BestCol); playIA(IA2,P,BestCol)), writeln(BestCol), playInCol(BestCol, P), continueGame(BestCol, P, C,IA1,IA2), !.

playIA(IA, P, BestCol) :- IA == 1, DEPTH = 4, assert(maxDepth(DEPTH)), negamax(0, DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, P, BestCol) :- IA == 2, DEPTH = 5, assert(maxDepth(DEPTH)), alphabeta(0, (-inf), (inf), DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, P, BestCol) :- IA == 3, DEPTH = 4, assert(maxDepth(DEPTH)), negamax(1, DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, P, BestCol) :- IA == 4, DEPTH = 5, assert(maxDepth(DEPTH)), alphabeta(1, (-inf), (inf), DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, P, BestCol) :- IA == 5, DEPTH = 4, assert(maxDepth(DEPTH)), negamax(2, DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, P, BestCol) :- IA == 6, DEPTH = 5, assert(maxDepth(DEPTH)), alphabeta(2, (-inf), (inf), DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, P, BestCol) :- IA == 7, getGameBoard(GB), heuristicAdj(GB, BestCol,P), !.

continueGame(_,_,_,_,_) :- isGameFull, displayGame, writeln('Pas de vainqueur.'), resetGame, !.
continueGame(COL,P,_,_,_) :- not(isGameFull), winner(COL,P), displayGame, write('Le joueur '), write(P), writeln(' a gagné'), resetGame, !.
continueGame(COL,P,C,IA1,IA2) :- not(isGameFull), not(winner(COL,P)), changePlayer(P,P1), play(P1,C,IA1,IA2), !.
changePlayer(P,P1) :- P==1, P1 = 2, !.
changePlayer(P,P1) :- P==2, P1 = 1, !.
resetCol(COL) :- col(COL,X), retract(col(COL,X)), !.
resetGame :- resetCol(1), resetCol(2), resetCol(3), resetCol(4), resetCol(5), resetCol(6), resetCol(7), !.



% lancer la partie

playGame :- initGame, choice(C, IA1, IA2), play(1, C, IA1, IA2), !.
choice(C, IA1, IA2) :- writeln('CHOIX 1 : 2 joueurs humains ?'),
            writeln('CHOIX 2 : 1 joueur humain contre une IA ?'),
            writeln('CHOIX 3 : 2 IA ?'),
            write('Tapez votre choix : '), read(Num), checkMode(Num, C), chooseIA(C, IA1, IA2), !.
checkMode(Num, C) :- ((Num == 1 ; Num == 2 ; Num == 3), C = Num);
                     (writeln("Vous devez choisir entres les choix 1, 2 ou 3."), write('Tapez votre choix : '), read(Num1), checkMode(Num1, C)), !.

chooseIA(1,_,_).
chooseIA(C, IA1, IA2) :- ((C==2), writeIAChoice(2), read(Num), checkNumIA(Num, IA2), IA1=0);
                         ((C==3), writeIAChoice(1), read(Num1), checkNumIA(Num1, IA1),  writeIAChoice(2), read(Num2), checkNumIA(Num2, IA2)), !.

writeIAChoice(Num) :- write('Niveau IA '), (((Num==1; Num==2),write(Num));write()), writeln(' :'),
                      writeln('CHOIX 1 : IA aléatoire minmax'),
                      writeln('CHOIX 2 : IA aléatoire alphabeta'),
                      writeln('CHOIX 3 : IA heuristique grille minmax'),
                      writeln('CHOIX 4 : IA heuristique grille alphabeta'),
                      writeln('CHOIX 5 : IA heuristique grille dynamique minmax'),
                      writeln('CHOIX 6 : IA heuristique grille dynamique alphabeta'),
                      writeln('CHOIX 7 : IA heuristique adjacence'),
                      write('Tapez votre choix : '), !.

checkNumIA(Num, IA) :- (((Num == 1 ; Num == 2 ; Num == 3 ; Num == 4 ; Num == 5 ; Num == 6 ; Num == 7), IA = Num);
                        (writeln("Vous devez choisir entres les choix 1, 2, 3, 4, 5, 6 ou 7."), write('Tapez votre choix : '), read(Num1), checkNumIA(Num1, IA))), !.


getGameBoard(GB):-  col(1,A),
                    col(2,B),
                    col(3,C),
                    col(4,D),
                    col(5,E),
                    col(6,F),
                    col(7,G),
                    GB=[A,B,C,D,E,F,G], !.

checkWinning(P) :- winner(1,P); winner(2,P); winner(3,P); winner(4,P); winner(5,P); winner(6,P); winner(7,P), !.
