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



% vérifier qu une colonne est bien une colonne du jeu
isCol(1).
isCol(2).
isCol(3).
isCol(4).
isCol(5).
isCol(6).
isCol(7).



% on intialise le jeu avec dans chaque colonne des cases égales à 0
initCol(COL) :- assert(col(COL,[0,0,0,0,0,0])), !.
initGame :- initCol(1), initCol(2), initCol(3), initCol(4), initCol(5), initCol(6), initCol(7), !.



% obtenir le plateau de jeu actuel sous forme de matrice

getGameBoard(GB):-  col(1,A),
                    col(2,B),
                    col(3,C),
                    col(4,D),
                    col(5,E),
                    col(6,F),
                    col(7,G),
                    GB=[A,B,C,D,E,F,G], !.



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


% anuller un coup en précisant la colonne jouée (utile dans le parcours des IA)

replaceFirstNotZero([A|Y], R, R2) :- A\==0, append(R,[0],R1), append(R1,Y,R2), !.
replaceFirstNotZero([A|Y], R, R2) :- A==0, append(R,[A],R1), replaceFirstNotZero(Y, R1, R2), !.
removePlayerCoin(X,R) :- replaceFirstNotZero(X,[],R), !.
cancelPlayInCol(COL) :- col(COL,X), removePlayerCoin(X,Y), retract(col(COL,X)), assert(col(COL,Y)), !.



% gagner avec une colonne pour un joueur P :

winner(COL,P) :- col(COL,X), X = [A,B,C,D,E,F], (
                 (P==A, A==B, B==C, C==D, A\==0);
                 (P==B, B==C, C==D, D==E, B\==0);
                 (P==C, C==D, D==E, E==F, C\==0)
                 ), !.


% gagner avec une ligne pour un joueur P :

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


% gagner avec une diagonale pour un joueur P :

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



% vérifier la victoire en regardant chaque colonne

checkWinning(P) :- (winner(1,P); winner(2,P); winner(3,P); winner(4,P); winner(5,P); winner(6,P); winner(7,P)), !.



% vérifier si une colonne est totalement remplie

noZeroFound([]).
noZeroFound([A|X]) :- A\==0, noZeroFound(X), !.
isColFull(COL) :- col(COL,X), noZeroFound(X), !.


% vérifier si le jeu est totalement rempli

isGameFull :- isColFull(1), isColFull(2), isColFull(3), isColFull(4), isColFull(5), isColFull(6), isColFull(7), !.




% Un joueur humain doit jouer

play(P,C,IA1,Depth1,IA2,Depth2) :- (C==1;(C==21,P==1);(C==22,P==2)), displayGame,
           write('Le joueur '), write(P), writeln(' doit jouer.'),
           write('Colonne choisie : '), read(COL),
           chooseCol(COL, P,C,IA1,Depth1,IA2,Depth2), !.



% Une IA doit jouer

play(P, C,IA1,Depth1,IA2,Depth2) :- ((C==21,P==2);(C==22,P==1);C==3), displayGame,
           write('L\' IA '), write(P), writeln(' doit jouer.'),
           write('Colonne choisie : '), ia(P, C,IA1,Depth1,IA2,Depth2), !.


% Un joueur humain doit choisir une colonne

chooseCol(COL,P, C,IA1,Depth1,IA2,Depth2) :- isCol(COL), not(isColFull(COL)), playInCol(COL,P), continueGame(COL, P, C, IA1,Depth1,IA2,Depth2), !.
chooseCol(COL,P, C,IA1,Depth1,IA2,Depth2) :- (not(isCol(COL)); isColFull(COL)), writeln('Impossible de jouer sur cette colonne.'), write('Colonne choisie : '), read(COL1), chooseCol(COL1, P, C,IA1,Depth1,IA2,Depth2), !.


% Une IA doit choisir la bonne IA/heuristique selon les paramètres choisis

ia(P, C,IA1,Depth1,IA2,Depth2) :- (P==1, playIA(IA1,Depth1,P,BestCol); playIA(IA2,Depth2,P,BestCol)), writeln(BestCol), playInCol(BestCol, P), continueGame(BestCol, P, C,IA1,Depth1,IA2,Depth2), !.


% Les différents choix pour une IA selon les paramètres : on appelle pour chaque choix la bonne heuristique

playIA(IA, DEPTH, P, BestCol) :- IA == 1, assert(maxDepth(DEPTH)), negamax(0, DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, DEPTH, P, BestCol) :- IA == 2, assert(maxDepth(DEPTH)), alphabeta(0, (-inf), (inf), DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, DEPTH, P, BestCol) :- IA == 3, assert(maxDepth(DEPTH)), negamax(1, DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, DEPTH, P, BestCol) :- IA == 4, assert(maxDepth(DEPTH)), alphabeta(1, (-inf), (inf), DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, DEPTH, P, BestCol) :- IA == 5, assert(maxDepth(DEPTH)), negamax(2, DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, DEPTH, P, BestCol) :- IA == 6, assert(maxDepth(DEPTH)), alphabeta(2, (-inf), (inf), DEPTH, P, P, _, BestCol), retract(maxDepth(DEPTH)), !.
playIA(IA, _, P, BestCol) :- IA == 7, getGameBoard(GB), heuristicAdj(GB, BestCol,P), !.


% Le joueur ou l IA a joué et on continue le jeu ou on arrête le jeu en cas de victoire ou d égalité

continueGame(_,_,_,_,_,_,_) :- isGameFull, displayGame, writeln('Pas de vainqueur.'), resetGame, !.
continueGame(COL,P,_,_,_,_,_) :- not(isGameFull), winner(COL,P), displayGame, write('Le joueur '), write(P), writeln(' a gagné'), resetGame, !.
continueGame(COL,P,C,IA1,Depth1,IA2,Depth2) :- not(isGameFull), not(winner(COL,P)), changePlayer(P,P1), play(P1,C,IA1,Depth1,IA2,Depth2), !.


% changer de joueur pour le coup suivant

changePlayer(P,P1) :- P==1, P1 = 2, !.
changePlayer(P,P1) :- P==2, P1 = 1, !.


% on reset le jeu à la fin de la partie en supprimant le contenu de chaque colonne du jeu

resetCol(COL) :- col(COL,X), retract(col(COL,X)), !.
resetGame :- resetCol(1), resetCol(2), resetCol(3), resetCol(4), resetCol(5), resetCol(6), resetCol(7), !.



% lancer la partie

playGame :- initGame, choice(C, IA1, Depth1, IA2, Depth2), play(1, C, IA1, Depth1, IA2, Depth2), !.


% choisir le mode de jeu

choice(FinalChoice, IA1, Depth1, IA2, Depth2) :- writeln('CHOIX 1 : 2 joueurs humains ?'),
            writeln('CHOIX 2 : 1 joueur humain contre une IA ?'),
            writeln('CHOIX 3 : 2 IA ?'),
            write('Tapez votre choix : '), read(Num), checkMode(Num, C), chooseFirstPlayer(C, FinalChoice), chooseIA(C, IA1, Depth1, IA2, Depth2), !.


% vérifier que le choix du mode de jeu soit correct

checkMode(Num, C) :- (((Num == 1 ; Num == 2 ; Num == 3), C = Num);
                     (writeln("Vous devez choisir entre les choix 1, 2 ou 3."), write('Tapez votre choix : '), read(Num1), checkMode(Num1, C))), !.


% choisir qui doit jouer en premier dans le cas où le mode sélectionne est humain contre IA

chooseFirstPlayer(C, FinalChoice) :- (((C==1;C==3), FinalChoice=C);(
                                      writeln(''),
                                      writeln('Quel joueur doit jouer en premier ?'),
                                      writeln('CHOIX 1 : joueur humain'),
                                      writeln('CHOIX 2 : IA'),
                                      write('Tapez votre choix : '),
                                      read(Num),
                                      checkNumFirstPlayer(Num, FinalChoice)
                                      )), !.


% vérifier que le choix du premier joueru soit correct

checkNumFirstPlayer(Num, FirstPlayer) :- (((Num == 1 ; Num == 2), FirstPlayer is Num + 20);
                                        (writeln("Vous devez choisir entres les choix 1 ou 2."), write('Tapez votre choix : '), read(Num1), checkNumFirstPlayer(Num1, FirstPlayer))), !.


% choisir une ou plusieurs IA dans le cas où un mode de jeu avec au moins une IA a été sélectionne

chooseIA(1,_,_,_,_).
chooseIA(C, IA1, Depth1, IA2, Depth2) :- (((C==2), writeIAChoice(_), read(Num), checkNumIA(Num, IA2), IA1=IA2, chooseDepth(IA2,Depth2), Depth1 = Depth2);
                         ((C==3), writeIAChoice(1), read(Num1), checkNumIA(Num1, IA1), chooseDepth(IA1,Depth1), writeIAChoice(2), read(Num2), checkNumIA(Num2, IA2), chooseDepth(IA2,Depth2))), !.


% choisir la profondeur de recherche dans le cas où une IA avec minmax ou alphabeta a été sélectionne

chooseDepth(IA, Depth) :- ((IA==7, Depth = 0);
                          (writeln(''), writeln('Choisissez la profondeur de recherche de l\'IA. Attention, pour des conditions de jeu optimales, vous ne pouvez pas choisir une profondeur supérieure à 4 pour les IA minmax et une profondeur supérieure à 6 pour les IA alphabeta.'),
                          write('Tapez votre choix : '), read(D), checkDepth(IA, D, Depth))), !.


% vérifier que la prondeur de recherche de l IA soit correcte

checkDepth(IA, D, Depth) :- (((IA==1;IA==3;IA==5), (((D==1;D==2;D==3;D==4), Depth = D); writeln('Vous devez choisir une pronfondeur entre 1 et 4 pour une IA minmax.'),write('Tapez votre choix : '), read(D1), checkDepth(IA, D1, Depth)));
                            ((IA==2;IA==4;IA==6), (((D==1;D==2;D==3;D==4;D==5;D==6), Depth = D); writeln('Vous devez choisir une pronfondeur entre 1 et 6 pour une IA alphabeta.'),write('Tapez votre choix : '), read(D1), checkDepth(IA, D1, Depth)))),!.


% proposer les différens choix d IA

writeIAChoice(Num) :- writeln(''),
                      write('Niveau IA '), (((Num==1; Num==2),write(Num),write(' '));write('')), writeln(':'),
                      writeln('CHOIX 1 : IA aléatoire minmax'),
                      writeln('CHOIX 2 : IA aléatoire alphabeta'),
                      writeln('CHOIX 3 : IA heuristique grille minmax'),
                      writeln('CHOIX 4 : IA heuristique grille alphabeta'),
                      writeln('CHOIX 5 : IA heuristique grille dynamique minmax'),
                      writeln('CHOIX 6 : IA heuristique grille dynamique alphabeta'),
                      writeln('CHOIX 7 : IA heuristique adjacence'),
                      write('Tapez votre choix : '), !.



% vérifier que le numéro de l IA sélectionne soit correct

checkNumIA(Num, IA) :- (((Num == 1 ; Num == 2 ; Num == 3 ; Num == 4 ; Num == 5 ; Num == 6 ; Num == 7), IA = Num);
                        (writeln("Vous devez choisir entres les choix 1, 2, 3, 4, 5, 6 ou 7."), write('Tapez votre choix : '), read(Num1), checkNumIA(Num1, IA))), !.
