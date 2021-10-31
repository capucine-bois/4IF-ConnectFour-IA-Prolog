:- module(negaMax, [
  negamax/5
]).

:- use_module(puissance4).

% algorithme minimax de profondeur limitée avec simplification negamax

negamax(DEPTH, Pmax, P, SCORE, _) :- checkWinning(Pmax), ((Pmax\==P,SCORE is (-1000*(DEPTH+1))); (maxDepth(DEPTHmax), SCORE is (DEPTHmax+1-DEPTH))), !.

negamax(DEPTH, Pmax, P, SCORE, _) :- changePlayer(Pmax,P1), checkWinning(P1), ((Pmax==P, maxDepth(DEPTHmax), SCORE is (DEPTH-DEPTHmax-1)); SCORE is (1000*(DEPTH+1))), !.

negamax(_, _, _, SCORE, _) :- isGameFull, SCORE=0, !.

negamax(DEPTH, Pmax, P, SCORE, _) :- DEPTH==0, SCOREtmp is random(800)+100, ((Pmax==P, SCORE = SCOREtmp); SCORE is (-SCOREtmp)), !.

negamax(DEPTH, Pmax, P, SCORE, BestCol) :-
                              SCOREinit is (-inf),
                              getGameBoard(GB),
                              forEachChild(0, GB, DEPTH, Pmax, P, SCOREinit, SCOREfinal, 1, BestColFinal),
                              SCORE = SCOREfinal,
                              BestCol = BestColFinal.


forEachChild(_, [], _, _, _, SCORE, SCOREfinal, BestCol, BestColFinal) :- SCOREfinal = SCORE, BestColFinal = BestCol.
forEachChild(COL, [_|GB], DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, not(isColFull(COL1)),
                    playInCol(COL1, P),
                    changePlayer(P,P1),
                    DEPTHnext is DEPTH-1, negamax(DEPTHnext, Pmax, P1, SCOREtmp, _),
                    cancelPlayInCol(COL1),
                    SCOREnega is (-SCOREtmp),
                    (((SCOREnega < SCORE; SCOREnega==SCORE),SCOREnext = SCORE, BestColNext = BestCol); SCOREnext = SCOREnega, BestColNext = COL1),
                    write('Pronfondeur : '), write(DEPTH), write(', Colonne : '), write(COL1), write(', Score : '), writeln(SCOREnega),
                    forEachChild(COL1, GB, DEPTH, Pmax, P, SCOREnext, SCOREfinal, BestColNext, BestColFinal).

forEachChild(COL, [_|GB], DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, isColFull(COL1),
                    forEachChild(COL1, GB, DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal).
