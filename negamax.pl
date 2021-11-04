:- module(negaMax, [
  negamax/6
]).

:- use_module(puissance4).
:- use_module(heuristiqueGrille).
:- use_module(heuristiqueGrilleDynamique).

% algorithme minimax de profondeur limitée avec simplification negamax

negamax(_, DEPTH, Pmax, P, SCORE, _) :- checkWinning(Pmax), ((Pmax\==P,SCORE is (-1000*(DEPTH+1))); (maxDepth(DEPTHmax), SCORE is (DEPTHmax+1-DEPTH))), !.

negamax(_, DEPTH, Pmax, P, SCORE, _) :- changePlayer(Pmax,P1), checkWinning(P1), ((Pmax==P, maxDepth(DEPTHmax), SCORE is (DEPTH-DEPTHmax-1)); SCORE is (1000*(DEPTH+1))), !.

negamax(_, _, _, _, SCORE, _) :- isGameFull, SCORE=0, !.

negamax(H, DEPTH, Pmax, P, SCORE, _) :- DEPTH==0,
                                        (
                                        (H==0, SCOREtmp is random(800)+100);
                                        (H==1, getGameBoard(GB), heuristiqueGrille(SCOREtmp,GB,Pmax));
                                        (H==2, getGameBoard(GB), heuristiqueGrilleDynamique(SCOREtmp,GB,Pmax))
                                        ),
                                        ((Pmax==P, SCORE = SCOREtmp); SCORE is (-SCOREtmp)), !.

negamax(H, DEPTH, Pmax, P, SCORE, BestCol) :-
                              SCOREinit is (-inf),
                              getGameBoard(GB),
                              forEachChild(H, 0, GB, DEPTH, Pmax, P, SCOREinit, SCOREfinal, 1, BestColFinal),
                              SCORE = SCOREfinal,
                              BestCol = BestColFinal.


forEachChild(_, _, [], _, _, _, SCORE, SCOREfinal, BestCol, BestColFinal) :- SCOREfinal = SCORE, BestColFinal = BestCol.
forEachChild(H, COL, [_|GB], DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, not(isColFull(COL1)),
                    playInCol(COL1, P),
                    changePlayer(P,P1),
                    DEPTHnext is DEPTH-1, negamax(H, DEPTHnext, Pmax, P1, SCOREtmp, _),
                    cancelPlayInCol(COL1),
                    SCOREnega is (-SCOREtmp),
                    (((SCOREnega < SCORE; SCOREnega==SCORE),SCOREnext = SCORE, BestColNext = BestCol); SCOREnext = SCOREnega, BestColNext = COL1),
                    forEachChild(H, COL1, GB, DEPTH, Pmax, P, SCOREnext, SCOREfinal, BestColNext, BestColFinal).

forEachChild(H, COL, [_|GB], DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, isColFull(COL1),
                    forEachChild(H, COL1, GB, DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal).
