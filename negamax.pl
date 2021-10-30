:- module(negaMax, [
  negamax/5
]).

:- use_module(puissance4).

% algorithme minimax de profondeur limitée avec simplification negamax

negamax(_, Pmax, _, SCORE, _) :- checkWinning(Pmax), SCORE=inf, !.

negamax(_, Pmax, _, SCORE, _) :- changePlayer(Pmax,P1), checkWinning(P1), SCORE=(-inf), !.

negamax(_, _, _, SCORE, _) :- isGameFull, SCORE=0, !.

negamax(DEPTH, Pmax, P, SCORE, _) :- DEPTH==0, SCOREtmp is random(100)+1, ((Pmax==P, SCORE = SCOREtmp); SCORE = -SCOREtmp), !.

negamax(DEPTH, Pmax, P, SCORE, BestCol) :-
                              SCOREinit = -inf,
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
                    SCOREnega = - SCOREtmp,
                    ((SCOREnega < SCORE, SCOREnext = SCORE, BestColNext = BestCol); SCOREnext = SCOREnega, BestColNext = COL1),
                    forEachChild(COL1, GB, DEPTH, Pmax, P, SCOREnext, SCOREfinal, BestColNext, BestColFinal).

forEachChild(COL, [_|GB], DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, isColFull(COL1),
                    forEachChild(COL1, GB, DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal).
