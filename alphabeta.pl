:- module(alphaBeta, [
  alphabeta/8
]).

:- use_module(puissance4).
:- use_module(heuristiqueGrille).
:- use_module(heuristiqueGrilleDynamique).

% algorithme alphabeta (avec simplification negaMax)

alphabeta(_, _, _, DEPTH, Pmax, P, SCORE, _) :- checkWinning(Pmax), ((Pmax\==P,SCORE is (-1000*(DEPTH+1))); (maxDepth(DEPTHmax), SCORE is (DEPTHmax+1-DEPTH))), !.

alphabeta(_ ,_, _, DEPTH, Pmax, P, SCORE, _) :- changePlayer(Pmax,P1), checkWinning(P1), ((Pmax==P, maxDepth(DEPTHmax), SCORE is (DEPTH-DEPTHmax-1)); SCORE is (1000*(DEPTH+1))), !.

alphabeta(_, _, _, _, _, _, SCORE, _) :- isGameFull, SCORE=0, !.

alphabeta(H, _, _, DEPTH, Pmax, P, SCORE, _) :- DEPTH==0,
                                            (
                                            (H==0, SCOREtmp is random(800)+100);
                                            (H==1, getGameBoard(GB), heuristiqueGrille(SCOREtmp,GB,Pmax));
                                            (H==2, getGameBoard(GB), heuristiqueGrilleDynamique(SCOREtmp,GB,Pmax))
                                            ),
                                            ((Pmax==P, SCORE = SCOREtmp); SCORE is (-SCOREtmp)), !.

alphabeta(H, ALPHA, BETA, DEPTH, Pmax, P, SCORE, BestCol) :-
                              getGameBoard(GB),
                              forEachChild(H, ALPHA, BETA, 0, GB, DEPTH, Pmax, P, SCOREfinal, 1, BestColFinal),
                              SCORE is SCOREfinal,
                              BestCol = BestColFinal.


forEachChild(_, ALPHA, _, _, [], _, _, _, SCOREfinal, BestCol, BestColFinal) :- SCOREfinal = ALPHA, BestColFinal = BestCol.
forEachChild(H, ALPHA, BETA, COL, [_|GB], DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, not(isColFull(COL1)),
                    playInCol(COL1, P),
                    changePlayer(P,P1),
                    DEPTHnext is DEPTH-1,
                    ALPHAnext is -ALPHA,
                    BETAnext is -BETA,
                    alphabeta(H, BETAnext, ALPHAnext, DEPTHnext, Pmax, P1, SCOREtmp, _),
                    cancelPlayInCol(COL1),
                    SCOREnega is -SCOREtmp,
                    (
                      ((SCOREnega < ALPHA; SCOREnega==ALPHA), forEachChild(H, ALPHA, BETA, COL1, GB, DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal));
                      (
                        ALPHAnew = SCOREnega, BestColNext = COL1,
                        (
                          (ALPHAnew < BETA, forEachChild(H, ALPHAnew, BETA, COL1, GB, DEPTH, Pmax, P, SCOREfinal, BestColNext, BestColFinal));
                          (SCOREfinal = ALPHAnew, BestColFinal = BestColNext)
                        )
                      )
                    ).

forEachChild(H, ALPHA, BETA, COL, [_|GB], DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, isColFull(COL1),
                    forEachChild(H, ALPHA, BETA, COL1, GB, DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal).
