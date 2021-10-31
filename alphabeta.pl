:- module(alphaBeta, [
  alphabeta/7
]).

:- use_module(puissance4).

% algorithme alphabeta (avec simplification negaMax)

alphabeta(_, _, _, Pmax, _, SCORE, _) :- checkWinning(Pmax), SCORE=inf, !.

alphabeta(_, _, _, Pmax, _, SCORE, _) :- changePlayer(Pmax,P1), checkWinning(P1), SCORE=(-inf), !.

alphabeta(_, _, _, _, _, SCORE, _) :- isGameFull, SCORE=0, !.

alphabeta(_, _, DEPTH, Pmax, P, SCORE, _) :- DEPTH==0, SCOREtmp is random(100)+1, ((Pmax==P, SCORE = SCOREtmp); SCORE is -SCOREtmp), !.

alphabeta(ALPHA, BETA, DEPTH, Pmax, P, SCORE, BestCol) :-
                              getGameBoard(GB),
                              forEachChild(ALPHA, BETA, 0, GB, DEPTH, Pmax, P, SCOREfinal, 1, BestColFinal),
                              SCORE is SCOREfinal,
                              BestCol = BestColFinal.


forEachChild(ALPHA, _, _, [], _, _, _, SCOREfinal, BestCol, BestColFinal) :- SCOREfinal = ALPHA, BestColFinal = BestCol.
forEachChild(ALPHA, BETA, COL, [_|GB], DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, not(isColFull(COL1)),
                    playInCol(COL1, P),
                    changePlayer(P,P1),
                    DEPTHnext is DEPTH-1,
                    ALPHAnext is -ALPHA,
                    BETAnext is -BETA,
                    alphabeta(ALPHAnext, BETAnext, DEPTHnext, Pmax, P1, SCOREtmp, _),
                    cancelPlayInCol(COL1),
                    SCOREnega is -SCOREtmp,
                    (
                      (SCOREnega < ALPHA, forEachChild(ALPHA, BETA, COL1, GB, DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal));
                      (
                        ALPHAnew = SCOREnega, BestColNext = COL1,
                        (
                          (ALPHAnew < BETA, forEachChild(ALPHAnew, BETA, COL1, GB, DEPTH, Pmax, P, SCOREfinal, BestColNext, BestColFinal));
                          (SCOREfinal = ALPHAnew, BestColFinal = BestColNext)
                        )
                      )
                    ).

forEachChild(ALPHA, BETA, COL, [_|GB], DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, isColFull(COL1),
                    forEachChild(ALPHA, BETA, COL1, GB, DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal).
