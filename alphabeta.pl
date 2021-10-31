:- module(alphaBeta, [
  alphabeta/7
]).

:- use_module(puissance4).

% algorithme alphabeta (avec simplification negaMax)

alphabeta(_, _, DEPTH, Pmax, P, SCORE, _) :- checkWinning(Pmax), ((Pmax\==P,SCORE is (-1000*(DEPTH+1))); (maxDepth(DEPTHmax), SCORE is (DEPTHmax+1-DEPTH))), !.

alphabeta(_, _, DEPTH, Pmax, P, SCORE, _) :- changePlayer(Pmax,P1), checkWinning(P1), ((Pmax==P, maxDepth(DEPTHmax), SCORE is (DEPTH-DEPTHmax-1)); SCORE is (1000*(DEPTH+1))), !.

alphabeta(_, _, _, _, _, SCORE, _) :- isGameFull, SCORE=0, !.

alphabeta(_, _, DEPTH, Pmax, P, SCORE, _) :- DEPTH==0, SCOREtmp is random(800)+100, ((Pmax==P, SCORE = SCOREtmp); SCORE is (-SCOREtmp)), !.

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
                    alphabeta(BETAnext, ALPHAnext, DEPTHnext, Pmax, P1, SCOREtmp, _),
                    cancelPlayInCol(COL1),
                    SCOREnega is -SCOREtmp,
                    write('Profondeur : '), write(DEPTH), write(', Colonne : '), write(COL1), write(', Score : '), write(SCOREnega), write(', Alpha : '), write(ALPHA), write(', Beta : '), writeln(BETA),
                    (
                      ((SCOREnega < ALPHA; SCOREnega==ALPHA), forEachChild(ALPHA, BETA, COL1, GB, DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal));
                      (
                        ALPHAnew = SCOREnega, BestColNext = COL1, write('Mise à jour Alpha :'), writeln(ALPHAnew),
                        (
                          (ALPHAnew < BETA, forEachChild(ALPHAnew, BETA, COL1, GB, DEPTH, Pmax, P, SCOREfinal, BestColNext, BestColFinal));
                          (writeln('Coupure'), SCOREfinal = ALPHAnew, BestColFinal = BestColNext)
                        )
                      )
                    ).

forEachChild(ALPHA, BETA, COL, [_|GB], DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, isColFull(COL1),
                    forEachChild(ALPHA, BETA, COL1, GB, DEPTH, Pmax, P, SCOREfinal, BestCol, BestColFinal).
