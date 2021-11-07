:- module(negaMax, [
  negamax/6
]).

:- use_module(puissance4).
:- use_module(heuristiqueGrille).
:- use_module(heuristiqueGrilleDynamique).

% -----------------------------------------------------------------------
% algorithme minimax de profondeur limitée avec simplification negamax
% -----------------------------------------------------------------------


% le joueur qu on veut maximiser peut gagner et on renvoie un gros score si c est à lui de jouer et un petit score si c est au tour du joueur qu on veut minimiser

negamax(_, DEPTH, Pmax, P, SCORE, _) :- checkWinning(Pmax), ((Pmax\==P,SCORE is (-1000*(DEPTH+1))); (maxDepth(DEPTHmax), SCORE is (DEPTHmax+1-DEPTH))), !.


% le joueur qu on veut minimiser peut gagner et on renvoie un gros score si c est à lui de jouer et un petit score si c est au tour du joueur qu on veut maximiser

negamax(_, DEPTH, Pmax, P, SCORE, _) :- changePlayer(Pmax,P1), checkWinning(P1), ((Pmax==P, maxDepth(DEPTHmax), SCORE is (DEPTH-DEPTHmax-1)); SCORE is (1000*(DEPTH+1))), !.


% il peut y avoir une égalité donc on renvoie un score nul

negamax(_, _, _, _, SCORE, _) :- isGameFull, SCORE=0, !.


% aucun des cas précédents n a été détécté et on atteint la prondeur de recherche maximum alors on renvoie comme score l heuristique associée à l état actuel du jeu : on appelle ici la bonne heuristique passée en paramètre

negamax(H, DEPTH, Pmax, P, SCORE, _) :- DEPTH==0,
                                        (
                                        (H==0, SCOREtmp is random(800)+100);
                                        (H==1, getGameBoard(GB), heuristiqueGrille(SCOREtmp,GB,Pmax));
                                        (H==2, getGameBoard(GB), heuristiqueGrilleDynamique(SCOREtmp,GB,Pmax))
                                        ),
                                        ((Pmax==P, SCORE = SCOREtmp); SCORE is (-SCOREtmp)), !.


% appel par défaut de negamax qui va nous permettre de parcourir les enfants de chaque noeud

negamax(H, DEPTH, Pmax, P, SCORE, BestCol) :-
                              SCOREinit is (-inf),
                              getGameBoard(GB),
                              forEachChild(H, 0, GB, DEPTH, Pmax, P, SCOREinit, SCOREfinal, 1, BestColFinal),
                              SCORE = SCOREfinal,
                              BestCol = BestColFinal, !.


% boucle sur tous les enfants de chaque noeud pour identifier les différents scores à la plus basse prondeur et faire remonter le meilleure colonne à jouer en choisissant le minimum ou le maximum des score selon la profondeur et le joueur qui doit jouer
% on appelle pour cela récursivement negamax en jouant avant un coup puis en annulant ce coup après : une fois la recherche terminée, le jeu n est donc pas abîmé
% on vérifie également que la colonne sur laquelle on essaie de jouer n est pas déjà pleine

forEachChild(_, _, [], _, _, _, SCORE, SCOREfinal, BestCol, BestColFinal) :- SCOREfinal = SCORE, BestColFinal = BestCol, !.
forEachChild(H, COL, [_|GB], DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, not(isColFull(COL1)),
                    playInCol(COL1, P),
                    changePlayer(P,P1),
                    DEPTHnext is DEPTH-1, negamax(H, DEPTHnext, Pmax, P1, SCOREtmp, _),
                    cancelPlayInCol(COL1),
                    SCOREnega is (-SCOREtmp),
                    (((SCOREnega < SCORE; SCOREnega==SCORE),SCOREnext = SCORE, BestColNext = BestCol); SCOREnext = SCOREnega, BestColNext = COL1),
                    forEachChild(H, COL1, GB, DEPTH, Pmax, P, SCOREnext, SCOREfinal, BestColNext, BestColFinal), !.

forEachChild(H, COL, [_|GB], DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal) :-
                    COL1 is COL+1, isColFull(COL1),
                    forEachChild(H, COL1, GB, DEPTH, Pmax, P, SCORE, SCOREfinal, BestCol, BestColFinal), !.
