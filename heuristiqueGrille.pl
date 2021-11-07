:- module(heuristiqueGrille, [
  heuristiqueGrille/3
]).

% on renvoie ici le score lié à l heuristique appelée "Grille"
% on dispose d une grille (écrite en dur dans le code ci-dessous) qui correspond pour chaque case au nombre de possibilités de gagner par défaut
% on passe en paramètre l état actuel du jeu et un joueur
% ensuite pour chaque jeton présent dans le jeu, on récupère le nombre de la grille présent dans la case du jeton, et si ce jeton appartient au joueur passé en paramètre, on ajoute ce nombre au score final, sinon on retire ce nombre au score final
% ces additions et soustractions résultent alors en un score renvoyé à negamax ou alphabeta

heuristiqueGrille(SCOREFinal,GB,P) :-
  A = [3,4,5,5,4,3],
  B = [4,6,8,8,6,4],
  C = [5,8,11,11,8,5],
  D = [7,10,13,13,10,7],
  E = [5,8,11,11,8,5],
  F = [4,6,8,8,6,4],
  G = [3,4,5,5,4,3],
  Table = [A,B,C,D,E,F,G],
  calculColonne(GB,Table,P,0,SOMMEScore),
  SCOREFinal is SOMMEScore + 500, !.



% boucle sur chaque colonne du jeu

calculColonne([],_,_,SommeColonnes,SommeScore) :-
  SommeScore = SommeColonnes, !.

calculColonne([X|GB],[Y|Table],P,ScoreInit,SommeScore) :-
  calculCase(X,Y,P,ScoreInit,ScoreCol),
  calculColonne(GB,Table,P,ScoreCol,SommeScore), !.


% boucle sur chaque case de la colonne passée en paramètre

calculCase([],_,_,ScoreInit,SommeScore) :-
  SommeScore = ScoreInit, !.

calculCase([Case|X],[CaseTab|Y],P,ScoreInit,SommeScore) :-
  ((Case == P, SommeCaseTemp is ScoreInit + CaseTab);
  (Case \== 0, SommeCaseTemp is ScoreInit - CaseTab);
  (Case == 0), SommeCaseTemp is ScoreInit),
  calculCase(X,Y,P,SommeCaseTemp,SommeScore), !.
