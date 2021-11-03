:- module(heuristiqueGrille, [
  heuristiqueGrille/3
]).

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
  % writeln(SOMMEScore),
  SCOREFinal is SOMMEScore + 500.

calculColonne([],_,_,SommeColonnes,SommeScore) :-
  SommeScore = SommeColonnes.

calculColonne([X|GB],[Y|Table],P,ScoreInit,SommeScore) :-
  calculCase(X,Y,P,ScoreInit,ScoreCol),
  calculColonne(GB,Table,P,ScoreCol,SommeScore).



calculCase([],_,_,ScoreInit,SommeScore) :-
  SommeScore = ScoreInit.


calculCase([Case|X],[CaseTab|Y],P,ScoreInit,SommeScore) :-
  ((Case == P, SommeCaseTemp is ScoreInit + CaseTab);
  (Case \== 0, SommeCaseTemp is ScoreInit - CaseTab);
  (Case == 0), SommeCaseTemp is ScoreInit),
  calculCase(X,Y,P,SommeCaseTemp,SommeScore).
