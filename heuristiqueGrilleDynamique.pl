:- module(heuristiqueGrilleDynamique, [
  heuristiqueGrilleDynamique/3
]).

:- use_module(puissance4).

heuristiqueGrilleDynamique(SCOREFinal,GB,P) :-
  calculColonne(1,GB,P,0,SOMMEScore),
  SCOREFinal is SOMMEScore + 500.

calculColonne(_,[],_,SommeColonnes,SommeScore) :-
  SommeScore = SommeColonnes.

calculColonne(COL,[X|GB],P,ScoreInit,SommeScore) :-
  calculCase(1,COL,X,P,ScoreInit,ScoreCol),
  COL1 is COL+1,
  calculColonne(COL1,GB,P,ScoreCol,SommeScore).



calculCase(_,_,[],_,ScoreInit,SommeScore) :-
  SommeScore = ScoreInit.


calculCase(LINE,COL,[Case|X],P,ScoreInit,SommeScore) :-
  computeTableValue(COL,LINE,P,CaseTab),
  ((Case == P, SommeCaseTemp is ScoreInit + CaseTab);
  (Case \== 0, SommeCaseTemp is ScoreInit - CaseTab);
  (Case == 0), SommeCaseTemp is ScoreInit),
  LINE1 is LINE+1,
  calculCase(LINE1,COL,X,P,SommeCaseTemp,SommeScore).


computeTableValue(COL,LINE,P,VALUE) :-
        ((P==1, P1 = 2); P1 = 1),

        % colonne dessus
        ((LINE10 is LINE, LINE11 is LINE-1, LINE12 is LINE-2, LINE13 is LINE-3,
        getCase(COL,LINE10,C11), getCase(COL,LINE11,C12), getCase(COL,LINE12,C13), getCase(COL,LINE13,C14),
        Possibility1 = [C11,C12,C13,C14]);
        Possibility1 = [P1]),

        % colonne mi dessus
        ((LINE20 is LINE+1, LINE21 is LINE, LINE22 is LINE-1, LINE23 is LINE-2,
        getCase(COL,LINE20,C21), getCase(COL,LINE21,C22), getCase(COL,LINE22,C23), getCase(COL,LINE23,C24),
        Possibility2 = [C21,C22,C23,C24]);
        Possibility2 = [P1]),

        % colonne mi dessous
        ((LINE30 is LINE+2, LINE31 is LINE+1, LINE32 is LINE, LINE33 is LINE-1,
        getCase(COL,LINE30,C31), getCase(COL,LINE31,C32), getCase(COL,LINE32,C33), getCase(COL,LINE33,C34),
        Possibility3 = [C31,C32,C33,C34]);
        Possibility3 = [P1]),

        % colonne dessous
        ((LINE40 is LINE+3, LINE41 is LINE+2, LINE42 is LINE+1, LINE43 is LINE,
        getCase(COL,LINE40,C41), getCase(COL,LINE41,C42), getCase(COL,LINE42,C43), getCase(COL,LINE43,C44),
        Possibility4 = [C41,C42,C43,C44]);
        Possibility4 = [P1]),

        % ligne droite
        ((COL50 is COL, COL51 is COL+1, COL52 is COL+2, COL53 is COL+3,
        getCase(COL50,LINE,C51), getCase(COL51,LINE,C52), getCase(COL52,LINE,C53), getCase(COL53,LINE,C54),
        Possibility5 = [C51,C52,C53,C54]);
        Possibility5 = [P1]),

        % ligne mi droite
        ((COL60 is COL-1, COL61 is COL, COL62 is COL+1, COL63 is COL+2,
        getCase(COL60,LINE,C61), getCase(COL61,LINE,C62), getCase(COL62,LINE,C63), getCase(COL63,LINE,C64),
        Possibility6 = [C61,C62,C63,C64]);
        Possibility6 = [P1]),

        % ligne mi gauche
        ((COL70 is COL-2, COL71 is COL-1, COL72 is COL, COL73 is COL+1,
        getCase(COL70,LINE,C71), getCase(COL71,LINE,C72), getCase(COL72,LINE,C73), getCase(COL73,LINE,C74),
        Possibility7 = [C71,C72,C73,C74]);
        Possibility7 = [P1]),

        % ligne gauche
        ((COL80 is COL-3, COL81 is COL-2, COL82 is COL-1, COL83 is COL,
        getCase(COL80,LINE,C81), getCase(COL81,LINE,C82), getCase(COL82,LINE,C83), getCase(COL83,LINE,C84),
        Possibility8 = [C81,C82,C83,C84]);
        Possibility8 = [P1]),

        % diagonale dessus droite
        ((COL90 is COL, COL91 is COL+1, COL92 is COL+2, COL93 is COL+3,
        LINE90 is LINE, LINE91 is LINE-1, LINE92 is LINE-2, LINE93 is LINE-3,
        getCase(COL90,LINE90,C91), getCase(COL91,LINE91,C92), getCase(COL92,LINE92,C93), getCase(COL93,LINE93,C94),
        Possibility9 = [C91,C92,C93,C94]);
        Possibility9 = [P1]),

        % diagonale dessus gauche
        ((COL100 is COL, COL101 is COL-1, COL102 is COL-2, COL103 is COL-3,
        LINE100 is LINE, LINE101 is LINE-1, LINE102 is LINE-2, LINE103 is LINE-3,
        getCase(COL100,LINE100,C101), getCase(COL101,LINE101,C102), getCase(COL102,LINE102,C103), getCase(COL103,LINE103,C104),
        Possibility10 = [C101,C102,C103,C104]);
        Possibility10 = [P1]),

        % diagonale dessous droite
        ((COL110 is COL, COL111 is COL+1, COL112 is COL+2, COL113 is COL+3,
        LINE110 is LINE, LINE111 is LINE+1, LINE112 is LINE+2, LINE113 is LINE+3,
        getCase(COL110,LINE110,C111), getCase(COL111,LINE111,C112), getCase(COL112,LINE112,C113), getCase(COL113,LINE113,C114),
        Possibility11 = [C111,C112,C113,C114]);
        Possibility11 = [P1]),

        % diagonale dessous gauche
        ((COL120 is COL, COL121 is COL-1, COL122 is COL-2, COL123 is COL-3,
        LINE120 is LINE, LINE121 is LINE+1, LINE122 is LINE+2, LINE123 is LINE+3,
        getCase(COL120,LINE120,C121), getCase(COL121,LINE121,C122), getCase(COL122,LINE122,C123), getCase(COL123,LINE123,C124),
        Possibility12 = [C121,C122,C123,C124]);
        Possibility12 = [P1]),

        Possibilities = [Possibility1,
                         Possibility2,
                         Possibility3,
                         Possibility4,
                         Possibility5,
                         Possibility6,
                         Possibility7,
                         Possibility8,
                         Possibility9,
                         Possibility10,
                         Possibility11,
                         Possibility12],

        checkPlayerOrFree(P, Possibilities, 0, VALUE), !.


checkPlayerOrFree(_,[],InitValue,VALUE) :- VALUE = InitValue.
checkPlayerOrFree(P, [X|Possibilities], InitValue, VALUE) :- ((checkPossibility(P,X),VALUEtmp is InitValue + 1); VALUEtmp = InitValue), checkPlayerOrFree(P,Possibilities,VALUEtmp,VALUE).

checkPossibility(_,[]).
checkPossibility(P,[X|Possibility]) :- (X==P;X==0), checkPossibility(P,Possibility).


getCase(COL, LINE, CONTENT) :- col(COL,X), getLineInCol(LINE,X,1,CONTENT).

getLineInCol(_,[],_,_) :- false.
getLineInCol(LINE,[A|X],INCR,CONTENT) :- (INCR==LINE, CONTENT=A); INCR1 is INCR+1, getLineInCol(LINE,X,INCR1,CONTENT).
