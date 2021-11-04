%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ADJACENCE HEURISTIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(adjHeuristic, [
    heuristicAdj/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% MAIN FUNCTION TO CALL %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IN Parameter :
% GB : GAMEBOARD, JUST BEFORE PLAYING THE COIN, <=> the actual configuration
% OUT Parameter :
% NUMCOL : THE NUMBER OF THE COLUMN WHERE THE PLAYER SHOULD PLAY
heuristicAdj(GB, NUMCOL):-heuristicAdjOneColumn(GB,1,SCORE1),
                          heuristicAdjOneColumn(GB,2,SCORE2),
                          ((SCORE1>SCORE2, INTM1 is 1, CURRENT is SCORE1);(SCORE2>=SCORE1, INTM1 is 2, CURRENT is SCORE2)),
                          heuristicAdjOneColumn(GB,3,SCORE3),
                          ((SCORE3>CURRENT, INTM2 is 3, CURRENT2 is SCORE3);(CURRENT>=SCORE3, INTM2 is INTM1, CURRENT2 is CURRENT)),
                          heuristicAdjOneColumn(GB,4,SCORE4),
                          ((SCORE4>CURRENT2, INTM3 is 4, CURRENT3 is SCORE4);(CURRENT2>=SCORE4, INTM3 is INTM2, CURRENT3 is CURRENT2)),
                          heuristicAdjOneColumn(GB,5,SCORE5),
                          ((SCORE5>CURRENT3, INTM4 is 5, CURRENT4 is SCORE5);(CURRENT3>=SCORE5, INTM4 is INTM3, CURRENT4 is CURRENT3)),
                          heuristicAdjOneColumn(GB,6,SCORE6),
                          ((SCORE6>CURRENT4, INTM5 is 6, CURRENT5 is SCORE6);(CURRENT4>=SCORE6, INTM5 is INTM4, CURRENT5 is CURRENT4)),
                          heuristicAdjOneColumn(GB,7,SCORE7),
                          ((SCORE7>CURRENT5, INTM6 is 7);(CURRENT5>=SCORE7, INTM6 is INTM5)),
                          NUMCOL is INTM6,!.


heuristicAdjOneColumn(GB,NUMCOL,SCORE):- biggestScoreofAllLines(GB,NUMCOL,SCORE).

%% get All the scores of each Line( Diag A,Diag D, HORIZ, VERTIC) and keep the max
biggestScoreofAllLines(GB,NUMCOL,SCORE):-vScore(GB,NUMCOL,SCORE1), hScore(GB,NUMCOL,SCORE2), INTM is max(SCORE1,SCORE2), daScore(GB,NUMCOL,SCORE3), INTM2 is max(INTM,SCORE3), ddScore(GB,NUMCOL,SCORE4), SCORE is max(INTM2,SCORE4), !.
vScore(GB,NUMCOL,SCORE):-getVLine(GB,NUMCOL,LINE), scoreLINE(LINE, SCORE).
hScore(GB, NUMCOL,SCORE):-getHoriz(GB,NUMCOL,LINE), scoreLINE(LINE,SCORE).
daScore(GB,NUMCOL,SCORE):- getAscDiag(GB,NUMCOL,LINE), scoreLINE(LINE,SCORE).
ddScore(GB,NUMCOL,SCORE):- getDescDiag(GB,NUMCOL,LINE), scoreLINE(LINE,SCORE).


%get The score of a line (consecutive occurences)
scoreLINE(LINE,SCORE):- countSuccessiveOnes(LINE,COUNT1), countSuccessiveTwos(LINE, COUNT2), SCORE is max(COUNT1,COUNT2),!. % puis reverse, countOnes .. COUNT2, additionner COUNT1 et COUNT2, faire de meme pour les 2 et prendre le max, SCORE=max


countSuccessiveOnes(LINE, SCORE):- afterPlayerCoin(LINE, LINE1), countOnes(LINE1, SCORE1),
                                   reverse(LINE, REVLINE),
                                   afterPlayerCoin(REVLINE, LINE2), countOnes(LINE2, SCORE2),
                                   SCORE is SCORE1+SCORE2+1. % +1 for the playerCoin

countSuccessiveTwos(LINE, SCORE):- afterPlayerCoin(LINE, LINE1), countTwos(LINE1, SCORE1),
                                   reverse(LINE, REVLINE),
                                   afterPlayerCoin(REVLINE, LINE2), countTwos(LINE2, SCORE2),
                                   SCORE is SCORE1+SCORE2+1,!. % +1 for the playerCoin


afterPlayerCoin([3|T], LINE1):- LINE1=T.
afterPlayerCoin([X|T],LINE1) :- X\==3, afterPlayerCoin(T,TEMP), LINE1 = TEMP,!.

countTwos([],N):- N=0.
countTwos([2|T],N) :- countTwos(T,N1), N is N1 + 1, !.
countTwos([_|_],N) :- N=0.

% from : https://stackoverflow.com/questions/46902653/prolog-how-to-count-the-number-of-elements-in-a-list-that-satisfy-a-specific-c?rq=1
countOnes([],N):- N=0.
countOnes([1|T],N) :- countOnes(T,N1), N is N1 + 1, !.
countOnes([_|_],N) :- N=0.


%heuristique d'adjacence
getElem2D(GB,INDEXCOL,INDEXLINE, ELEM):-nth1(INDEXCOL,GB,X), nth1(INDEXLINE,X,ELEM).


firstElem([Element|_], Element, 0):- !.
firstElem([_|Tail], Element, Index):-  firstElem(Tail, Element, Index1),  !,  Index is Index1 +1 .


%COL est la colonne et non le n° de colonne
getIndexFirstElem(GB, NUMCOL, INDEX):-    nth1(NUMCOL,GB,COL), ((firstElem(COL, 2, INDEX2),firstElem(COL, 1, INDEX1), I is min(INDEX1,INDEX2), INDEX is I);
                                    ((firstElem(COL, 1, INDEX1), I is INDEX1, INDEX is I);(firstElem(COL, 2, INDEX2),  I is INDEX2, INDEX is I))).



%%%%%%%%%%%%%%%%%%%%%
%%  VERTICAL LINE  %%
%%%%%%%%%%%%%%%%%%%%%
getVLine(GB,NUMCOL,LINE) :- (getIndexFirstElem(GB, NUMCOL,INDEX); INDEX is 6), INDEX1 is (INDEX-1), (NUMCOL1 is NUMCOL-1), replace(GB, NUMCOL1, INDEX1, 3, GB1), nth1(NUMCOL,GB1,LINE),!.


%%%%%%%%%%%%%%%%%%%%%%
%%  HORIZONTAL LINE %%
%%%%%%%%%%%%%%%%%%%%%%
getHoriz(GB, NUMCOL, LINE):- (getIndexFirstElem(GB, NUMCOL,INDEX); INDEX is 6),
                             INDEX1 is (INDEX-1), (NUMCOL1 is NUMCOL-1), replace(GB, NUMCOL1, INDEX1, 3, GB1),
                             getElem2D(GB1,1,INDEX,A),
                             getElem2D(GB1,2,INDEX,B),
                             getElem2D(GB1,3,INDEX,C),
                             getElem2D(GB1,4,INDEX,D),
                             getElem2D(GB1,5,INDEX,E),
                             getElem2D(GB1,6,INDEX,F),
                             getElem2D(GB1,7,INDEX,G),
                             LINE = [A,B,C,D,E,F,G], !.


%%%%%%%%%%%%%%%%%%%%
%%  DIAGONAL ASC  %%
%%%%%%%%%%%%%%%%%%%%
getAscDiag(GB, NUMCOL, LINE):- (getIndexFirstElem(GB, NUMCOL,INDEX); INDEX is 6), DIAGLENGTH is INDEX+NUMCOL,
                                  INDEX1 is (INDEX-1), (NUMCOL1 is NUMCOL-1), replace(GB, NUMCOL1, INDEX1, 3, GB1),
                                  getDiag1(GB1, DIAGLENGTH, NUMCOL, INDEX, LINE), !.

% 1 seul élément sur diagonale
getDiag1(GB,LENGTH, INDEXCOL, INDEXLINE, DIAG):- (LENGTH==2;LENGTH==13),getElem2D(GB,INDEXCOL,INDEXLINE, ELEM), DIAG=[ELEM].

% 2 éléments sur diagonale
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==3,  getElem2D(GB,1,2, ELEM1), getElem2D(GB,2,1, ELEM2), DIAG=[ELEM1,ELEM2].
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==12, getElem2D(GB,6,6, ELEM1), getElem2D(GB,7,5, ELEM2), DIAG=[ELEM1,ELEM2].

% 3 éléments sur diagonale
getDiag1(GB,LENGTH, _, _,DIAG):- LENGTH==4,  getElem2D(GB,1,3, ELEM1), getElem2D(GB,2,2, ELEM2), getElem2D(GB,3,1, ELEM3), DIAG=[ELEM1,ELEM2,ELEM3].
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==11, getElem2D(GB,5,6, ELEM1), getElem2D(GB,6,5, ELEM2), getElem2D(GB,7,4, ELEM3), DIAG=[ELEM1,ELEM2,ELEM3].

% 4 éléments sur diagonale
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==5,  getElem2D(GB,1,4, ELEM1), getElem2D(GB,2,3, ELEM2), getElem2D(GB,3,2, ELEM3), getElem2D(GB,4,1, ELEM4),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4].
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==10, getElem2D(GB,4,6, ELEM1), getElem2D(GB,5,5, ELEM2), getElem2D(GB,6,4, ELEM3), getElem2D(GB,7,3, ELEM4),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4].

% 5 éléments sur diagonale
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==6,  getElem2D(GB,1,5, ELEM1), getElem2D(GB,2,4, ELEM2), getElem2D(GB,3,3, ELEM3), getElem2D(GB,4,2, ELEM4), getElem2D(GB,5,1, ELEM5),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5].
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==9,  getElem2D(GB,3,6, ELEM1), getElem2D(GB,4,5, ELEM2), getElem2D(GB,5,4, ELEM3), getElem2D(GB,6,3, ELEM4), getElem2D(GB,7,2, ELEM5),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5].

% 6 éléments sur diagonale
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==7,  getElem2D(GB,1,6, ELEM1), getElem2D(GB,2,5, ELEM2), getElem2D(GB,3,4, ELEM3), getElem2D(GB,4,3, ELEM4), getElem2D(GB,5,2, ELEM5), getElem2D(GB,6,1, ELEM6),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5, ELEM6].
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==8,  getElem2D(GB,2,6, ELEM1), getElem2D(GB,3,5, ELEM2), getElem2D(GB,4,4, ELEM3), getElem2D(GB,5,3, ELEM4), getElem2D(GB,6,2, ELEM5), getElem2D(GB,7,1, ELEM6),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5, ELEM6].




%%%%%%%%%%%%%%%%%%%%%
%%  DIAGONAL DESC  %%
%%%%%%%%%%%%%%%%%%%%%
getDescDiag(GB, NUMCOL, LINE):- (getIndexFirstElem(GB, NUMCOL,INDEX); INDEX is 6), DIAGLENGTH is NUMCOL-INDEX,
                                   INDEX1 is (INDEX-1), (NUMCOL1 is NUMCOL-1), replace(GB, NUMCOL1, INDEX1, 3, GB1),
                                   getDiag2(GB1, DIAGLENGTH, NUMCOL, INDEX, LINE),!.

% 1 seul élément sur diagonale
getDiag2(GB,LENGTH, INDEXCOL, INDEXLINE, DIAG):- (LENGTH==6;LENGTH==(-5)),getElem2D(GB,INDEXCOL,INDEXLINE, ELEM), DIAG=[ELEM].

% 2 éléments sur diagonale
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==5,  getElem2D(GB,6,1, ELEM1), getElem2D(GB,7,2, ELEM2), DIAG=[ELEM1,ELEM2].
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==(-4), getElem2D(GB,1,5, ELEM1), getElem2D(GB,2,6, ELEM2), DIAG=[ELEM1,ELEM2].

% 3 éléments sur diagonale
getDiag2(GB,LENGTH, _, _,DIAG):- LENGTH==4,  getElem2D(GB,5,1, ELEM1), getElem2D(GB,6,2, ELEM2), getElem2D(GB,7,3, ELEM3), DIAG=[ELEM1,ELEM2,ELEM3].
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==(-3), getElem2D(GB,1,4, ELEM1), getElem2D(GB,2,5, ELEM2), getElem2D(GB,3,6, ELEM3), DIAG=[ELEM1,ELEM2,ELEM3].

% 4 éléments sur diagonale
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==3,  getElem2D(GB,4,1, ELEM1), getElem2D(GB,5,2, ELEM2), getElem2D(GB,6,3, ELEM3), getElem2D(GB,7,4, ELEM4),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4].
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==(-2), getElem2D(GB,1,3, ELEM1), getElem2D(GB,2,4, ELEM2), getElem2D(GB,3,5, ELEM3), getElem2D(GB,4,6, ELEM4),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4].

% 5 éléments sur diagonale
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==2,  getElem2D(GB,3,1, ELEM1), getElem2D(GB,4,2, ELEM2), getElem2D(GB,5,3, ELEM3), getElem2D(GB,6,4, ELEM4), getElem2D(GB,7,5, ELEM5),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5].
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==(-1), getElem2D(GB,1,2, ELEM1), getElem2D(GB,2,3, ELEM2), getElem2D(GB,3,4, ELEM3), getElem2D(GB,4,5, ELEM4), getElem2D(GB,5,6, ELEM5),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5].

% 6 éléments sur diagonale
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==1,  getElem2D(GB,2,1, ELEM1), getElem2D(GB,3,2, ELEM2), getElem2D(GB,4,3, ELEM3), getElem2D(GB,5,4, ELEM4), getElem2D(GB,6,5, ELEM5), getElem2D(GB,7,6, ELEM6),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5, ELEM6].
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==0,  getElem2D(GB,1,1, ELEM1), getElem2D(GB,2,2, ELEM2), getElem2D(GB,3,3, ELEM3), getElem2D(GB,4,4, ELEM4), getElem2D(GB,5,5, ELEM5), getElem2D(GB,6,6, ELEM6),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5, ELEM6].



%from : https://stackoverflow.com/questions/26719774/replacing-elements-in-list-of-lists-prolog
replace( L , X , Y , Z , R ) :-
  append(RowPfx,[Row|RowSfx],L),     % decompose the list-of-lists into a prefix, a list and a suffix
  length(RowPfx,X) ,                 % check the prefix length: do we have the desired list?
  append(ColPfx,[_|ColSfx],Row) ,    % decompose that row into a prefix, a column and a suffix
  length(ColPfx,Y) ,                 % check the prefix length: do we have the desired column?
  append(ColPfx,[Z|ColSfx],RowNew) , % if so, replace the column with its new value
  append(RowPfx,[RowNew|RowSfx],R).  % and assemble the transformed list-of-lists