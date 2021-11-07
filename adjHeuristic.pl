%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ADJACENCE HEURISTIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(adjHeuristic, [
    heuristicAdj/3
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% MAIN FUNCTION TO CALL %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IN Parameter :
% GB : GAMEBOARD, JUST BEFORE PLAYING THE COIN, <=> the current configuration
% OUT Parameter :
% NUMCOL : THE NUMBER OF THE COLUMN WHERE THE PLAYER SHOULD PLAY
heuristicAdj(GB, NUMCOL,P):-((not(isColFull(1,GB)), heuristicAdjOneColumn(GB,1,SCORE1,P)); SCORE1 is 0),
                          ((not(isColFull(2,GB)), heuristicAdjOneColumn(GB,2,SCORE2,P)); SCORE2 is 0),
                          ((SCORE1>SCORE2, INTM1 is 1, CURRENT is SCORE1);(SCORE2>=SCORE1, INTM1 is 2, CURRENT is SCORE2)),
                          ((not(isColFull(3,GB)), heuristicAdjOneColumn(GB,3,SCORE3,P)); SCORE3 is 0),
                          ((SCORE3>CURRENT, INTM2 is 3, CURRENT2 is SCORE3);(CURRENT>=SCORE3, INTM2 is INTM1, CURRENT2 is CURRENT)),
                          ((not(isColFull(4,GB)), heuristicAdjOneColumn(GB,4,SCORE4,P)); SCORE4 is 0),
                          ((SCORE4>CURRENT2, INTM3 is 4, CURRENT3 is SCORE4);(CURRENT2>=SCORE4, INTM3 is INTM2, CURRENT3 is CURRENT2)),
                          ((not(isColFull(5,GB)), heuristicAdjOneColumn(GB,5,SCORE5,P)); SCORE5 is 0),
                          ((SCORE5>CURRENT3, INTM4 is 5, CURRENT4 is SCORE5);(CURRENT3>=SCORE5, INTM4 is INTM3, CURRENT4 is CURRENT3)),
                          ((not(isColFull(6,GB)), heuristicAdjOneColumn(GB,6,SCORE6,P)); SCORE6 is 0),
                          ((SCORE6>CURRENT4, INTM5 is 6, CURRENT5 is SCORE6);(CURRENT4>=SCORE6, INTM5 is INTM4, CURRENT5 is CURRENT4)),
                          ((not(isColFull(7,GB)), heuristicAdjOneColumn(GB,7,SCORE7,P)); SCORE7 is 0),
                          ((SCORE7>CURRENT5, INTM6 is 7);(CURRENT5>=SCORE7, INTM6 is INTM5)),
                          NUMCOL is INTM6,!.


% to verify if the column is full <=> we can't play in it anymore
noZeroFound([]).
noZeroFound([A|X]) :- A\==0, noZeroFound(X), !.
isColFull(NUMCOL, GB):- nth1(NUMCOL,GB,LINE), noZeroFound(LINE), !.
heuristicAdjOneColumn(GB,NUMCOL,SCORE,P):- biggestScoreofAllLines(GB,NUMCOL,SCORE,P), !.

%% get All the scores of each Line( Diag A,Diag D, HORIZ, VERTIC) and add them
biggestScoreofAllLines(GB,NUMCOL,SCORE,P):- vScore(GB,NUMCOL,SCORE1, SCORE11,P),
                                          hScore(GB,NUMCOL, SCORE2, SCORE22,P),
                                          daScore(GB,NUMCOL,SCORE3, SCORE33,P),
                                          ddScore(GB,NUMCOL,SCORE4, SCORE44,P),
                                          (INTM1 is SCORE1+SCORE2+SCORE3+SCORE4),
                                          (INTM2 is SCORE11+SCORE22+SCORE33+SCORE44),
                                          SCORE is INTM1+INTM2,!.


vScore(GB,NUMCOL,SCORE1,SCORE2,P):-getVLine(GB,NUMCOL,LINE), scoreLINE(LINE, SCORE1, SCORE2,P), !.
hScore(GB, NUMCOL,SCORE1, SCORE2,P):-getHoriz(GB,NUMCOL,LINE), scoreLINE(LINE,SCORE1, SCORE2,P), !.
daScore(GB,NUMCOL,SCORE1, SCORE2,P):- getAscDiag(GB,NUMCOL,LINE), scoreLINE(LINE,SCORE1, SCORE2,P), !.
ddScore(GB,NUMCOL,SCORE1, SCORE2,P):- getDescDiag(GB,NUMCOL,LINE), scoreLINE(LINE,SCORE1, SCORE2,P), !.


%get The score of a line (consecutive occurences)
%first we check if there is enough space to be able to align 4 tokens
% if there is, we get the adjacent tokens, and we weight them : score of 1 for 1 token, score of 5 for 2 tokens, score of 15 for 3 tokens, score of 1000 for 4 tokens
% if not, the score is ZERO
% SCOREONE and SCORETWO are the respectively scores for the player 1 and the player 2

scoreLINE(LINE,SCOREONE, SCORETWO,P):- (
                          (isEnoughFor4Coins(LINE, S1,1), S1>=4,(
                            countSuccessiveOnes(LINE,COUNT1),
                            (((COUNT1>=4, ((P==1,INTM1 is 100000);(P==2, INTM1 is 10000))));
                            (COUNT1==3, INTM1 is 15);
                            (COUNT1==2, INTM1 is 5);
                            (COUNT1==1, INTM1 is COUNT1)
                            )))
                        ;
                        (INTM1 is 0)),
                        (
                          (isEnoughFor4Coins(LINE, S2,2), S2>=4,(
                            countSuccessiveTwos(LINE, COUNT2),
                            (((COUNT2>=4, ((P==1,INTM2 is 10000);(P==2, INTM2 is 100000))));
                            (COUNT2==3, INTM2 is 15);
                            (COUNT2==2, INTM2 is 5);
                            (COUNT2==1, INTM2 is COUNT2)
                            )))
                        ;
                        (INTM2 is 0)),
                        SCOREONE is INTM1,
                        SCORETWO is INTM2, !.

% to check if there is enough space to align 4 tokens (or more)
isEnoughFor4Coins(LINE, SCORE, P) :-  afterPlayerCoin(LINE, LINE1), ((P==1,countFreeForOne(LINE1, FREE1)); (P==2, countFreeForTwo(LINE1, FREE1))) ,
                                      reverse(LINE, REVLINE),
                                      afterPlayerCoin(REVLINE, LINE2), ((P==1,countFreeForOne(LINE2, FREE2)); (P==2, countFreeForTwo(LINE2, FREE2))),
                                      FREE is FREE1+FREE2,
                                      afterFreePlace(LINE1,AFTERFREE1), % prévoir valeur []
                                      afterFreePlace(LINE2,AFTERFREE2),
                                      ((P==1, countOnes2(AFTERFREE1, COUNT1AFTERFREE1), countOnes2(AFTERFREE2, COUNT1AFTERFREE2),countSuccessiveOnes(LINE,GLOBAL), NUMBERSAFTERFREE is COUNT1AFTERFREE1 + COUNT1AFTERFREE2);
                                      (P==2, countTwos2(AFTERFREE1, COUNT2AFTERFREE1),countTwos2(AFTERFREE2, COUNT2AFTERFREE2), countSuccessiveTwos(LINE,GLOBAL), NUMBERSAFTERFREE is COUNT2AFTERFREE1 + COUNT2AFTERFREE2)),


                                      SCORE is NUMBERSAFTERFREE + GLOBAL + FREE,!.



countSuccessiveOnes(LINE, SCORE):- afterPlayerCoin(LINE, LINE1), countOnes(LINE1, SCORE1),
                                   reverse(LINE, REVLINE),
                                   afterPlayerCoin(REVLINE, LINE2), countOnes(LINE2, SCORE2),
                                   SCORE is SCORE1+SCORE2+1, !. % +1 for the playerCoin

countSuccessiveTwos(LINE, SCORE):- afterPlayerCoin(LINE, LINE1), countTwos(LINE1, SCORE1),
                                   reverse(LINE, REVLINE),
                                   afterPlayerCoin(REVLINE, LINE2), countTwos(LINE2, SCORE2),
                                   SCORE is SCORE1+SCORE2+1,!. % +1 for the playerCoin

% get the line after the player token (the player token is written as a '3' in the gameboard)
afterPlayerCoin([3|T], LINE1):- LINE1=T, !.
afterPlayerCoin([X|T],LINE1) :- X\==3, afterPlayerCoin(T,TEMP), LINE1 = TEMP,!.


%get the line after an empty box (written as zero in the gameboard)
afterFreePlace([], LINE1):- LINE1=[], !.
afterFreePlace([0|T], LINE1):- LINE1=T, !.
afterFreePlace([_|T],LINE1) :- afterFreePlace(T,TEMP), LINE1 = TEMP,!.
afterFreePlace([_|T],LINE1) :- T==[], LINE1 = [],!.

% from : https://stackoverflow.com/questions/46902653/prolog-how-to-count-the-number-of-elements-in-a-list-that-satisfy-a-specific-c?rq=1
countTwos([],N):- N=0, !.
countTwos([2|T],N) :- countTwos(T,N1), N is N1 + 1, !.
countTwos([_|_],N) :- N=0, !.


countOnes([],N):- N=0, !.
countOnes([1|T],N) :- countOnes(T,N1), N is N1 + 1, !.
countOnes([_|_],N) :- N=0, !.

%count empty boxes after and before a box '1'
countFreeForOne([],N):- N=0, !.
countFreeForOne([2|_],N) :- N=0, !.
countFreeForOne([1|T],N) :- countFreeForOne(T,N1), N is N1, !.
countFreeForOne([_|T],N) :- countFreeForOne(T,N1), N is N1 +1, !.

%count empty boxes after and before a box '2'
countFreeForTwo([],N):- N=0, !.
countFreeForTwo([1|_],N) :- N=0, !.
countFreeForTwo([2|T],N) :- countFreeForTwo(T,N1), N is N1, !.
countFreeForTwo([_|T],N) :- countFreeForTwo(T,N1), N is N1 +1, !.

%count successive ones including the player token
countOnes2([_|_],N) :- N=0, !.
countOnes2([],N):- N=0, !.
countOnes2([2|_],N) :- N=0, !.
countOnes2([3|T],N) :-  countOnes2(T,N1), N is N1 +1, !.
countOnes2([1|T],N) :- countOnes2(T,N1), N is N1 +1, !.

%count successive twos including the player token
countTwos2([_|_],N) :- N=0, !.
countTwos2([],N):- N=0, !.
countTwos2([1|_],N) :- N=0, !.
countTwos2([3|T],N) :-  countTwos2(T,N1), N is N1 +1, !.
countTwos2([2|T],N) :- countTwos2(T,N1), N is N1 +1, !.

%get an element in a list of lists with a given column and a given row
getElem2D(GB,INDEXCOL,INDEXLINE, ELEM):-nth1(INDEXCOL,GB,X), nth1(INDEXLINE,X,ELEM), !.

%get the index of the first element in a column
firstElem([Element|_], Element, 0):- !.
firstElem([_|Tail], Element, Index):-  firstElem(Tail, Element, Index1),  !,  Index is Index1 +1 , !.
getIndexFirstElem(GB, NUMCOL, INDEX):-    nth1(NUMCOL,GB,COL), ((firstElem(COL, 2, INDEX2),firstElem(COL, 1, INDEX1), I is min(INDEX1,INDEX2), INDEX is I);
                                    ((firstElem(COL, 1, INDEX1), I is INDEX1, INDEX is I);(firstElem(COL, 2, INDEX2),  I is INDEX2, INDEX is I))), !.


%replace an element by an other in a list of lists, used to put the player token '3' in the current line at the first empty box
%from : https://stackoverflow.com/questions/26719774/replacing-elements-in-list-of-lists-prolog
replace( L , X , Y , Z , R ) :-
  append(RowPfx,[Row|RowSfx],L),     % decompose the list-of-lists into a prefix, a list and a suffix
  length(RowPfx,X) ,                 % check the prefix length: do we have the desired list?
  append(ColPfx,[_|ColSfx],Row) ,    % decompose that row into a prefix, a column and a suffix
  length(ColPfx,Y) ,                 % check the prefix length: do we have the desired column?
  append(ColPfx,[Z|ColSfx],RowNew) , % if so, replace the column with its new value
  append(RowPfx,[RowNew|RowSfx],R),  % and assemble the transformed list-of-lists
  !.

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
getDiag1(GB,LENGTH, INDEXCOL, INDEXLINE, DIAG):- (LENGTH==2;LENGTH==13),getElem2D(GB,INDEXCOL,INDEXLINE, ELEM), DIAG=[ELEM], !.

% 2 éléments sur diagonale
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==3,  getElem2D(GB,1,2, ELEM1), getElem2D(GB,2,1, ELEM2), DIAG=[ELEM1,ELEM2], !.
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==12, getElem2D(GB,6,6, ELEM1), getElem2D(GB,7,5, ELEM2), DIAG=[ELEM1,ELEM2], !.

% 3 éléments sur diagonale
getDiag1(GB,LENGTH, _, _,DIAG):- LENGTH==4,  getElem2D(GB,1,3, ELEM1), getElem2D(GB,2,2, ELEM2), getElem2D(GB,3,1, ELEM3), DIAG=[ELEM1,ELEM2,ELEM3], !.
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==11, getElem2D(GB,5,6, ELEM1), getElem2D(GB,6,5, ELEM2), getElem2D(GB,7,4, ELEM3), DIAG=[ELEM1,ELEM2,ELEM3], !.

% 4 éléments sur diagonale
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==5,  getElem2D(GB,1,4, ELEM1), getElem2D(GB,2,3, ELEM2), getElem2D(GB,3,2, ELEM3), getElem2D(GB,4,1, ELEM4),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4], !.
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==10, getElem2D(GB,4,6, ELEM1), getElem2D(GB,5,5, ELEM2), getElem2D(GB,6,4, ELEM3), getElem2D(GB,7,3, ELEM4),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4], !.

% 5 éléments sur diagonale
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==6,  getElem2D(GB,1,5, ELEM1), getElem2D(GB,2,4, ELEM2), getElem2D(GB,3,3, ELEM3), getElem2D(GB,4,2, ELEM4), getElem2D(GB,5,1, ELEM5),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5], !.
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==9,  getElem2D(GB,3,6, ELEM1), getElem2D(GB,4,5, ELEM2), getElem2D(GB,5,4, ELEM3), getElem2D(GB,6,3, ELEM4), getElem2D(GB,7,2, ELEM5),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5], !.

% 6 éléments sur diagonale
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==7,  getElem2D(GB,1,6, ELEM1), getElem2D(GB,2,5, ELEM2), getElem2D(GB,3,4, ELEM3), getElem2D(GB,4,3, ELEM4), getElem2D(GB,5,2, ELEM5), getElem2D(GB,6,1, ELEM6),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5, ELEM6], !.
getDiag1(GB,LENGTH, _, _, DIAG):- LENGTH==8,  getElem2D(GB,2,6, ELEM1), getElem2D(GB,3,5, ELEM2), getElem2D(GB,4,4, ELEM3), getElem2D(GB,5,3, ELEM4), getElem2D(GB,6,2, ELEM5), getElem2D(GB,7,1, ELEM6),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5, ELEM6], !.




%%%%%%%%%%%%%%%%%%%%%
%%  DIAGONAL DESC  %%
%%%%%%%%%%%%%%%%%%%%%
getDescDiag(GB, NUMCOL, LINE):- (getIndexFirstElem(GB, NUMCOL,INDEX); INDEX is 6), DIAGLENGTH is NUMCOL-INDEX,
                                   INDEX1 is (INDEX-1), (NUMCOL1 is NUMCOL-1), replace(GB, NUMCOL1, INDEX1, 3, GB1),
                                   getDiag2(GB1, DIAGLENGTH, NUMCOL, INDEX, LINE),!.

% 1 seul élément sur diagonale
getDiag2(GB,LENGTH, INDEXCOL, INDEXLINE, DIAG):- (LENGTH==6;LENGTH==(-5)),getElem2D(GB,INDEXCOL,INDEXLINE, ELEM), DIAG=[ELEM], !.

% 2 éléments sur diagonale
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==5,  getElem2D(GB,6,1, ELEM1), getElem2D(GB,7,2, ELEM2), DIAG=[ELEM1,ELEM2], !.
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==(-4), getElem2D(GB,1,5, ELEM1), getElem2D(GB,2,6, ELEM2), DIAG=[ELEM1,ELEM2], !.

% 3 éléments sur diagonale
getDiag2(GB,LENGTH, _, _,DIAG):- LENGTH==4,  getElem2D(GB,5,1, ELEM1), getElem2D(GB,6,2, ELEM2), getElem2D(GB,7,3, ELEM3), DIAG=[ELEM1,ELEM2,ELEM3], !.
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==(-3), getElem2D(GB,1,4, ELEM1), getElem2D(GB,2,5, ELEM2), getElem2D(GB,3,6, ELEM3), DIAG=[ELEM1,ELEM2,ELEM3], !.

% 4 éléments sur diagonale
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==3,  getElem2D(GB,4,1, ELEM1), getElem2D(GB,5,2, ELEM2), getElem2D(GB,6,3, ELEM3), getElem2D(GB,7,4, ELEM4),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4], !.
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==(-2), getElem2D(GB,1,3, ELEM1), getElem2D(GB,2,4, ELEM2), getElem2D(GB,3,5, ELEM3), getElem2D(GB,4,6, ELEM4),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4], !.

% 5 éléments sur diagonale
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==2,  getElem2D(GB,3,1, ELEM1), getElem2D(GB,4,2, ELEM2), getElem2D(GB,5,3, ELEM3), getElem2D(GB,6,4, ELEM4), getElem2D(GB,7,5, ELEM5),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5], !.
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==(-1), getElem2D(GB,1,2, ELEM1), getElem2D(GB,2,3, ELEM2), getElem2D(GB,3,4, ELEM3), getElem2D(GB,4,5, ELEM4), getElem2D(GB,5,6, ELEM5),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5], !.

% 6 éléments sur diagonale
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==1,  getElem2D(GB,2,1, ELEM1), getElem2D(GB,3,2, ELEM2), getElem2D(GB,4,3, ELEM3), getElem2D(GB,5,4, ELEM4), getElem2D(GB,6,5, ELEM5), getElem2D(GB,7,6, ELEM6),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5, ELEM6], !.
getDiag2(GB,LENGTH, _, _, DIAG):- LENGTH==0,  getElem2D(GB,1,1, ELEM1), getElem2D(GB,2,2, ELEM2), getElem2D(GB,3,3, ELEM3), getElem2D(GB,4,4, ELEM4), getElem2D(GB,5,5, ELEM5), getElem2D(GB,6,6, ELEM6),  DIAG=[ELEM1,ELEM2,ELEM3, ELEM4, ELEM5, ELEM6], !.