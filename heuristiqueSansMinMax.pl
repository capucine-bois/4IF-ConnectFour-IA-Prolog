:- use_module(library(lists)).
% Heuristique sans min-max

    % NOTE A DISCUTER EN PREMIER LIEU : fait-on vraiment seulement +3 par rapport à +2 ? (je me comprends)

    % ATTENTION (dis à d'autres endroits) : il manque la vérification que les 4 colonnes sont atteignables dans les "obtenirScore"
    % Autre point à prendre en compte : je fais des tests généraux sur les "obtenirScore" donc potentiellement en faisant des calculs sur des colonnes qui n'existent pas
    % Par exemple si on sintéresse à une case de la colonne n°5 je peux aller vérifier la couleur d'une case sur la colonne N°5+3, soit une colonne inexsitante
    % je propose donc de faire un tableau plus grand, un gameBoard plus grand, avec des cases valant autre chose que P, P1 ou 0
    % on renvoie avec adjacence() le nombre max de pions actuellement adjacents tel qu'il est possible d'arriver à 4 et la liste des colonnes permettant d'augmenter ce total
    % Gb = retour de getGameBoard.
    :- dynamic LISTESCOREP/7.
LISTESCOREP(0,0,0,0,0,0,0).
    :- dynamic LISTESCOREP2/7.
LISTESCOREP2(0,0,0,0,0,0,0).
    adjacence(Gb,P, SCORES) :- obtenirCaseVideParColonne(CasesVides),obtenirScorePourChaqueCase(CasesVides,ScoresActuels,Gb,P), assert(LISTESCOREP, SCORES), scoreColonne(Gb,P, SCORES), assert(LISTESCOREP, SCORES), scoreDiagonale(Gb,P, SCORES), assert(LISTESCOREP, SCORES).


    % pour chaque colonne on va chercher la première case vide
    obtenirCaseVideParColonne(CasesVides) :- CasesVides = [A,B,C,D,E,F,G], obtenirCaseVide(1,A),obtenirCaseVide(2,B),obtenirCaseVide(3,C),obtenirCaseVide(4,D),obtenirCaseVide(5,E),obtenirCaseVide(6,F),obtenirCaseVide(7,G).
    % on cherche la première case vide de la colonne
    obtenirCaseVide(ColonneActuelle, CaseVide) :- col(ColonneActuelle, A), A = [A1,A2,A3,A4,A5,A6], (A1 == 0, caseVide = 1);(A2 == 0, caseVide = 2);(A3 == 0, caseVide = 3);(A4 == 0, caseVide = 4);(A5 == 0, caseVide = 5);(A6 == 0, caseVide = 6).
    % on cherche le score de la première case vide de chaque colonne
    % on remet le scoreTmp à 1 avant chaque appel
    % on apelle la fonction suivante une fois pour chaque joueur
    obtenirScorePourChaqueCase(CasesVides,ScoresActuels,Gb,P) :-CasesVides = [A,B,C,D,E,F,G], ScoresActuels = [A1,B1,C1,D1,E1,F1,G1],ScoreTmp=1,obtenirScoreLigne(CasesVides,1,Gb,A1,P,ScoreTmp),assert(A1), ScoreTmp=1,
    obtenirScoreColonne(CasesVides,1,Gb,A1,P,ScoreTmp),assert(A1),ScoreTmp=1, obtenirScoreDiagonale(),assert(A1), etc : pour chaque colonne



    % ATTENTION : là je fais comme si la colonne était la colonne du milieu, il faut faire des disjonctions de cas pour les colonnes du côté (ou un tableau plus grand comme écrit en préambule)
    % ATTENTION BIS : il faut rajouter la vérif qu'on peut aller à 4, pas dur (vérifier que les cases de la ligne sont non renseignées) mais chiant. IL faudrait faire un prédicat
    % ScoreTmp est le seulement le score pour la ligne, Score correspond à la somme pour les diagonales colonne et ligne

    obtenirScoreLigne(CasesVides,Colonne,Gb,Score,P,ScoreTmp) :- COL1=Gb[Colonne+1],COL2=Gb[Colonne+2],COL3=Gb[Colonne+3],COL4=Gb[Colonne-1],COL5=Gb[Colonne-2],COL6=Gb[Colonne-3],
    % on teste si le score serait d'au moins 2 (donc si y a aune case vide à côté)
    (COL1[CasesVides[Colonne]]==P;COL4[CasesVides[Colonne]]==P,ScoreTmp=2),
    (ScoreTmp==2,(COL1[CasesVides[Colonne]]==P,COL4[CasesVides[Colonne]]==P);(COL1[CasesVides[Colonne]]==P,COL2[CasesVides[Colonne]]==P);(COL4[CasesVides[Colonne]]==P,COL5[CasesVides[Colonne]]==P),ScoreTmp=3),
    % on vérifie maintenant si le score vaut 4, il faudrait break à cet endroit là
    (ScoreTmp==3,(COL3[CasesVides[Colonne]]==P;COL6[CasesVides[Colonne]]==P),ScoreTmp=4,Score=10000000,!),Score=Score+ScoreTmp.

    obtenirScoreLigne(+CasesVides,+COL,+P, -Score) :- P\== 0, (
                ScoreTmp is 1,
                 (COL1 is COL+1, COL2 is COL+2, COL3 is COL+3);
                 (COL1 is COL-1, COL2 is COL+1, COL3 is COL+2);
                 (COL1 is COL-2, COL2 is COL-1, COL3 is COL+1);
                 (COL1 is COL-3, COL2 is COL-2, COL3 is COL-1)),
                 col(COL,W),  W = [A1,A2,A3,A4,A5,A6],
                 col(COL1,X), X = [B1,B2,B3,B4,B5,B6],
                 col(COL2,Y), Y = [C1,C2,C3,C4,C5,C6],
                 col(COL3,Z), Z = [D1,D2,D3,D4,D5,D6], (
                 nth1(COL,CasesVides,CaseVide),
                 CaseVide==1,(B1==0;B1==P, incr(ScoreTmp) ;!), (C1==0;C1==P, incr(ScoreTmp) ;!), (D1==0;D1==P, incr(ScoreTmp) ;!), assert(ScoreTmp), !;
                 CaseVide==2,(B2==0;B2==P, incr(ScoreTmp) ;!), (C2==0;C2==P, incr(ScoreTmp) ;!), (D2==0;D2==P, incr(ScoreTmp) ;!), assert(ScoreTmp), !;
                 CaseVide==3,(B3==0;B3==P, incr(ScoreTmp) ;!), (C3==0;C3==P, incr(ScoreTmp) ;!), (D3==0;D3==P, incr(ScoreTmp) ;!), assert(ScoreTmp), !;
                 CaseVide==4,(B4==0;B4==P, incr(ScoreTmp) ;!), (C4==0;C4==P, incr(ScoreTmp) ;!), (D4==0;D4==P, incr(ScoreTmp) ;!), assert(ScoreTmp), !;
                 CaseVide==5,(B5==0;B5==P, incr(ScoreTmp) ;!), (C5==0;C5==P, incr(ScoreTmp) ;!), (D5==0;D5==P, incr(ScoreTmp) ;!), assert(ScoreTmp), !;
                 CaseVide==6,(B6==0;B6==P, incr(ScoreTmp) ;!), (C6==0;C6==P, incr(ScoreTmp) ;!), (D6==0;D6==P, incr(ScoreTmp) ;!), assert(ScoreTmp), !;
                 
                 ).


    % même principe que pour la ligne mais pour les colonnes
    obtenirScoreColonne(CasesVides,Colonne,Gb,Score,P,ScoreTmp):- CaseActuelle=CasesVides[Colonne],COLActuelle = Gb[Colonne],(COLActuelle[CaseActuelle-1]==P;COLActuelle[CaseActuelle+1]==P,ScoreTmp=2),
(ScoreTmp==2,COLActuelle[CaseActuelle-2]==P;COLActuelle[CaseActuelle+2]==P,ScoreTmp=3),(ScoreTmp=3,COLActuelle[CaseActuelle+3]==P;COLActuelle[CaseActuelle-3],Score=1000000,!),Score=Score+ScoreTmp.


    obtenirScoreDiagonale(CasesVides,Colonne,Gb,Score,P,ScoreTmp) :- COL1=Gb[Colonne+1],COL2=Gb[Colonne+2],COL3=Gb[Colonne+3],COL4=Gb[Colonne-1],COL5=Gb[Colonne-2],COL6=Gb[Colonne-3],
    % changement de pricipe ici, trop de cas donc on y va comme un cochon
% Note à moi même : ici c'est le seul cas où on peut augmenter les scores tmp parce qu'une case peut être impliqué dans deux diagonales
((COL1[CasesVides[Colonne]-1]==P;COL1[CasesVides[Colonne]+1]==P,ScoreTmp=ScoreTmp+1),(COL4[CasesVides[Colonne]-1]==P;COL4[CasesVides[Colonne]+1]==P,ScoreTmp=ScoreTmp+1),
((COL1[CasesVides[Colonne]-1]==P,(COL4[CasesVides[Colonne]+1]==P ; COL2[CasesVides[Colonne]-2]),ScoreTmp=ScoreTmp+1);(COL1[CasesVides[Colonne]+1]==P,(COL4[CasesVides[Colonne]-1]==P ; COL2[CasesVides[Colonne]+2])),
((COL4[CasesVides[Colonne]-1]==P,(COL1[CasesVides[Colonne]+1]==P ; COL5[CasesVides[Colonne]-2]),ScoreTmp=ScoreTmp+1);(COL4[CasesVides[Colonne]+1]==P,(COL1[CasesVides[Colonne]-1]==P ; COL5[CasesVides[Colonne]+2])),
Score = Score + ScoreTmp,
% cas où on est à 4 donc
((COL1[CasesVides[Colonne]-1]==P,(COL2[CasesVides[Colonne]-2]==P, (COL3[CasesVides[Colonne]-3]==P;COL4[CasesVides[Colonne]+1]==P));(COL4[CasesVides[Colonne]+1]==P, COL5[CasesVides[Colonne]+2]==P));
(COL4[CasesVides[Colonne]+1]==P,COL5[CasesVides[Colonne]+2]==P,(COL1[CasesVides[Colonne]-1]==P;COL6[CasesVides[Colonne]+3]==P));
(COL1[CasesVides[Colonne]+1]==P,(COL2[CasesVides[Colonne]+2]==P, (COL3[CasesVides[Colonne]+3]==P;COL4[CasesVides[Colonne]-1]==P));(COL4[CasesVides[Colonne]-1]==P, COL5[CasesVides[Colonne]-2]==P));
(COL4[CasesVides[Colonne]-1]==P,COL5[CasesVides[Colonne]-2]==P,(COL1[CasesVides[Colonne]+1]==P;COL6[CasesVides[Colonne]-3]==P)),Score=10000000).



% récupère le max d'une liste
% trouvé sur le net
    maxx_list([X],X).
        maxx_list([X|Xs],S) :- maxx_list(Xs,Y),(X>=Y,S=X,!;S=Y).


% on modifie les valeurs de la liste pour avoir seulement les valeurs absolues

    abs_list([X|Xs],Indice,S) :- X>=0,nth1(Indice,S,X),Indice2 is Indice+1,abs_list(Xs,Indice2,S).
    abs_list([X|Xs],Indice,S) :- X<0,ToPut is -X,nth1(Indice,S,ToPut), Indice2 is Indice+1,abs_list(Xs,Indice2,S).
    abs_list(_,8,_).


    % on effectue les sous-stractions sur les listes de score pour chaque colonne des deux joueurs
    listeSoustraite(ListeP1,ListeP2,ListeFinale) :- nth1(1,ListeP1,Elem1P1),nth1(1,ListeP2,Elem1P2),ElemToadd1 is Elem1P2-Elem1P1,nth1(1,ListeFinale,ElemToadd1),
    nth1(2,ListeP1,Elem2P1),nth1(2,ListeP2,Elem2P2),ElemToadd2 is Elem2P2-Elem2P1,nth1(2,ListeFinale,ElemToadd2),
        nth1(3,ListeP1,Elem3P1),nth1(3,ListeP2,Elem3P2),ElemToadd3 is Elem3P2-Elem3P1,nth1(3,ListeFinale,ElemToadd3),
            nth1(4,ListeP1,Elem4P1),nth1(4,ListeP2,Elem4P2),ElemToadd4 is Elem4P2-Elem4P1,nth1(4,ListeFinale,ElemToadd4),
                nth1(5,ListeP1,Elem5P1),nth1(5,ListeP2,Elem5P2),ElemToadd5 is Elem5P2-Elem5P1,nth1(5,ListeFinale,ElemToadd5),
                    nth1(6,ListeP1,Elem6P1),nth1(6,ListeP2,Elem6P2),ElemToadd6 is Elem6P2-Elem6P1,nth1(6,ListeFinale,ElemToadd6),
                        nth1(7,ListeP1,Elem7P1),nth1(7,ListeP2,Elem7P2),ElemToadd7 is Elem7P2-Elem7P1,nth1(7,ListeFinale,ElemToadd7).
    % application de l'heuristique
    % CaseToPlay est la case où il faut jouer
    choixDeLaCase(P) :- getGameBoard(Gb), adjacence(Gb,P,LISTESCOREP),adjacence(Gb,P2,LISTESCOREP2),listeSoustraite(LISTESCOREP,LISTESCOREP2,ListeToAbs),abs_list(ListeToAbs,ListeFinale,0),max_list(ListeFinale,CaseToPlay).
