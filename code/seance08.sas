/*
 * Code SAS pour la semaine 8 :
 * Exercices 8.1, 8.2, 8.3, 8.4, 8.5 et 8.6.
 *
 */

/* Exercice 8.1 */
DATA Exercice1_1;
    INPUT x;
    CARDS;
    3.68
    2.21
    2.45
    8.64
    4.32
    3.43
    5.11
    3.87
    ;
RUN;

PROC SUMMARY DATA=Exercice1_1 PRINT n min max range;
    VAR x;
RUN;

DATA Exercice1_1;
    INPUT x;
    CARDS;
    3.68
    2.21
    2.45
    3.64    /* (a) */
    4.32
    3.43
    .       /* (b) */
    3.87
    ;
RUN;

/* Exercice 8.2 */
DATA Exercice1_2;
    INPUT X;
    detect=1;
    IF X <= log10(50) THEN detect=0;
    CARDS;
    3.64
    2.27
    1.43
    1.77
    4.62
    3.04
    1.01
    2.14
    3.02
    5.62
    5.51
    5.51
    1.01
    1.05
    4.19
    2.63
    4.34
    4.85
    4.02
    5.92
    ;
RUN;

PROC FREQ DATA=exercice1_2;
    TABLES detect;
RUN;

DATA detect;
    SET exercice1_2;
    Y=exp(X*log(10));
    IF detect=1;
RUN;

PROC SUMMARY DATA=detect PRINT median;
    VAR Y;
RUN;

/* Exercice 8.3 */
DATA anorexia;
    INFILE "C:\data\anorexia.dat" firstobs=2 dlm="09"X;
    INPUT  groupe $ 1-2  before 3-7   after 8-13;
RUN;

PROC FORMAT;
    VALUE Therapie
        1='Thérapie comportementale'
        2='Thérapie familiale'
        3='Thérapie contrôle'
        ;
RUN;

PROC FREQ DATA=anorexia;
    TABLES groupe;
    FORMAT groupe therapie.;
RUN;
    
DATA anorexia;
    SET anorexia;
    before_kg=before/2.2;
    after_kg=after/2.2;
RUN;

DATA anorexia;
    SET anorexia;
    diff=after_kg-before_kg;
RUN;

PROC SORT DATA=anorexia;
    BY groupe;
RUN;

PROC SUMMARY DATA=anorexia PRINT n mean min max range;
    VAR diff;
    BY groupe;
    FORMAT groupe therapie.;
RUN;

/* Exercice 8.4 */
DATA X;
    INPUT X;
    CARDS;
    24.9
    25.0
    25.0
    25.1
    25.2
    25.2
    25.3
    25.3
    25.3
    25.4
    25.4
    25.4
    25.4
    25.5
    25.5
    25.5
    25.5
    25.6
    25.6
    25.6
    25.7
    25.7
    25.8
    25.8
    25.9
    26.0
    ;
RUN;

PROC SUMMARY DATA=X PRINT mean median mode;
    VAR X;
RUN;

PROC SUMMARY DATA=X PRINT var;
    VAR X;
RUN;

DATA X;
    SET X;
    X_classes=1;
    IF X GE 25.2 THEN X_classes=2;
    IF X GE 25.5 THEN X_classes=3;
    IF X GE 25.8 THEN X_classes=4;
RUN;

PROC FREQ DATA=X;
    TABLES X_classes;
RUN;

PROC FORMAT;
    VALUE classes   24.9-25.1="Intervalle 1"
                    25.2-25.4="Intervalle 2"
                    25.5-25.7="Intervalle 3"
                    25.8-26.0="Intervalle 4";
RUN;

PROC FREQ DATA=X;
    TABLES X;
    FORMAT X classes.;
RUN;

PROC GCHART DATA=X;
    Hbar X;
RUN;  

PROC GCHART DATA=X;
    Vbar X;
RUN;

/* Exercice 8.5 */
DATA elderly;
    INFILE  "C:\data\elderly.dat" dlm="09"X ;
    INPUT  taille @@ ;
RUN;

PROC SUMMARY DATA=elderly PRINT nmiss;
    VAR taille;
RUN;

PROC SUMMARY DATA=elderly PRINT clm uclm lclm;
    VAR taille;
RUN;

PROC UNIVARIATE DATA=elderly;
    VAR taille;
    HISTOGRAM taille / kernel;
RUN;

/* Exercice 8.6 */
PROC IMPORT OUT=WORK.BIRTHWT
    DATAFILE= "C:\data\birthwt.dat"
    DBMS=DLM REPLACE;
    DELIMITER='20'x;
    GETNAMES=NO;
    DATAROW=1;
RUN;

DATA birthwt1;
    SET birthwt;
    low = var1;
    age = var2;
    lwt = var3 ;
    race = var4 ;
    smoke = var5;
    ptl = var6 ;
    ht = var7 ;
    ui = var8 ;
    ftw = var9 ;
    bwt = var10 ;
    DROP var1-var10;
RUN;

PROC FORMAT;
      VALUE  low 1="Poids inférieur à 2.5 Kg"
                 0="Poids supérieur à 2.5 Kg";
      VALUE  ethnicite 1="White"
                       2="Black"
                       3="Other";
      VALUE  tabac 1="consommation tabac durant grossesse"
                   0="Pas de consommation tabac durant grossesse";
      VALUE  Hypert 1="Antecedent d hypertension"
                    0="Pas d antecedent d hypertension";
      VALUE  uterine 1="Manisfestation d irritabilite uterine"
                     0="Pas de manisfestation d irritabilite uterine";
RUN;

PROC FREQ DATA=birthwt1; 
  TABLES low race smoke ui ht;
  FORMAT low low. race ethnicite. smoke tabac. ht hypert. ui uterine.;
RUN;

DATA birthwt2;
    SET birthwt1;
    lwt=lwt/2.2; 
RUN;

PROC SUMMARY DATA=birthwt2 PRINT mean median qrange;
    VAR lwt;
RUN;

PROC GCHART DATA=birthwt2;
    Hbar lwt;
RUN;

PROC UNIVARIATE DATA=birthwt2;
  VAR lwt;
  HISTOGRAM lwt / kernel;
RUN;

PROC SUMMARY DATA=birthwt2 PRINT clm uclm lclm;
    VAR smoke;
RUN;

PROC GCHART DATA=birthwt2;
    Vbar smoke;
RUN;

PROC GCHART DATA=birthwt2;
    Hbar smoke;
RUN;

/* PROC UNIVARIATE DATA=birthwt2; VAR lwt; RUN; */

PROC RANK DATA=birthwt2 OUT=ranking; VAR age;
    RANKS ordre;
RUN;

DATA birthwt3;
    SET ranking;
    age_classe=1;
    IF ordre GT 189/3 THEN age_classe=2; 
    IF ordre GT 2*(189/3) THEN age_classe=3;
RUN;

PROC FREQ DATA=birthwt3;
    TABLES age_classe*low;
RUN;

PROC FREQ DATA=birthwt3;
    TABLES race;
    FORMAT race ethnicite.;
RUN;

PROC FREQ DATA=birthwt3; 
    TABLES (race smoke ui ht age_classe)*low;
    FORMAT low low. race ethnicite. smoke tabac. ht hypert. ui uterine.;
RUN;
