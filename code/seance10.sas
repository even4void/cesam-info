/*
 * Code SAS pour la semaine 10 :
 * Exercices 10.1, 10.2, 10.3, 10.4, 10.5, et 10.6.
 *
 */

/* Exercice 10.1 */
DATA cystic;
  INFILE  "C:\data\cystic.dat" firstobs=2;
  INPUT Sub Age Sex Height Weight BMP FEV RV FRC TLC PEmax;
RUN;

PROC CORR DATA=cystic fisher;
    VAR PEmax Weight;
RUN;

PROC CORR DATA=cystic fisher (RHO0=0.3);
    VAR PEmax Weight;
RUN;

PROC CORR DATA=cystic PLOTS=matrix (nvar=all histogram);
    VAR sex Age Height Weight BMP FEV RV FRC TLC PEmax;
RUN;

PROC CORR DATA=cystic SPEARMAN KENDALL;
    VAR sex Age Height Weight BMP FEV RV FRC TLC PEmax;
RUN;

PROC CORR DATA=cystic PLOTS=scatter(alpha=.33 .66);
    VAR PEmax Weight;
    PARTIAL age;
RUN;

/* Exercice 10.2 */
PROC IMPORT OUT= WORK.QUETELET
    DATAFILE= "C:\quetelet.txt"
    DBMS=DLM REPLACE;
    DELIMITER='3B'x;
    GETNAMES=YES;
    DATAROW=2;
RUN;

PROC CORR DATA=quetelet fisher (alpha=0.10);
    VAR PAS QTT;
RUN;

PROC REG DATA=quetelet;
    MODEL pas=qtt;
RUN;

PROC SGPLOT DATA=quetelet;
  SCATTER x= qtt y=pas / group=tab;
  REG x=qtt y=pas;
RUN;

DATA fumeur; SET quetelet;
    IF TAB=1;
RUN;

PROC REG DATA=fumeur;
    MODEL pas=qtt;
RUN;

/* Exercice 10.3 */
PROC IMPORT OUT= WORK.FRAMINGHAM
    DATAFILE= "C:\data\Framingham.csv"
    DBMS=CSV REPLACE;
    GETNAMES=YES;
    DATAROW=2;
RUN;

PROC SGPLOT DATA=FRAMINGHAM;
  SCATTER x=bmi y=sbp / group=sex;
  REG x=bmi y=sbp;
RUN;

PROC GLM DATA=framingham;
    CLASS sex;
    MODEL sbp=bmi sex bmi*sex;
RUN;

PROC GLM DATA=framingham;
    CLASS sex;
    MODEL sbp=bmi sex;
RUN;

PROC SORT DATA=framingham;
    BY sex;
RUN;

PROC REG DATA=framingham;
    MODEL sbp=bmi / clb;
    BY sex;
RUN;

/* Exercice 10.4 */
DATA mucovisi;
    INPUT infection traitement nombre;
    CARDS;
    0 1 157
    1 1 52
    0 0 119
    1 0 103
    ;
RUN;

PROC FORMAT;
    VALUE ouinon 0="Non" 1="Oui";
    VALUE treat 0="Placebo (B)" 1="Traitement (A)";
RUN;

PROC FREQ DATA=mucovisi ORDER=data;
    TABLES traitement*infection / expected chisq;
    WEIGHT nombre;
    FORMAT infection ouinon. traitement treat.;
RUN;

PROC FREQ DATA=mucovisi ORDER=data;
    TABLES traitement*infection / relrisk;
    WEIGHT nombre;
    FORMAT infection ouinon. traitement treat.;
RUN;

DATA mucocentres;
    INPUT centre infection traitement nombre;
    CARDS;
    1 0 1 51
    1 1 1 8
    1 0 0 47
    1 1 0 19
    2 0 1 91
    2 1 1 35
    2 0 0 61
    2 1 0 71
    3 0 1 15
    3 1 1 9
    3 0 0 11
    3 1 0 13
    ;
RUN;

PROC FREQ DATA=mucocentres ORDER=data;
    TABLES centre*traitement*infection / cmh;
    WEIGHT nombre;
    FORMAT infection ouinon. traitement treat.;
RUN;
    
/* Exercice 10.5 */
DATA sck;
    INPUT ck pres abs;
    CARDS;
    0       2      88
    40       13      26
    80       30      8
    120       30      5
    160       21      0
    200       19      1
    240       18      1
    280       13      1
    320       19      0
    360       15      0
    400       7      0
    440       8      0
    480       35      0
    ;
RUN;

DATA sck1; SET sck;
    particip=pres+abs;
RUN;

PROC SUMMARY DATA=sck1 PRINT sum;
    VAR particip;
RUN;

DATA sck2; SET sck1;
    p_pres=pres/particip;
    p_abs=abs/particip;
RUN;

PROC SGPLOT DATA=sck2;
   SERIES x=ck y=p_pres;
   SERIES x=ck y=p_abs;
RUN;

PROC LOGISTIC DATA=sck2;
    MODEL pres/particip=ck;
    OUTPUT OUT=fic1 PRED=p1;
RUN;

PROC PRINT DATA=fic1;
    VAR ck p1;
RUN;

PROC LOGISTIC DATA=sck2;
    MODEL pres/particip=ck;
   SCORE OUT=score1;
RUN;

PROC LOGISTIC DATA=sck2 OUTMODEL=sasuser.ckmodel;
    MODEL pres/particip=ck;
    SCORE DATA=sck2 OUT=score2;
RUN;

DATA ck3;
    INPUT ck;
    CARDS;
    0
    5
    10
    15
    20
    25
    30
    35
    40
    45
    50
    55
    60
    65
    66
    67
    68
    69
    70
    71
    75
    80
    85
    90
    95
    100
    110
    ;
RUN;

PROC LOGISTIC INMODEL=sasuser.Ckmodel;
    SCORE DATA=Ck3 OUT=Score3;
RUN;

PROC SGPLOT DATA=fic1;
   SCATTER x=ck y=p_pres;
   SCATTER x=ck y=p1;
RUN;

PROC LOGISTIC DATA=sck2 PLOTS=roc(id=prob);
    MODEL pres/particip=ck;
    ROC ck;
RUN;

/* Exercice 10.6 */
DATA alcool;
    INPUT Conso Cancer nombre;
    CARDS;
    1 1 96
    0 1 104
    1 0 109
    0 0 666
    ;
RUN;

PROC FORMAT;
    VALUE alcool 1=">= 80" 0="<80";
    VALUE cas    1="Cas" 0="Temoins";
RUN;

PROC FREQ DATA=alcool;
    TABLES cancer*conso / relrisk;
    WEIGHT nombre;
    FORMAT conso alcool. cancer cas.;
RUN;

PROC FREQ DATA=alcool;
    TABLES cancer*conso / chisq;
    WEIGHT nombre;
    FORMAT conso alcool. cancer cas.;
RUN;

PROC LOGISTIC DATA=alcool;
   MODEL Cancer = conso;
   WEIGHT nombre;
RUN;



    
