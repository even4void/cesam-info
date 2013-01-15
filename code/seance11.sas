/*
 * Code SAS pour la semaine 11 :
 * Exercices 11.1 et 10.2.
 *
 */

/* Exercice 11.1 */
PROC IMPORT OUT=WORK.PBC
    DATAFILE="C:\data\pbc.txt"
    DBMS=TAB REPLACE;
    GETNAMES=YES;
    DATAROW=2;
RUN;

PROC CONTENTS DATA=pbc; RUN;

PROC FORMAT;
    VALUE treat 1="Placebo" 2="DPCA";
    VALUE Stat 0="Vivant" 1="décédé";
RUN;

PROC FREQ DATA=pbc;
    TABLES status*rx;
    FORMAT status stat. rx treat.;
RUN;

PROC SGPLOT DATA=pbc;
  SCATTER x=years  y=number / group=status;
  FORMAT status stat. ;
RUN;

PROC SORT DATA=pbc;
    BY rx;
RUN;

PROC SUMMARY DATA=pbc PRINT median;
    VAR years;
    BY rx;
    FORMAT rx treat.;
RUN;

DATA pbc; SET pbc;
    EVE=0;
    IF years GE 10.5 THEN EVE=1;
RUN;

PROC FORMAT;
    VALUE pos 0="Suivi < 10.5"   1="Suivi >= 10.5";
RUN;

PROC FREQ DATA=pbc;
    TABLES EVE*status;
    FORMAT eve pos. status stat.;
RUN;

DATA transplant;
    INPUT number @@;
    tranpl=1;
    CARDS;
    5 105 111 120 125 158 183 241 246 247 254 263 264 265 274 288 291 295 297 345 361 362 375 380 383
    ;
RUN;

PROC SORT DATA=transplant;
    BY number;
RUN;

DATA all;
    MERGE pbc transplant;
    BY number;
RUN;

DATA pbc_transplant;
    SET all;
    years_J=years*365;
    IF tranpl=1;
    IF sample=1;
RUN;

PROC MEANS DATA=pbc_transplant;
    VAR age;
RUN;

PROC FREQ DATA=pbc_transplant;
    TABLES sex;
RUN;

PROC SUMMARY DATA=pbc_transplant PRINT median;
    VAR years_J;
RUN;

PROC LIFETEST DATA=pbc;
   TIME years*status(0);
RUN;

PROC LIFETEST DATA=pbc PLOT=survival(cl);
   TIME years*status(0);
RUN;

PROC LIFETEST DATA=pbc PLOT=survival(cl) /*conftype=linear*/;
    TIME years*status (0);
    STRATA rx;
RUN;

PROC LIFETEST DATA=pbc PLOT=survival(cl) /*conftype=linear*/;
    TIME years*status (0);
    TEST rx;
RUN;

DATA pbc;
    SET pbc;
    age_classe=1;
    IF age GT 40 THEN age_classe=2;
    IF age GT 55 THEN age_classe=3;
    PRC print;
    VAR age age_classe;
RUN;

PROC LIFETEST DATA=pbc PLOT=survival(cl);
    TIME years*status (0);
    STRATA age_classe;
    TEST rx;
run;

PROC PHREG DATA=pbc;
    MODEL years*status(0)=rx;
RUN;

/* Exercice 11.2 */
PROC IMPORT OUT=WORK.Prostate
    DATAFILE= "C:\data\prostate.dat"
    DBMS=DLM REPLACE;
    DELIMITER='00'x;
    GETNAMES=YES;
    DATAROW=2;
RUN;

PROC LIFETEST DATA==prostate;
   TIME time*status(0);
RUN;

PROC LIFETEST DATA=prostate;
    TIME time*status(0);
    STRATA treatment;
RUN;

PROC LIFETEST DATA=prostate PLOT=survival(cl) /*conftype=linear*/;
    TIME time*status (0);
    STRATA treatment;
RUN;

