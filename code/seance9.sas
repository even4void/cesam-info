/*
 * Code SAS pour la semaine 9 :
 * Exercices 9.1, 9.2, 9.3, 9.4, 9.5, 9.6 et 9.7.
 *
 */

/* Exercice 9.1 */
DATA DCD;
INPUT poids @@;
deces=1;
DATALINES;
1.050 1.175 1.230 1.310 1.500 1.600 1.720 1.750 1.770 2.275
2.500 1.030 1.100 1.185 1.225 1.262 1.295 1.300 1.550 1.820
1.890 1.940 2.200 2.270 2.440 2.560 2.730
;
RUN;

DATA VIV;
INPUT poids @@;
deces=0;
CARDS;
1.130 1.575 1.680 1.760 1.930 2.015 2.090 2.600 2.700 2.950
3.160 3.400 3.640 2.830 1.410 1.715 1.720 2.040 2.200 2.400
2.550 2.570 3.005
;
RUN;

DATA DRIA; SET DCD VIV; RUN;

PROC TTEST DATA=DRIA PLOTS=all;
  CLASS deces;
  VAR poids;
RUN;

/* Exercice 9.2 */
DATA DHH;
INPUT GMSD @@;
CARDS;
0.7 -1.6 -0.2 -1.2 -0.1 3.4 3.7 0.8 0.0 2.0
;
RUN;

DATA LHH;
INPUT GMSL @@;
CARDS;
1.9 0.8 1.1 0.1 -0.1 4.4 5.5 1.6 4.6 3.4
;
RUN;

DATA HH; MERGE DHH LHH; diff_GMS=GMSL-GMSD; RUN;

PROC SUMMARY DATA=HH PRINT n mean var lclm uclm; VAR GMSD GMSL diff_GMS; RUN;

PROC GCHART DATA=hh; Hbar diff_gms / midpoints=(0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5 5.5 6); RUN;

PROC TTEST DATA=hh;
  PAIRED GMSD*GMSL;
RUN;

/* Exercice 9.3 */
DATA symptom;
INPUT regime amelioration effectif;
CARDS;
1 1 26
0 1 21
1 0 38
0 0 44
;
RUN;

PROC FORMAT; VALUE ouinon 1="Oui" 0="Non"; RUN;
    
PROC FREQ DATA=symptom ORDER=data;
  TABLES amelioration * regime / chisq;
  WEIGHT effectif;
  FORMAT regime amelioration ouinon.;
RUN;

/* Exercice 9.4 */
DATA myocarde;
INPUT Infractus traitement nombre;
CARDS;
1 1 28
1 2 18
2 1 656
2 2 658
;
RUN;

PROC FORMAT; 
  VALUE ouinon 1="Oui" 2 = "Non";
  VALUE treat 1=" Placebo" 2 = "Aspirine";
RUN;

PROC FREQ DATA=myocarde;
  TABLES traitement*Infractus / all;
  WEIGHT nombre;
  FORMAT Infractus ouinon. traitement treat.;
RUN;

/* Exercice 9.5 */
PROC IMPORT OUT= WORK.polymorphism
            DATAFILE= "C:\data\polymorphism.dta"
            DBMS=STATA REPLACE;
RUN;

PROC GLM DATA=polymorphism; 
  CLASS genotype; 
  MODEL age=genotype; 
RUN;
    
PROC SORT DATA=polymorphism; BY genotype; RUN;

PROC GCHART DATA=polymorphism; Vbar age; BY genotype; RUN;
    
PROC SORT DATA=polymorphism; BY genotype; RUN;

PROC SUMMARY DATA=polymorphism PRINT n mean stddev ucl lcl; VAR age; BY genotype; RUN;

PROC GLM DATA=polymorphism; 
  CLASS genotype; 
  MODEL age=genotype;
  MEANS genotype / BON CLDIFF;
RUN;
    
PROC GLM DATA=polymorphism PLOT=MEANPLOT(CLBAND); 
  CLASS genotype; 
  MODEL age=genotype;
  MEANS genotype / BON CLM;
RUN;

/* Exercice 9.6 */


/* Exercice 9.7 */
