/*
 * Code Stata pour la semaine 9 :
 * Exercices 9.1, 9.2, 9.3, 9.4, 9.5, 9.6 et 9.7.
 *
 */

/* Exercice 9.1 */
infile poids using "sirds.dat", clear
gen status = 0
replace status = 1 if _n>27
label define labstatus 0 "décédé" 1 "vivant"
label values status labstatus

by status, sort: summarize poids

histogram poids, frequency by(status)

ttest poids, by(status)

display r(p)

/* Exercice 9.2 */
clear all
input DHH LHH

list in 1/5

tabstat DHH LHH, save

return list
matrix m = r(StatTotal)
mat li m
display m[1,2] - m[1,1]

gen sdif = LHH - DHH
tabstat sdif, stats(mean sd)

histogram sdif, percent bin(8) start(0)

ttest DHH == LHH

graph hbar DHH LHH, bargap(20)

/* Exercice 9.3 */
tabi 26 21\ 38 44, exact chi2 expected

/* Exercice 9.4 */
clear all
input traitement infarctus N

label define tx 0 "Placebo" 1 "Aspirine"
label values traitement tx
label define ouinon 0 "Non" 1 "Oui"
label values infarctus ouinon
list

graph bar (asis) N, over(infarctus) asyvars over(traitement) legend(title("Infarctus"))

catplot infarctus traitement [fw=N] if infarctus==1, percent

tabodds infarctus traitement [fweight=N], or base(2)

/* Exercice 9.5 */
use polymorphism.dta
by genotype: summarize age

by genotype: summarize age, detail

graph box age, over(genotype)

histogram age, by(genotype, col(3))

oneway age genotype

robvar age, by(genotype)

oneway age genotype, tabulate

display invttail(14-3, 0.025)

collapse (mean) agem=age (sd) ages=age (count) n=age, by(genotype)
generate agelci = agem - invttail(n-1, 0.025)*(ages/sqrt(n))
generate ageuci = agem + invttail(n-1, 0.025)*(ages/sqrt(n))

gen ageb = agem-agelci
serrbar agem ageb genotype, xlabel(1 "1.6/1.6" 2 "1.6/0.7" 3 "0.7/0.7")

regress age i.genotype

lincom 3.genotype - 2.genotype

lincom _cons + 1.genotype

/* Exercice 9.6 */
stack A B C, into(pb) clear
rename _stack tx
label define txlab 1 "A" 2 "B" 3 "C"
label values tx txlab
list in 1/5

tabstat pb, stats(mean sd n) by(tx)

graph box pb, over(tx)

oneway pb tx, bonferroni

kwallis pb, by(tx)

kwallis2 pb, by(tx)

/* Exercice 9.7 */
use "weights.dta", clear
list in 1/5

tabulate PARITY

tabstat WEIGHT, stats(mean sd) by(PARITY) format(%9.2f)

oneway WEIGHT PARITY

anova WEIGHT PARITY

histogram WEIGHT, by(PARITY) freq

stripplot WEIGHT, over(PARITY) stack height(.4) center vertical width(.3)

twoway scatter WEIGHT PARITY, jitter(3) xlabel(1 "Singleton" 2 "One sibling" 3 "2 siblings" 4 "3 more")

robvar WEIGHT, by(PARITY)

recode PARITY (1=1) (2=2) (3/4=3), gen(PARITY2)

tabulate PARITY PARITY2

oneway WEIGHT PARITY2

quietly: regress WEIGHT i.PARITY2
contrast p.PARITY2, noeffects

regress WEIGHT PARITY2
