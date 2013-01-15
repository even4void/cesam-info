/*
 * Code Stata pour la semaine 10 :
 * Exercices 10.1, 10.2, 10.3, 10.4, 10.5, et 10.6.
 *
 */

/* Exercice 10.1 */
infile int (Sub Age Sex) Height Weight BMP FEV RV FRC TLC PEmax using "cystic.dat" in 2/26, clear

label define labsex 0 "M" 1 "F"
label values Sex labsex
tabulate Sex

correlate PEmax Weight
corrci PEmax Weight

pwcorr PEmax Weight, sig

graph matrix Age Height-PEmax

pwcorr Age Height-PEmax

spearman Age Height-PEmax, stats(rho)

pcorr PEmax Weight Age

xtile Age3 = Age, nq(3)

scatter PEmax Weight if Age3 != 2, mlab(Age3)

scatter PEmax Weight if Age3 == 1, msymbol(circle) || scatter PEmax Weight if Age3 == 3, msymbol(square) legend(label(1 "1st tercile") label(2 "3rd tercile"))

/* Exercice 10.2 */
insheet using "quetelet.csv", delim(";") clear
describe

destring qtt, dpcomma replace

label define ltab 0 "NF" 1 "F"
label values tab ltab
list in 1/5

summarize pas-tab

corrci pas qtt, level(90)

regress pas qtt

matrix b = e(b)
svmat b
di "pente = " b1 ", ordonnée origine = " b2

twoway lfit pas qtt if tab == 0, range(2 5) lpattern(dot) || scatter pas qtt if tab == 0, msymbol(square) || lfit pas qtt if tab == 1, range(2 5) || scatter pas qtt if tab == 1, msymbol(circle) legend(label(1 "") label(2 "NF") label(3 "") label(4 "F"))

regress pas qtt if tab == 1

/* Exercice 10.3 */
insheet using "Framingham.csv"
describe, simple
list in 1/5

label define labsex 1 "M" 2 "F"
label values sex labsex
tabulate sex

misstable summarize

tabulate sex if bmi < .

scatter sbp bmi, by(sex) msymbol(Oh)

by sex, sort: correlate sbp bmi

gen logbmi = log(bmi)
gen logsbp = log(sbp)

histogram bmi, saving(gphbmi)
histogram logbmi, saving(gphlogbmi)
histogram sbp, saving(gphsbp)
histogram logsbp, saving(gphlogsbp)
graph combine gphbmi.gph gphlogbmi.gph gphsbp.gph gphlogsbp.gph

regress logsbp logbmi if sex == 1, noheader
regress logsbp logbmi if sex == 2, noheader

/* Exercice 10.4 */
clear all
input str1 traitement str3 infection N
list

tabulate traitement infection [fweight=N], chi

return list

display %10.9f r(p)

tabulate traitement infection [fweight=N], chi expected nofreq

input traitement infection N

label define tx 0 "Placebo" 1 "Traitement"
label values traitement tx
label define ouinon 0 "Non" 1 "Oui"
label values infection ouinon

cc infection traitement [fweight=N], woolf exact

input infection centre N
tabulate infection centre [fweight=N], chi

input tx inf cen N

label define txlab 0 "A" 1 "B"
label define inflab 0 "Non" 1 "Oui"
label values tx txlab
label value inf inflab
table tx inf [fw=N], by(cen)

cc tx inf [freq=N], by(cen)

/* Exercice 10.5 */
infile ck pres abs using sck.dat

generate tot = pres+abs
egen ntot = sum(tot)
display ntot

blogit pres tot ck

display exp(_b[ck])

gen prop = pres/tot
predict pred, p
label variable prop "observed"
label variable pred "predicted"
graph twoway (line pred ck) (scatter prop ck), ytitle("Probabilité")

expand tot
bysort ck: gen infct = _n <= pres
logit infct ck, nolog

estat classification

lroc

lsens

/* Exercice 10.6 */
insheet using "cc_oesophage.csv", clear
label define yesno 0 "No" 1 "Yes" 
label values cancer yesno 
label define dose 0 "< 80g" 1 ">= 80g"
label values alcohol dose
list

egen ntot = sum(patients)
display ntot

tabulate cancer alcohol [fweight=patients], row

cc cancer alcohol [fweight=patients], woolf

prtesti 96 0.4800 109 0.1406

logistic alcohol cancer [freq=patients]

logit
