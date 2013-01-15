/*
 * Code Stata pour la semaine 11 :
 * Exercices 11.1 et 11.2.
 *
 */

/* Exercice 11.1 */
insheet using "prostate.dat", delimiter(" ")
list in 1/5

tabulate status

stset time, failure(status)

stci, by(treatment) p(50)

sts graph, by(treatment) censored(s)

sts test treatment

/* Exercice 11.2 */
insheet using "pbc.txt", tab
describe, simple

label define trt 1 "Placebo" 2 "DPCA"
label define sexe 0 "M" 1 "F"
label values rx trt
label values sex sexe

tabulate status
tabulate status rx, row

separate number, by(status)
twoway scatter number0 number1 years, msymbol(S O)

tabstat years, by(rx) stats(median) nototal

tabulate status if years > 10.49

tabulate sex if years > 10.49 & status == 1

preserve
egen idx = anymatch(number), values(5 105 111 120 125 158 183 241 246 247 254 263 264 265 274 288 291 295 297 345 361 362 375 380 383)
keep if idx
gen days = years*365
tabstat age sex days, stats(mean median sum)

restore

stset years, failure(status)

sts list

sts graph, ci censored(single)
