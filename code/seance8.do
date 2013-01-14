/*
 * Code Stata pour la semaine 8 :
 * Exercices 8.1, 8.2, 8.3, 8.4, 8.5 et 8.6.
 *
 */

/* Exercice 8.1 */
input x
list in 1/4

summarize

replace x = 3.64 in 4
replace x = . in 7   
summarize

/* Exercice 8.2 */
display log10(50)
count if X <= log10(50)

egen Xm = median(10^X) if X > log10(50)
display round(Xm)

/* Exercice 8.3 */
infile using anorexia
describe

tabulate Group

replace Before = Before/2.2
replace After = After/2.2

generate diff = After - Before
summarize diff

tabstat diff, by(Group) stats(mean min max)

/* Exercice 8.4 */
infile x using "saisie_x.txt"             

tabstat x, stats(mean median)

egen varx = sd(x)
di varx^2

egen xc = cut(x), at(24.8,25.2,25.5,25.8,26.1) label
table xc, contents(min x max x)

tabulate xc, plot

histogram x, frequency

/* Exercice 8.5 */
infile tailles using "elderly.dat", clear

count if tailles == .

ci tailles

histogram tailles, kdensity

/* Exercice 8.6 */
infile low age lwt race smoke ptl ht ui ftv bwt using "birthwt.dat", clear
describe

label define yesno 0 "no" 1 "yes"
label define ethn 1 "White" 2 "Black" 3 "Other"
label values low smoke ui ht yesno
label values race ethn
list in 1/5

replace lwt = lwt/2.2

summarize lwt, detail

egen lwtiqr = iqr(lwt)
display lwtiqr

histogram lwt

tabulate smoke
prtest smoke == .5

gen one = 1
graph bar (sum) one, over(smoke) ytitle("Effectif")

xtile ageter = age, nq(3)
tabulate ageter

_pctile age, n(3)
return list

table ageter, contents(freq min age min age)

egen ageter = cut(age), group(3) label
tabulate ageter low, col nofreq

tabulate race
  
by low, sort: summarize age lwt ptl ftv
by low, sort: tab1 race smoke ht ui
