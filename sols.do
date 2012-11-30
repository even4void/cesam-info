set scheme s1mono

/* Exercice 3.1
 * Poids à la naissance et syndrôme de détresse respiratoire
 * idiopathique aïgue.
 */
infile poids using "sirds.dat", clear
gen status = 0
replace status = 1 if _n>27
label define labstatus 0 "décédé" 1 "vivant"
label values status labstatus
by status, sort: summarize poids
histogram poids, frequency by(status)
ttest poids, by(status)

/* Exercice 4.1
 * Âge de diagnostic du cancer du sein et génotype du récepteur
 * à oestrogènes.
 */
use polymorphism.dta
by genotype: summarize age
by genotype: summarize age, detail
graph box age, over(genotype)
oneway age genotype
robvar age, by(genotype)
oneway age genotype, tabulate
display invttail(14-3, 0.025)
regress age i.genotype
lincom 3.genotype - 2.genotype
lincom _cons + 1.genotype
