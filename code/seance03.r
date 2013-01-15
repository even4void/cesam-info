poids <- scan("data/sirds.dat")
status <- factor(rep(c("décédé", "vivant"), c(27,23)))


t.test(poids ~ status, var.equal=TRUE)


histogram(~ poids | status, type="count")


bwplot(poids ~ status, xlab="status")


data(sleep)
mean(sleep$extra[sleep$group == 1])  # D. hyoscyamine hydrobromide
mean(sleep$extra[sleep$group == 2])  # L. hyoscyamine hydrobromide
m <- with(sleep, tapply(extra, group, mean))
m[2] - m[1]


sdif <- sleep$extra[sleep$group == 2] - sleep$extra[sleep$group == 1]
c(mean=mean(sdif), sd=sd(sdif))


histogram(~ sdif, breaks=seq(0, 5, by=0.5), xlab="LHH - DHH")


t.test(extra ~ group, data=sleep, paired=TRUE)


barchart(with(sleep, tapply(extra, group, mean)))


occ1 <- c(1.83,0.50,1.62,2.48,1.68,1.88,1.55,3.06,1.30)
occ2 <- c(0.878,0.647,0.598,2.05,1.06,1.29,1.06,3.14,1.29)


xyplot(occ2 ~ occ1, type=c("p","g"), xlab="Première visite", ylab="Seconde visite")


wilcox.test(occ1, occ2, paired=TRUE)


regime <- matrix(c(26,38,21,44), nrow=2)
dimnames(regime) <- list(c("amélioration","pas d'amélioration"), c("régime","pas de régime"))
regime
chisq.test(regime)


chisq.test(regime)$expected


fisher.test(regime)


aspirine <- matrix(c(28,18,656,658), nrow=2) 
dimnames(aspirine) <- list(c("Placebo","Aspirine"), c("Oui","Non"))
aspirine


sum(aspirine["Placebo",])


apply(aspirine, 1, sum)


apply(aspirine, 2, sum)


round(aspirine[,"Oui"]/sum(aspirine[,"Oui"]) * 100, 2)


barchart(aspirine, horizontal=FALSE, stack=FALSE, ylab="Effectifs")


prop.infarctus <- aspirine[,"Oui"]/sum(aspirine[,"Oui"])
barchart(prop.infarctus, ylab="Fréquence relative d'infarctus")


(aspirine["Aspirine","Oui"]/sum(aspirine["Aspirine",])) / (aspirine["Placebo","Oui"]/sum(aspirine["Placebo",]))


library(vcd)
asp.or <- oddsratio(aspirine, log=FALSE) 
print(list(or=asp.or , conf.int=confint(asp.or))) 
summary(oddsratio(aspirine))


library(foreign)
hc <- read.spss("data/health-camp.sav", to.data.frame=TRUE)


head(hc)


str(hc)


table(hc[,c("BEFORE","AFTER")])
round(prop.table(table(hc[,c("BEFORE","AFTER")])), 2)


margin.table(table(hc[,c("BEFORE","AFTER")]), 1)
margin.table(table(hc[,c("BEFORE","AFTER")]), 2)


hc.tab <- table(hc[,c("BEFORE","AFTER")])
mcnemar.test(hc.tab)


binom.test(6, 6+29)
mcnemar.test(hc.tab, correct=FALSE)
