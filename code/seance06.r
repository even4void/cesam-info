macrolid <- matrix(c(157,119,52,103), nr=2, 
                   dimnames=list(traitement=c("A","B"), 
                     infection=c("Non","Oui")))
macrolid


barchart(prop.table(macrolid, 1), xlab="Fréquence relative", 
         auto.key=list(space="top", column=2, title="Infection"))


chisq.test(macrolid, correct=FALSE)


chisq.test(macrolid, correct=FALSE)$expected


(52*119)/(103*157)
macrolid.bis <- matrix(nc=2, nr=2)
macrolid.bis[,1:2] <- macrolid[,2:1]  # échange colonne 1 et 2
or <- (macrolid.bis[1,1]*macrolid.bis[2,2]) / (macrolid.bis[2,1]*macrolid.bis[1,2])
or


se <- sqrt(sum(1/macrolid.bis))  # erreur standard
or * exp(qnorm(0.025)*se)        # borne inf. 95 %
or * exp(qnorm(0.975)*se)        # borne sup. 95 %


library(vcd)
oddsratio(macrolid.bis, log=FALSE)
confint(oddsratio(macrolid.bis, log=FALSE))


library(epiR)
epi.2by2(macrolid[,c(2,1)])


macrolid.centre <- matrix(c(98,27,152,106,26,22), nr=3, byrow=TRUE)
rownames(macrolid.centre) <- paste("Centre", 1:3, sep=":")
colnames(macrolid.centre) <- c("Non","Oui")
macrolid.centre


chisq.test(macrolid.centre)


tab1 <- matrix(c(8,19,51,47), nr=2)
tab2 <- matrix(c(35,71,91,61), nr=2)
tab3 <- matrix(c(9,13,15,11), nr=2)
colnames(tab1) <- colnames(tab2) <- colnames(tab3) <- c("Oui","Non")
rownames(tab1) <-rownames(tab2) <- rownames(tab3) <- c("A","B")


## macrolid.centre <- rbind(centre1=apply(tab1, 2, sum), 
##                          centre2=apply(tab2, 2, sum), 
##                          centre3=apply(tab3, 2, sum))
## chisq.test(macrolid.centre)


macrolid2 <- array(c(tab1, tab2, tab3), dim=c(2,2,3))
dimnames(macrolid2) <- list(c("A","B"), c("Oui","Non"), paste("Centre", 1:3, sep=":"))
macrolid2
mantelhaen.test(macrolid2)


tab1.or <- oddsratio(tab1, log=FALSE)
tab2.or <- oddsratio(tab2, log=FALSE)
tab3.or <- oddsratio(tab3, log=FALSE)
dfrm <- data.frame(centre=factor(1:3), or=c(tab1.or, tab2.or, tab3.or), 
                   rbind(confint(tab1.or), confint(tab2.or), confint(tab3.or)))
dfrm
# -- %< -----
prepanel.ci <- function(x, y, lx, ux, subscripts, ...) {
  x <- as.numeric(x)
  lx <- as.numeric(lx[subscripts])
  ux <- as.numeric(ux[subscripts])
  list(ylim = range(x, ux, lx, finite=TRUE))
}
panel.ci <- function(x, y, lx, ux, subscripts, pch=16, ...) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  lx <- as.numeric(lx[subscripts])
  ux <- as.numeric(ux[subscripts])
  panel.abline(h = 1, col = "black", lty = 2)
  panel.abline(v = unique(x), col = "grey")
  panel.arrows(x, lx, x, ux, col = 'black',
               length = 0.05, unit = "native",
               angle = 90, code = 3)
  panel.xyplot(x, y, pch = pch, ...)
}   
# -- %< -----
xyplot(or ~ centre, data=dfrm, pch=20, ylab="Odds-ratio (IC 95 %)", 
       lx=dfrm$lwr, ux=dfrm$upr, prepanel=prepanel.ci, panel=panel.ci)


tab <- as.table(matrix(c(815,115,208,327), nrow=2, byrow=TRUE, 
                       dimnames=list(EST=c("+","-"), CAD=c("+","-"))))
tab


library(epiR)
epi.tests(tab)


library(epicalc)
roc.from.table(tab, graph=FALSE)


sck <- read.table("data/sck.dat", header=FALSE)
names(sck) <- c("ck", "pres", "abs")
summary(sck)


sum(sck[,c("pres","abs")])


ni <- apply(sck[,c("pres","abs")], 2, sum)


sck$pres.prop <- sck$pres/ni[1]
sck$abs.prop <- sck$abs/ni[2]


apply(sck[,c("pres.prop","abs.prop")], 2, sum)


xyplot(pres.prop + abs.prop ~ ck, data=sck, type=c("b", "g"),
       auto.key=TRUE, ylab="Fréquence")


glm.res <- glm(cbind(pres, abs) ~ ck, data=sck, family=binomial)
summary(glm.res)


glm.pred <- predict(glm.res, type="response")
names(glm.pred) <- sck$ck


glm.pred[glm.pred >= 0.5]


sck$malade <- sck$pres/(sck$pres+sck$abs)
xyplot(glm.pred ~ sck$ck, type="l", 
       ylab="Probabilité", xlab="ck", 
       panel=function(...) {
         panel.xyplot(...)
         panel.xyplot(sck$ck, sck$malade, pch=19, col="grey")
       })


sck.expand <- data.frame(ck=c(rep(sck$ck, sck$pres), rep(sck$ck, sck$abs)), 
                         malade=c(rep(1, ni[1]), rep(0, ni[2])))
table(sck.expand$malade)
with(sck.expand, tapply(malade, ck, sum))


glm.res2 <- glm(malade ~ ck, data=sck.expand, family=binomial)
sck.expand$prediction <- ifelse(predict(glm.res2, type="response") >= 0.5, 1, 0)
with(sck.expand, table(malade, prediction))


classif.tab <- with(sck.expand, table(malade, prediction))
sum(diag(classif.tab))/sum(classif.tab)


library(ROCR)
pred <- prediction(predict(glm.res2, type="response"), sck.expand$malade)
perf <- performance(pred, "tpr","fpr")
plot(perf, ylab="Sensibilité", xlab="1-Spécificité")
grid()
abline(0, 1, lty=2)


performance(pred, "auc")@"y.values"


bp <- read.table("data/hdis.dat", header=TRUE)
str(bp)


blab <- c("<117","117-126","127-136","137-146",
          "147-156","157-166","167-186",">186")
clab <- c("<200","200-209","210-219","220-244",
          "245-259","260-284",">284")
bp$bpress <- factor(bp$bpress, labels=blab)
bp$chol <- factor(bp$chol, labels=clab)


str(bp)
summary(bp)


round(xtabs(hdis/total ~ bpress + chol, data=bp), 2)


blab2 <- c(111.5,121.5,131.5,141.5,151.5,161.5,176.5,191.5)
# levels(bp$bpress) <- blab2
# bp$bpress <- as.numeric(as.character(bp$bpress))
bp$bpress <- rep(blab2, each=7)
dfrm <- aggregate(bp[,c("hdis","total")], list(bpress=bp[,"bpress"]), sum)


head(dfrm, 5)


logit <- function(x) log(x/(1-x))
dfrm$prop <- dfrm$hdis/dfrm$total
dfrm$logit <- logit(dfrm$hdis/dfrm$total)


## log((dfrm$hdis/dfrm$total)/(1-dfrm$hdis/dfrm$total))


dfrm


## glm(cbind(hdis, total-hdis) ~ bpress, data=dfrm, family=binomial)


summary(glm(cbind(hdis, total-hdis) ~ bpress, data=dfrm, family=binomial))


glm.res <- glm(cbind(hdis, total-hdis) ~ bpress, data=dfrm, family=binomial)
predict(glm.res)


cbind(dfrm, logit.predit=predict(glm.res))


dfrm$prop.predit <- predict(glm.res, type="response") 
f <- function(x) 1/(1+exp(-(coef(glm.res)[1]+coef(glm.res)[2]*x)))
xyplot(hdis/total ~ bpress, data=dfrm, aspect=1.2, cex=.8,
       xlab="Pression artérielle", ylab="Probabilité infarctus",
       panel=function(x, y, ...) {
         panel.xyplot(x, y, col="gray30", pch=19, ...)
         panel.curve(f, lty=3, col="gray70")
         panel.points(x, dfrm$prop.predit, col="gray70", ...)
       })


alcool <- matrix(c(666,104,109,96), nr=2, dimnames=list(c("Témoin","Cas"), c("<80",">=80")))
alcool


library(vcd)
oddsratio(alcool, log=FALSE)


confint(oddsratio(alcool, log=FALSE))


prop.test(c(96,109), c(200,775), correct=FALSE)


library(reshape)
alcool.df <- melt(alcool)
names(alcool.df) <- c("maladie", "exposition", "n")
levels(alcool.df$maladie) <- c(1,0)
alcool.df$maladie <- relevel(alcool.df$maladie, "0")


glm.res <- glm(maladie ~ exposition, data=alcool.df, family=binomial, weights=n)
summary(glm.res)


exp(coef(glm.res)[2])
exp(confint(glm.res)[2,])


alcool2 <- data.frame(expos=c("<80",">=80"), cas=c(104,96), total=c(104+666, 96+109))
summary(glm(cbind(cas, total-cas) ~ expos, data=alcool2, family=binomial))


## summary(glm(cas/total ~ expos, data=alcool2, family=binomial, weights=total))
