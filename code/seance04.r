library(foreign)
polymsm <- read.dta("data/polymorphism.dta")
head(polymsm)


with(polymsm, tapply(age, genotype, mean))
with(polymsm, tapply(age, genotype, sd))


bwplot(age ~ genotype, data=polymsm)


histogram(~ age | genotype, data=polymsm)


aov.res <- aov(age ~ genotype, data=polymsm)
summary(aov.res)


mse <- unlist(summary(aov.res))["Mean Sq2"]
se <- sqrt(mse)


ni <- table(polymsm$genotype)
n <- sum(ni)
m <- with(polymsm, tapply(age, genotype, mean))
lci <- m - qt(0.975, n-3) * se / sqrt(ni)
uci <- m + qt(0.975, n-3) * se / sqrt(ni)
rbind(lci, uci) 


library(epiR)
epi.conf(polymsm$age[polymsm$genotype=="1.6/1.6"], ctype = "mean.single")


mm <- as.data.frame(cbind(m, lci, uci))
mm$g <- levels(polymsm$genotype)
rownames(mm) <- NULL
dotplot(m ~ g, data=mm, ylim=c(40,75),
        panel=function(x, y, ...) {
          panel.dotplot(x, y, ...)
          panel.segments(x, mm$lci, x, mm$uci)
        })


## library(Hmisc)
## xYplot(Cbind(m,lci,uci) ~ 1:3, data=mm, scales=list(x=list(at=1:3, labels=mm$g)), 
##        xlab="", ylim=c(40,75))


pb <- c(19.8,20.5,23.7,27.1,29.6,29.9,
        15.9,19.7,20.8,21.7,22.5,24.0,
        15.4,17.1,18.2,18.5,19.3,21.2)
tx <- gl(3, 6, labels=c("A","B","C"))
dfrm <- data.frame(pb, tx)
head(dfrm, 8)


pbm <- matrix(pb, nc=3)
pbm


apply(pbm, 2, mean)
sapply(data.frame(pbm), mean)


library(reshape)
head(melt(data.frame(pbm)))


with(dfrm, tapply(pb, tx, mean))
with(dfrm, tapply(pb, tx, var))


bwplot(pb ~ tx, data=dfrm)


aov.res <- aov(pb ~ tx, data=dfrm)
summary(aov.res)


pairwise.t.test(dfrm$pb, dfrm$tx, p.adjust="bonf")


kruskal.test(pb ~ tx, data=dfrm)


## wilcox.test(pb ~ tx, data=dfrm, subset=tx!="C")


with(dfrm, wilcox.test(pb[tx=="A"], pb[tx=="B"]))


library(foreign)
weights <- read.spss("data/weights.sav", to.data.frame=TRUE)
str(weights)


table(weights$PARITY)
round(prop.table(table(weights$PARITY))*100, 1)


round(with(weights, tapply(WEIGHT, PARITY, mean)), 2)
round(with(weights, tapply(WEIGHT, PARITY, sd)), 2)


aov.res <- aov(WEIGHT ~ PARITY, data=weights)
summary(aov.res)


stripplot(WEIGHT ~ PARITY, data=weights, ylab="Poids (kg)", jitter.data=TRUE)


histogram(~ WEIGHT | PARITY, data=weights)


library(car)
leveneTest(WEIGHT ~ PARITY, data=weights)


PARITY2 <- weights$PARITY
levels(PARITY2)[3:4] <- "2 siblings or more"


aov.res2 <- aov(WEIGHT ~ PARITY2, data=weights)
summary(aov.res2)


levels(PARITY2)
lm.res <- lm(WEIGHT ~ as.numeric(PARITY2), data=weights)
summary(lm.res)


levels(as.ordered(PARITY2))
summary(lm(WEIGHT ~ as.ordered(PARITY2), data=weights))


load("data/bioluminescence.RData")
ls()


with(biolum, tapply(y, list(An=An, Na=Na), mean))
with(biolum, tapply(y, list(An=An, Na=Na), var))


xyplot(y ~ An, data=biolum, groups=Na, type=c("a","g"), 
       auto.key=list(corner=c(0,1), lines=TRUE, points=FALSE, title="Na", cex.title=.8))


summary(aov(y ~ An + Na, data=biolum))


summary(aov(y ~ An + Na + An:Na, data=biolum))
