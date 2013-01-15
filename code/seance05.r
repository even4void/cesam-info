cystic <- read.table("data/cystic.dat", header=TRUE)
str(cystic)
summary(cystic)


cystic$Sex <- factor(cystic$Sex, labels=c("M","F"))
table(cystic$Sex)


with(cystic, cor(PEmax, Weight))


with(cystic, cor.test(PEmax, Weight))


library(psych)
r.test(25, 0.300, 0.6363)


splom(cystic[,-3], varname.cex=0.7, cex=.8)


round(cor(cystic[,-3]), 3)


round(cor(cystic[,-3], method="spearman"), 3)


library(ppcor)
with(cystic, pcor.test(PEmax, Weight, Age))


cystic$Age.ter <- cut(cystic$Age, breaks=quantile(cystic$Age, c(0,0.33,0.66,1)), 
                      include.lowest=TRUE)
cystic2 <- subset(cystic, as.numeric(Age.ter) %in% c(1,3))
cystic2$Age.ter <- factor(cystic2$Age.ter)
xyplot(PEmax ~ Weight, data=cystic2, groups=Age.ter, auto.key=list(corner=c(0,1)))


dfrm <- read.csv2("data/quetelet.csv")
head(dfrm)
dfrm$TAB <- factor(dfrm$TAB, labels=c("NF","F"))
summary(dfrm[,-1])


with(dfrm, cor.test(PAS, QTT, conf.level=0.90))


reg.res <- lm(PAS ~ QTT, data=dfrm)
summary(reg.res)
coef(reg.res)


xyplot(PAS ~ QTT, data=dfrm, groups=TAB, type=c("p", "g", "r"),
       auto.key=list(points=FALSE, lines=TRUE))


reg.res2 <- lm(PAS ~ QTT, data=dfrm, subset=TAB=="F")
summary(reg.res2)
coef(reg.res2)


fram <- read.csv("data/Framingham.csv")
head(fram)
str(fram)


table(fram$sex)
fram$sex <- factor(fram$sex, labels=c("M","F"))


apply(fram, 2, function(x) sum(is.na(x)))


with(fram, table(sex[!is.na(bmi)]))


xyplot(sbp ~ bmi | sex, data=fram, type=c("p","g"), alpha=0.5, cex=0.7, pch=19)


with(subset(fram, sex=="M"), cor(sbp, bmi, use="pair"))
with(subset(fram, sex=="F"), cor(sbp, bmi, use="pair"))


library(psych)
r.test(n=2047, r12=0.23644, n2=2643, r34=0.37362)


library(gridExtra)
p1 <- histogram(~ bmi, data=fram)
p2 <- histogram(~ log(bmi), data=fram)
p3 <- histogram(~ sbp, data=fram)
p4 <- histogram(~ log(sbp), data=fram)
grid.arrange(p1, p2, p3, p4)


reg.resM <- lm(log(sbp) ~ log(bmi), data=fram, subset=sex=="M")
reg.resF <- lm(log(sbp) ~ log(bmi), data=fram, subset=sex=="F")
summary(reg.resM)   # Hommes
confint(reg.resM)
summary(reg.resF)   # Femmes
confint(reg.resF)


res <- data.frame(pente=c(coef(reg.resM)[2], coef(reg.resF)[2]), 
                  rbind(confint(reg.resM)[2,], confint(reg.resF)[2,]))
rownames(res) <- c("M","F")
colnames(res)[2:3] <- c("2.5 %", "97.5 %")
round(res, 3)


## data(birthwt, package="MASS")
## str(birthwt)
## head(birthwt, 5)
## yesno <- c("No","Yes")
## ethn <- c("White","Black","Other")
## birthwt <- within(birthwt, {
##   low <- factor(low, labels=yesno)
##   race <- factor(race, labels=ethn)
##   smoke <- factor(smoke, labels=yesno)
##   ui <- factor(ui, labels=yesno)
##   ht <- factor(ht, labels=yesno)
## })


xyplot(bwt ~ lwt | race, data=birthwt, layout=c(3,1), type=c("p","g"), aspect=0.8)


reg.res <- lm(bwt ~ scale(lwt, scale=FALSE), data=birthwt)
summary(reg.res)


reg.res2 <- lm(bwt ~ race, data=birthwt)
summary(reg.res2)


aov.res <- aov(bwt ~ race, data=birthwt)
summary(aov.res)


summary.lm(aov.res)


anova(reg.res2)


grp.means <- with(birthwt, tapply(bwt, race, mean))
grp.means[2:3] - grp.means[1]     # 1er modèle de régression (reg.res2)


m <- lm(bwt ~ lwt, data=birthwt)
d <- data.frame(lwt=60)
predict(m, newdata=d, interval="confidence") 


m <- lm(bwt ~ lwt + race, data=birthwt)
d <- data.frame(lwt=c(55, 60), race=rep("Other", 2))
predict(m, d, interval="confidence")
