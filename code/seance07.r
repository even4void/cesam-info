pb <- read.table("data/pbc.txt", header=TRUE)
names(pb)[1:20]


pb$rx <- factor(pb$rx, labels=c("Placebo", "DPCA"))
table(pb$rx)
pb$sex <- factor(pb$sex, labels=c("M","F"))


prop.table(table(pb$status))


prop.table(with(pb, table(status, rx)), 1)


xyplot(number ~ years, data=pb, pch=pb$status, cex=.8)


with(pb, tapply(years, rx, median))


with(pb, table(status[years > 10.5]))
with(pb, table(sex[years > 10.5 & status == 1]))


subset(pb, years > 10.5 & status == 1, sex)


idx <- c(5,105,111,120,125,158,183,241,246,247,254,263,264,265,274,288,291,295,
         297,345,361,362,375,380,383)
table(pb$status[pb$number %in% idx])


pb.transp <- subset(pb, number %in% idx, c(age, sex, years))


mean(pb.transp$age)
table(pb.transp$sex)
median(pb.transp$years * 365)


library(survival)
head(with(pb, Surv(time=years, event=status)))


s <- survfit(Surv(years, status) ~ 1, data=pb)
summary(s)


plot(s)


s <- survfit(Surv(years, status) ~ rx, data=pb)


s


plot(s)


survdiff(Surv(years, status) ~ rx, data=pb)


survdiff(Surv(years, status) ~ rx, data=pb, rho=1)


agec <- cut(pb$age, c(26, 40, 55, 79))


survdiff(Surv(years, status) ~ rx + strata(agec), data=pb)


summary(coxph(Surv(years, status) ~ rx + strata(agec), data=pb))


placebo.time <- c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
placebo.status <- rep(1, length(placebo.time))
mp.time <- c(6,6,6,6,7,9,10,10,11,13,16,17,19,20,22,23,25,32,32,34,35)
mp.status <- c(1,1,1,0,1,0,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0)
mp <- data.frame(tx=rep(c("Placebo","6-MP"), c(21,21)),
                 time=c(placebo.time, mp.time), 
                 status=c(placebo.status, mp.status))
summary(mp)


with(mp, Surv(time, status))


s <- survfit(Surv(time, status) ~ tx, data=mp)
summary(s)


plot(survfit(Surv(time, status) ~ tx, data=mp, subset=tx=="6-MP"))


plot(s, fun="cumhaz")


s


prostate <- read.table("data/prostate.dat", header=TRUE)
str(prostate)
head(prostate)
prostate$Treatment <- factor(prostate$Treatment)
table(prostate$Status)


library(survival)
with(prostate, Surv(time=Time, event=Status))


survfit(Surv(Time, Status) ~ 1, data=prostate)


survfit(Surv(Time, Status) ~ Treatment, data=prostate)


plot(survfit(Surv(Time, Status) ~ Treatment, data=prostate))


survdiff(Surv(time=Time, event=Status) ~ Treatment, data=prostate)


summary(coxph(Surv(time=Time, event=Status) ~ Treatment, data=prostate))
