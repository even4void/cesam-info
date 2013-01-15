x <- c(24.9,25.0,25.0,25.1,25.2,25.2,25.3,25.3,25.3,25.4,25.4,25.4,25.4,
       25.5,25.5,25.5,25.5,25.6,25.6,25.6,25.7,25.7,25.8,25.8,25.9,26.0)


median(x)
mean(x)


quantile(x)


table(x)


m <- names(table(x)[table(x)==max(table(x))])


var(x)


xc <- cut(x, breaks=c(24.9,25.2,25.5,25.8,26.0), include.lowest=TRUE, right=FALSE)
table(xc)


histogram(~ x, type="count")


s <- c(7,47,58,74,177,232,273,285,317,429,440,445,455,468,495,497,532,571,
       579,581,650,702,715,779,881,900,930,968,1077,1109,1314,1334,1367,
       1534,1712,1784,1877,1886,2045,2056,2260,2429,2509)


median(s)


table(s <= 900)


quantile(s, 0.9)


table(s <= quantile(s, 0.9))


tailles <- scan("data/elderly.dat", na.strings=".")


sum(is.na(tailles))
table(is.na(tailles))


m <- mean(tailles, na.rm=TRUE)    # moyenne
s <- sd(tailles, na.rm=TRUE)      # écart-type
n <- sum(!is.na(tailles))         # nombre d'observations
m - qnorm(0.975) * s/sqrt(n)      # borne inf. IC 95 %
m + qnorm(0.975) * s/sqrt(n)      # borne sup. IC 95 %


## m + c(-1,1) * qnorm(0.975) * s/sqrt(n)


densityplot(~ tailles)


data(birthwt, package="MASS")
str(birthwt)
head(birthwt, 5)


yesno <- c("No","Yes")
ethn <- c("White","Black","Other")
birthwt <- within(birthwt, {
  low <- factor(low, labels=yesno)
  race <- factor(race, labels=ethn)
  smoke <- factor(smoke, labels=yesno)
  ui <- factor(ui, labels=yesno)
  ht <- factor(ht, labels=yesno)
})


## birthwt$low <- factor(birthwt$low, labels=yesno)
## birthwt$race <- factor(birthwt$race, labels=ethn)


birthwt$lwt <- birthwt$lwt/2.2


summary(birthwt$lwt)


IQR(birthwt$lwt)


quantile(birthwt$lwt)


lwt.quartiles <- quantile(birthwt$lwt)
lwt.quartiles
lwt.quartiles[4] - lwt.quartiles[2]


histogram(~ lwt, data=birthwt, type="count", xlab="Poids de la mère (kg)", ylab="Effectifs")


table(birthwt$smoke)
round(prop.table(table(birthwt$smoke))*100, 1)


prop.test(table(birthwt$smoke), correct=FALSE)


prop.test(74, 189, correct=FALSE)


barchart(prop.table(table(birthwt$smoke))*100, horizontal=FALSE, 
         ylab="Proportion (%)", ylim=c(-5,105))


birthwt$age.dec <- cut(birthwt$age, breaks=quantile(birthwt$age, c(0, 0.33, 0.66, 1)), 
                       include.lowest=FALSE)
table(birthwt$age.dec)


with(birthwt, table(age.dec, low))


tab <- with(birthwt, table(age.dec, low))
prop.table(tab, 2)[,"Yes"]


res <- rbind(table(birthwt$race), prop.table(table(birthwt$race))*100)
rownames(res) <- c("n","%")
round(res, 2)


library(Hmisc)
summary(low ~ race + smoke + ui + ht + age, data=birthwt, method="reverse")
