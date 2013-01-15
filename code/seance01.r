x <- c(3.68,2.21,2.45,8.64,4.32,3.43,5.11,3.87)
x


length(x)


min(x)
max(x)
range(x)
range(x)[2] - range(x)[1]  # ou diff(range(x))


x[x == 8.64] <- 3.64
x


x[7] <- NA
x


log10(50)


X <- c(3.64,2.27,1.43,1.77,4.62,3.04,1.01,2.14,3.02,5.62,5.51,5.51,1.01,1.05,4.19,
       2.63,4.34,4.85,4.02,5.92)
length(X[X <= log10(50)])


Xc <- X[X > log10(50)]
round(median(10^Xc), 0)


## x <- scan("data/dosage.txt")


x <- scan("data/dosage.txt", what="character")
str(x)
head(x)


x[x == "2,914"] <- "2.914"
x <- as.numeric(x)
head(x)
round(mean(x), 3)


write.table(x, file="data/data.txt", row.names=FALSE)


id <- 1:10
age <- c(26,31,28,28,29,33,32,21,36,30)
sexe <- c("M","F","M","M","F","F","F","M","M","M")
qi <- c(126,123,114,125,134,141,123,114,122,127)
sse <- c("B","A","B","B","A","B","B","A","C","A")
qdv <- c(72,73,72,72,76,74,72,71,71,66)
dfrm <- data.frame(id, age, sexe, qi, sse, qdv)
dfrm


rm(id, age, sexe, qi, sse, qdv)
ls()


## write.csv(dfrm, file="data/dfrm.csv")


enquete <- read.csv("data/enquete.csv")
str(enquete)


head(enquete, n=10)


enquete[1:10,]


table(enquete$sexe) / nrow(enquete)


sum(enquete$sexe == "F") / nrow(enquete) 


nrow(enquete)
sum(complete.cases(enquete$sexe))
sum(complete.cases(enquete$qdv))


summary(enquete)


which(is.na(enquete$qdv))


anorex <- read.table("data/anorexia.dat", header=TRUE)
names(anorex)
head(anorex)


nrow(anorex)


table(anorex$Group)


anorex$Before <- anorex$Before/2.2
anorex$After <- anorex$After/2.2


## anorex$Before.kg <- anorex$Before/2.2
## anorex$After.kg <- anorex$After/2.2


anorex$poids.diff <- anorex$After - anorex$Before
head(anorex)


mean(anorex$poids.diff[anorex$Group == "g1"])
range(anorex$poids.diff[anorex$Group == "g1"])


tapply(anorex$poids.diff, anorex$Group, mean)
tapply(anorex$poids.diff, anorex$Group, range)


with(anorex, tapply(poids.diff, Group, mean))


biolum <- read.table("data/bioluminescence.dat", header=FALSE)
biolum


An <- rep(c("-","+"), c(20,10))
Na <- rep(c("-","+","-","+"), c(12,8,6,4))
biolum <- with(biolum, data.frame(y=c(V1,V2[1:8],V3[1:6],V4[1:4]), An, Na))
head(biolum)
summary(biolum)


with(biolum, tapply(y, list(An, Na), length))


save(biolum, file="data/bioluminescence.RData")
