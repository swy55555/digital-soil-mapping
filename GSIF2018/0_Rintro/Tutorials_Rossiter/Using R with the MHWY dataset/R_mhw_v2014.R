### R code from vignette source 'mhw_1'

###################################################
### code chunk number 1: mhw_1.Rnw:19-23
###################################################
options(prompt="> ", continue="+ ", digits=5, width=70, show.signif.stars=T)
par(mfrow=c(1,1))
rm(list=ls())
setwd("~/data/Journal/Monographs/StatsMonographs/MHW_Sweave/") # adjust to local path


###################################################
### code chunk number 2: mhw_1.Rnw:86-88
###################################################
sample <- runif(12, -1, 1)
print(sample)


###################################################
### code chunk number 3: mhw_1.Rnw:91-93
###################################################
sample <- round(sample, 2)
sample


###################################################
### code chunk number 4: mhw_1.Rnw:96-97
###################################################
(sample <- sort(sample))


###################################################
### code chunk number 5: mhw_1.Rnw:124-126
###################################################
(10 + 0)/2
(10 - 0)^2/12


###################################################
### code chunk number 6: mhw_1.Rnw:131-134
###################################################
sample <- runif(20, min=0, max=10)
mean(sample)
var(sample)


###################################################
### code chunk number 7: mhw_1.Rnw:143-146
###################################################
ls()
rm(sample)
ls()


###################################################
### code chunk number 8: mhw_1.Rnw:154-155
###################################################
help(round)


###################################################
### code chunk number 9: mhw_1.Rnw:169-174
###################################################
runif(1)
sort(runif(12))
sort(runif(12, 0, 5))
sort(runif(12, min=0, max=5))
sort(runif(max=5, n=12, min=0))


###################################################
### code chunk number 10: mhw_1.Rnw:181-182
###################################################
help.search("principal component")


###################################################
### code chunk number 11: mhw_1.Rnw:198-199
###################################################
help(prcomp)


###################################################
### code chunk number 12: mhw_1.Rnw:228-229 (eval = FALSE)
###################################################
## q()


###################################################
### code chunk number 13: mhw_1.Rnw:291-292 (eval = FALSE)
###################################################
## getwd()


###################################################
### code chunk number 14: mhw_1.Rnw:297-298 (eval = FALSE)
###################################################
## file.show("mhw.csv")


###################################################
### code chunk number 15: mhw_1.Rnw:314-315
###################################################
mhw <- read.csv("mhw.csv")


###################################################
### code chunk number 16: mhw_1.Rnw:330-331
###################################################
str(mhw)


###################################################
### code chunk number 17: mhw_1.Rnw:340-342
###################################################
names(mhw)
colnames(mhw)


###################################################
### code chunk number 18: mhw_1.Rnw:353-354
###################################################
class(mhw)


###################################################
### code chunk number 19: mhw_1.Rnw:366-368
###################################################
summary(mhw$grain)
summary(mhw$straw)


###################################################
### code chunk number 20: mhw_1.Rnw:375-376
###################################################
dim(mhw)


###################################################
### code chunk number 21: mhw_1.Rnw:386-392
###################################################
mhw[1,]
# third  column
# third column of first row
length(mhw[,3])
summary(mhw[,3])
mhw[1, 3]


###################################################
### code chunk number 22: mhw_1.Rnw:397-399
###################################################
# a named column
mhw[1, "grain"]


###################################################
### code chunk number 23: mhw_1.Rnw:406-408
###################################################
mhw[64,"grain"]
mhw[64, c("r", "c")]


###################################################
### code chunk number 24: mhw_1.Rnw:415-416
###################################################
mhw[1:3, 1:2]


###################################################
### code chunk number 25: mhw_1.Rnw:421-423
###################################################
summary(mhw[-(1:20),"grain"])
summary(mhw$grain[-(1:20)])


###################################################
### code chunk number 26: mhw_1.Rnw:428-431
###################################################
head(mhw[,3])
tail(mhw[,"grain"], n=10)
head(mhw$grain)


###################################################
### code chunk number 27: mhw_1.Rnw:442-445
###################################################
head(sort(mhw$straw), n=5)
head(order(mhw$straw), n=5)
head(mhw[order(mhw$straw), ] , n=5)


###################################################
### code chunk number 28: mhw_1.Rnw:458-459
###################################################
head(rev(sort(mhw$straw)), n=5)


###################################################
### code chunk number 29: mhw_1.Rnw:464-465
###################################################
head(sort(mhw$straw, decreasing=T), n=5)


###################################################
### code chunk number 30: mhw_1.Rnw:470-471
###################################################
tail(sort(mhw$straw), n=5)


###################################################
### code chunk number 31: mhw_1.Rnw:487-491
###################################################
(ix <- which(mhw$grain == max(mhw$grain)))
mhw[ix,]
(ix <- which(mhw$grain == min(mhw$grain)))
mhw[ix,]


###################################################
### code chunk number 32: mhw_1.Rnw:496-500
###################################################
(ix <- which.max(mhw$grain))
mhw[ix, ]
(ix <- which.min(mhw$grain))
mhw[ix, ]


###################################################
### code chunk number 33: mhw_1.Rnw:514-515
###################################################
mhw[which(mhw$straw > 8.8),]


###################################################
### code chunk number 34: mhw_1.Rnw:526-536
###################################################
col.right <- mhw[mhw$c==max(mhw$c),c("grain","straw","r")]
str(col.right)
(row.order <- order(col.right$grain, decreasing=T))
# no apparent order by rows
(col.right[row.order,])
t(col.right[row.order,])
# straw yields have some correspondence
dotchart(col.right[row.order,"straw"], main="Straw yields in decreasing grain-yield order")
# plot shows this also
rm(col.right, row.order)


###################################################
### code chunk number 35: mhw_1.Rnw:549-550
###################################################
save(mhw, file="mhw.RData")


###################################################
### code chunk number 36: mhw_1.Rnw:603-604
###################################################
stem(mhw$grain)


###################################################
### code chunk number 37: mhw_1.Rnw:621-622
###################################################
hist(mhw$grain)


###################################################
### code chunk number 38: mhw_1.Rnw:644-646
###################################################
hist(mhw$grain, breaks=seq(2.6, 5.2, by=.1), col="lightblue", border="red", main="Mercer-Hall uniformity trial", xlab="Grain yield, lb. per plot")
rug(mhw$grain)


###################################################
### code chunk number 39: mhw_1.Rnw:651-652
###################################################
seq(2.6, 5.2, by=.1)


###################################################
### code chunk number 40: mhw_1.Rnw:683-693
###################################################
hist(mhw$grain, breaks=seq(2.6, 5.2, by=.1), col="lavender", border="darkblue", main="Mercer-Hall uniformity trial",
     freq=F,
     #     probability=T,
     xlab="Grain yield, lb.\ per plot")
lines(density(mhw$grain), lwd=1.5)
lines(density(mhw$grain, adj=2), lwd=1.5, col="brown")
lines(density(mhw$grain, adj=.5), lwd=1.5, col="red")
text(2.5,0.95,"Default bandwidth", col="darkblue", pos=4)
text(2.5,0.90,"Double bandwidth", col="brown", pos=4)
text(2.5,0.85,"Half bandwidth", col="red", pos=4)


###################################################
### code chunk number 41: mhw_1.Rnw:702-711
###################################################
plot(density(mhw$grain) ,xlab="Grain yield, lb.\ per plot",
     lwd=1.5, ylim=c(0,1), col="darkblue",
     main="Mercer-Hall uniformity trial")
rug(mhw$grain)
lines(density(mhw$grain, adj=2), lwd=1.5, col="brown")
lines(density(mhw$grain, adj=.5), lwd=1.5, col="red")
text(2.5,0.85,"Default bandwidth", col="darkblue", pos=4)
text(2.5,0.80,"Double bandwidth", col="brown", pos=4)
text(2.5,0.75,"Half bandwidth", col="red", pos=4)


###################################################
### code chunk number 42: mhw_1.Rnw:730-741
###################################################
h <- hist(mhw$grain, breaks = seq(2.6, 5.2, by=.2), plot=F)
str(h)
plot(h, col = heat.colors(length(h$mids))[length(h$count)-rank(h$count)+1],
     ylim = c(0, max(h$count)+5),
     main="Frequency histogram, Mercer & Hall grain yield",
     sub="Counts shown above bar, actual values shown with rug plot",
     xlab="Grain yield, lb. per plot")
#          col=gray(1 - (h$count/length(mhw$grain)))
rug(mhw$grain)
text(h$mids, h$count, h$count, pos=3)
rm(h)


###################################################
### code chunk number 43: mhw_1.Rnw:756-757
###################################################
plot(mhw$grain, mhw$straw)  


###################################################
### code chunk number 44: mhw_1.Rnw:779-792
###################################################
plot(mhw$grain, mhw$straw, cex=0.8, pch=21, col="blue",
     bg="red", xlab="Grain yield, lb.\ plot-1",
     ylab="Straw yield, lb.\ per plot-1")
grid()
title(main="Mercer-Hall wheat uniformity trial")
abline(v=mean(mhw$grain), lty=2, col="blue")
abline(h=mean(mhw$straw), lty=2, col="blue")
points(mean(mhw$grain), mean(mhw$straw), pch=23, col="black",
       bg="brown", cex=2)
text(mean(mhw$grain), min(mhw$straw),
     paste("Mean:",round(mean(mhw$grain),2)), pos=4)
text(min(mhw$grain), mean(mhw$straw),
     paste("Mean:",round(mean(mhw$straw),2)), adj=c(0,-1))


###################################################
### code chunk number 45: mhw_1.Rnw:807-809 (eval = FALSE)
###################################################
## plot(mhw$grain, mhw$straw)  
## pts <- identify(mhw$grain, mhw$straw)


###################################################
### code chunk number 46: mhw_1.Rnw:811-812
###################################################
pts <- c(15, 35, 184, 284, 292, 295, 311, 337)


###################################################
### code chunk number 47: mhw_1.Rnw:827-830
###################################################
tmp <- mhw[ pts, ]
tmp[ order(tmp$grain), ]
rm(pts, tmp)


###################################################
### code chunk number 48: mhw_1.Rnw:870-873
###################################################
summary(mhw)
# summary(scale(mhw[,c("grain","straw")]))
summary(mhw$grain)


###################################################
### code chunk number 49: mhw_1.Rnw:885-890
###################################################
max(mhw$grain)
min(mhw$grain)
median(mhw$grain)
mean(mhw$grain)
quantile(mhw$grain, probs=c(.25, .75))


###################################################
### code chunk number 50: mhw_1.Rnw:901-904
###################################################
var(mhw$grain)
sd(mhw$grain)
IQR(mhw$grain)


###################################################
### code chunk number 51: mhw_1.Rnw:919-923
###################################################
require(e1071)
skewness(mhw$grain)
kurtosis(mhw$grain)
detach(package:e1071)


###################################################
### code chunk number 52: mhw_1.Rnw:939-940
###################################################
while (is.element("mhw", search())) detach(mhw)


###################################################
### code chunk number 53: mhw_1.Rnw:942-943
###################################################
search()


###################################################
### code chunk number 54: mhw_1.Rnw:946-947 (eval = FALSE)
###################################################
## summary(grain)


###################################################
### code chunk number 55: mhw_1.Rnw:954-955
###################################################
attach(mhw)


###################################################
### code chunk number 56: mhw_1.Rnw:957-959
###################################################
search()
summary(grain)


###################################################
### code chunk number 57: mhw_1.Rnw:974-976
###################################################
diff(range(grain))
diff(range(grain))/median(grain)


###################################################
### code chunk number 58: mhw_1.Rnw:985-987
###################################################
ix <- which.min(grain)
mhw[ix,"straw"]


###################################################
### code chunk number 59: mhw_1.Rnw:1001-1006
###################################################
# find all 
row.names(subset(mhw, grain<3))
quantile(grain)
quantile(grain, seq(0, 1, .1))
mhw[grain < quantile(grain, .01), ]


###################################################
### code chunk number 60: mhw_1.Rnw:1044-1045 (eval = FALSE)
###################################################
## fix(mhw)


###################################################
### code chunk number 61: mhw_1.Rnw:1060-1062
###################################################
gsr <- grain/straw
summary(gsr)


###################################################
### code chunk number 62: mhw_1.Rnw:1069-1072
###################################################
range(gsr)
diff(range(gsr))/median(gsr)
diff(range(grain))/median(grain)


###################################################
### code chunk number 63: mhw_1.Rnw:1085-1087
###################################################
if (!exists("gsr", where="mhw")) { mhw <- cbind(mhw, gsr) }
str(mhw)


###################################################
### code chunk number 64: mhw_1.Rnw:1091-1093 (eval = FALSE)
###################################################
## mhw <- cbind(mhw, gsr)
## str(mhw)  


###################################################
### code chunk number 65: mhw_1.Rnw:1100-1103
###################################################
ls()
rm(gsr)
ls()


###################################################
### code chunk number 66: mhw_1.Rnw:1109-1110 (eval = FALSE)
###################################################
## summary(gsr)


###################################################
### code chunk number 67: mhw_1.Rnw:1115-1116
###################################################
summary(mhw$gsr)


###################################################
### code chunk number 68: mhw_1.Rnw:1123-1124
###################################################
detach(mhw); attach(mhw)


###################################################
### code chunk number 69: mhw_1.Rnw:1126-1127
###################################################
summary(gsr)


###################################################
### code chunk number 70: mhw_1.Rnw:1136-1137
###################################################
save(mhw, file="mhw2.RData")

### R code from vignette source 'mhw_1a'

###################################################
### code chunk number 1: mhw_1a.Rnw:41-45
###################################################
plot(density(mhw$grain), col="darkblue",
     main="Grain yield, lb. per plot", lwd=1.5)
rug(mhw$grain, col="darkgreen")
grid()


###################################################
### code chunk number 2: mhw_1a.Rnw:58-70
###################################################
plot(ecdf(grain), pch=1,
     xlab="Mercer & Hall, Grain yield, lb. per plot", 
     ylab="Cumulative proportion of plots",
     main="Empirical CDF",
     sub="Quantiles shown with vertical lines")
q <- quantile(grain, c(.05, .1, .25, .75, .9, .95))
abline(v=q, lty=2)
abline(v=median(grain), col="blue")
abline(v=max(grain), col="green")
abline(v=min(grain), col="green")
text(q, 0.5, names(q))
rm(q)


###################################################
### code chunk number 3: mhw_1a.Rnw:83-87
###################################################
qqnorm(grain,
       main="Normal probability plot, grain yields (lb. plot-1)")
qqline(grain)
grid()


###################################################
### code chunk number 4: mhw_1a.Rnw:106-108
###################################################
mean(grain)
sd(grain)


###################################################
### code chunk number 5: mhw_1a.Rnw:117-130
###################################################
res <- 0.1
hist(grain, breaks=seq(round(min(grain),1)-res,
                       round(max(grain),1)+res, by=res),
     col="lightblue", border="red", freq=F,
     xlab="Wheat grain yield, lb. per plot",
     main="Mercer & Hall uniformity trial",
     sub="Theoretical distribution (solid), empirical density (dashed)")
grid()
rug(grain)
x <- seq(min(grain)-res, max(grain)+res, by=.01)
lines(x, dnorm(x, mean(grain), sd(grain)), col="blue", lty=1, lwd=1.8) 
lines(density(grain), lty=2, lwd=1.8, col="black")
rm(res, x)


###################################################
### code chunk number 6: mhw_1a.Rnw:135-145
###################################################
plot(density(mhw$grain), col="darkblue",
     main="Grain yield, lb. per plot", lwd=1.5, ylim=c(0,1),
     xlab=paste("Sample mean:",round(mean(mhw$grain), 3),
                "; s.d:", round(sd(mhw$grain),3)))
grid()
rug(mhw$grain)
curve(dnorm(x, mean(mhw$grain), sd(mhw$grain)), 2.5, 6, add=T,
      col="darkred", lwd=1.5)
text(2.5, 0.85, "Empirical", col="darkblue", pos=4)
text(2.5, 0.8, "Theoretical normal", col="darkred", pos=4)


###################################################
### code chunk number 7: mhw_1a.Rnw:150-151
###################################################
shapiro.test(grain)


###################################################
### code chunk number 8: mhw_1a.Rnw:168-169
###################################################
(t.q13 <- qt(c(.25, .75), length(mhw$grain) - 1))


###################################################
### code chunk number 9: mhw_1a.Rnw:174-175
###################################################
(pe <- mean(mhw$grain) + t.q13 * sd(mhw$grain))


###################################################
### code chunk number 10: mhw_1a.Rnw:180-182
###################################################
rel.error <- (diff(pe)/2) / mean(mhw$grain)
round(100 * rel.error, 2)


###################################################
### code chunk number 11: mhw_1a.Rnw:197-198
###################################################
(yield.ha <- mean(mhw$grain)*500/(0.40469)/(2.2046226))


###################################################
### code chunk number 12: mhw_1a.Rnw:208-209
###################################################
round(yield.ha * rel.error, 2)


###################################################
### code chunk number 13: mhw_1a.Rnw:213-216
###################################################
ans.re <- rel.error
ans.yld <- yield.ha
ans.yld.pe <- round(yield.ha * rel.error, 2)


###################################################
### code chunk number 14: mhw_1a.Rnw:221-222
###################################################
rm(t.q13, pe, rel.error, yield.ha)


###################################################
### code chunk number 15: mhw_1a.Rnw:257-258
###################################################
rm(ans.re, ans.yld, ans.yld.pe)


###################################################
### code chunk number 16: mhw_1a.Rnw:268-277
###################################################
plot(mhw$straw ~ mhw$grain,
     xlab="Grain yield (lb. plot-1)",
     ylab="Straw yield (lb. plot-1)",
     pch=20, col="darkblue", cex=1.2,
     sub="Medians shown with red dashed lines") 
title(main="Straw vs. grain yields, Mercer-Hall experiment")
grid()
abline(v=median(mhw$grain), lty=2, col="red")
abline(h=median(mhw$straw), lty=2, col="red")


###################################################
### code chunk number 17: mhw_1a.Rnw:331-332
###################################################
colMeans(mhw[,c("grain","straw")])


###################################################
### code chunk number 18: mhw_1a.Rnw:337-338
###################################################
var(mhw[,c("grain","straw")])


###################################################
### code chunk number 19: mhw_1a.Rnw:347-352
###################################################
require(MASS)
sim.sample <- mvrnorm(length(mhw$grain), mu=colMeans(mhw[,c("grain","straw")]), Sigma=var(mhw[,c("grain","straw")]))
head(sim.sample)
summary(sim.sample)
summary(mhw[,c("grain", "straw")])


###################################################
### code chunk number 20: mhw_1a.Rnw:358-370
###################################################
par(mfrow=c(2,2))
grain.lim = c(min(sim.sample[,"grain"], mhw$grain), max(sim.sample[,"grain"], mhw$grain)) 
straw.lim = c(min(sim.sample[,"straw"], mhw$straw), max(sim.sample[,"straw"], mhw$straw)) 
hist(mhw$grain, xlim=grain.lim, main="Grain (actual)", col="lightyellow",
     breaks=seq(grain.lim[1],grain.lim[2], length=17))
hist(sim.sample[,"grain"],xlim=grain.lim, main="Grain (simulated)", col="cornsilk",
     breaks=seq(grain.lim[1],grain.lim[2], length=17))
hist(mhw$straw, xlim=straw.lim, main="Straw (actual)", col="lightblue",
     breaks=seq(straw.lim[1],straw.lim[2], length=17))
hist(sim.sample[,"straw"],xlim=straw.lim, main="Straw (simulated)", col="lavender",
     breaks=seq(straw.lim[1],straw.lim[2], length=17))
par(mfrow=c(1,1))


###################################################
### code chunk number 21: mhw_1a.Rnw:381-389
###################################################
par(mfrow=c(1,2))
plot(sim.sample, main="Simulated straw vs. grain yields", xlab="Grain (lb. plot-1)", ylab="Straw (lb. plot-1)", xlim=grain.lim, ylim=straw.lim, pch=20, col="blue")  
abline(v=median(sim.sample[,1]), lty=2, col="red")
abline(h=median(sim.sample[,2]), lty=2, col="red")
plot(mhw$grain, mhw$straw, main="Actual straw vs. grain yields", xlab="Grain (lb. plot-1)", ylab="Straw (lb. plot-1)", xlim=grain.lim, ylim=straw.lim, pch=20, col="black")  
abline(v=median(mhw$grain), lty=2, col="red")
abline(h=median(mhw$straw), lty=2, col="red")
par(mfrow=c(1,1))


###################################################
### code chunk number 22: mhw_1a.Rnw:398-399
###################################################
rm(sim.sample, grain.lim, straw.lim)


###################################################
### code chunk number 23: mhw_1a.Rnw:414-416
###################################################
cor(mhw$grain, mhw$straw)
cor.test(mhw$grain, mhw$straw)


###################################################
### code chunk number 24: mhw_1a.Rnw:572-575
###################################################
model.straw.grain <- lm(straw ~ grain)
summary(model.straw.grain)
coefficients(model.straw.grain)


###################################################
### code chunk number 25: mhw_1a.Rnw:601-607
###################################################
plot(straw ~ grain)
title("Straw yield predicted by grain yield")
abline(model.straw.grain)
grid()
text(4.5, 4.5, paste("slope:",
                     round(coefficients(model.straw.grain)[2], 2)))


###################################################
### code chunk number 26: mhw_1a.Rnw:627-629
###################################################
hist(residuals(model.straw.grain),
     main="Residuals from straw vs.\ grain linear model")


###################################################
### code chunk number 27: mhw_1a.Rnw:632-636
###################################################
qqnorm(residuals(model.straw.grain),
       main="Residuals from straw vs.\ grain linear model")
qqline(residuals(model.straw.grain))
summary(residuals(model.straw.grain))


###################################################
### code chunk number 28: mhw_1a.Rnw:645-646
###################################################
shapiro.test(residuals(model.straw.grain))


###################################################
### code chunk number 29: mhw_1a.Rnw:659-675
###################################################
lim <- c(min(fitted(model.straw.grain), mhw$straw),
         max(fitted(model.straw.grain), mhw$straw))
plot(fitted(model.straw.grain), mhw$straw,
     xlab="Modelled", ylab="Actual", asp=1,
     xlim=lim, ylim=lim, pch=20,
     col=ifelse(
       (abs(fitted(model.straw.grain) - mhw$straw) < 1),
       "gray",
       ifelse(fitted(model.straw.grain) < mhw$straw, "blue","red")),
     cex=ifelse(
       (abs(fitted(model.straw.grain) - mhw$straw) < 1),1,1.3)
)
title("Actual vs. modelled straw yields")
grid()
abline(0,1)
rm(lim)


###################################################
### code chunk number 30: mhw_1a.Rnw:690-692
###################################################
which.high.res <- which(abs(residuals(model.straw.grain)) > 1.8)
sort(residuals(model.straw.grain)[which.high.res])


###################################################
### code chunk number 31: mhw_1a.Rnw:697-700
###################################################
high.res <- mhw[which.high.res,]
high.res[order(high.res$gsr),]
rm(which.high.res, high.res)


###################################################
### code chunk number 32: mhw_1a.Rnw:723-724
###################################################
plot(model.straw.grain, which=1)


###################################################
### code chunk number 33: mhw_1a.Rnw:739-740
###################################################
plot(model.straw.grain, which=5)


###################################################
### code chunk number 34: mhw_1a.Rnw:747-748
###################################################
plot(model.straw.grain, which=6, id.n=10)


###################################################
### code chunk number 35: mhw_1a.Rnw:800-806
###################################################
t1 <- mean(mhw$grain); t2 <- sd(mhw$grain);
p.frame <- data.frame(grain=seq(t1-2*t2, t1+2*t2, t2))
predict(model.straw.grain, newdata=p.frame,
        interval="confidence", level=0.95)
predict(model.straw.grain, newdata=p.frame,
        interval="prediction", level=0.95)


###################################################
### code chunk number 36: mhw_1a.Rnw:821-834
###################################################
p.frame <- data.frame(grain=seq(from=2,to=6,by=0.1))
pred.c <- predict(model.straw.grain, newdata=p.frame, interval="confidence", level=0.95)
pred.p <- predict(model.straw.grain, newdata=p.frame, interval="prediction", level=0.95)
plot(straw ~ grain, data=mhw, pch=20)
title(main="Straw yield predicted by grain yield", sub="Prediction (blue) and confidence (red) intervals")
abline(model.straw.grain)
grid()
lines(p.frame$grain, pred.c[, "lwr"], col = 2, lwd = 1.5)
lines(p.frame$grain, pred.c[, "upr"], col = 2, lwd = 1.5)
lines(p.frame$grain, pred.p[, "lwr"], col = 4, lwd = 1.5)
lines(p.frame$grain, pred.p[, "upr"], col = 4, lwd = 1.5)
points(mean(grain),mean(straw),pch=23,cex=2, bg="red")
abline(h=mean(straw), lty=2); abline(v=mean(grain), lty=2)


###################################################
### code chunk number 37: mhw_1a.Rnw:847-848
###################################################
rm(t1,t2,p.frame,pred.c,pred.p)


###################################################
### code chunk number 38: mhw_1a.Rnw:860-863
###################################################
model.grain.straw <- lm(grain ~ straw, data=mhw)
summary(model.grain.straw)
summary(model.straw.grain)


###################################################
### code chunk number 39: mhw_1a.Rnw:876-878
###################################################
coefficients(model.straw.grain)["grain"]
1/coefficients(model.grain.straw)["straw"]


###################################################
### code chunk number 40: mhw_1a.Rnw:893-900
###################################################
plot(straw ~ grain, pch=1, main="Mercer-Hall wheat yields", xlab="grain (lb. plot-1)", ylab="straw (lb. plot-1)")
title(sub="straw vs. grain: solid; grain vs. straw: dashed")
abline(model.straw.grain)
beta <- coefficients(model.grain.straw)
abline(-beta["(Intercept)"]/beta["straw"]  , 1/beta["straw"], lty=2)
grid()
rm(beta)


###################################################
### code chunk number 41: mhw_1a.Rnw:964-965
###################################################
var(straw)/var(grain)


###################################################
### code chunk number 42: mhw_1a.Rnw:992-997
###################################################
struct.beta <- function(y, x, lambda) {
  a <- var(y)-lambda*var(x);
  c <- var(x,y);
  return((a + sqrt(a^2 + 4 * lambda * c^2))/(2*c))
}


###################################################
### code chunk number 43: mhw_1a.Rnw:1011-1017
###################################################
print(paste("Forward:", round(coefficients(model.straw.grain)["grain"],4)))
print(paste("Proportional:", round(struct.beta(straw,grain,var(straw)/var(grain)),4)))
print(paste("Inverse proportional:", round(1/struct.beta(grain,straw,var(grain)/var(straw)),4)))
print(paste("Orthogonal:", round(struct.beta(straw,grain,1),4)))
print(paste("Inverse orthogonal:", round(1/struct.beta(grain,straw,1),4)))
print(paste("Reverse:", round(1/coefficients(model.grain.straw)["straw"],4)))


###################################################
### code chunk number 44: mhw_1a.Rnw:1033-1061
###################################################
plot(straw ~ grain, main="Mercer-Hall wheat yields",
     sub="Regression slopes", xlab="grain (lb. plot-1)",
     ylab="straw (lb. plot-1)")
abline(model.straw.grain, col="blue")
beta <- coefficients(model.grain.straw)
abline(-beta["(Intercept)"]/beta["straw"]  , 1/beta["straw"],
       lty=2, col="green")
beta <- struct.beta(straw,grain,1)
abline(mean(straw)-beta*mean(grain), beta, lty=3, col="red")
beta <- struct.beta(straw,grain,var(straw)/var(grain))
abline(mean(straw)-beta*mean(grain), beta, lty=4, col="brown")
lines(c(4,4.5),c(5,5), lty=1, col="blue") 
lines(c(4,4.5),c(4.4,4.4), lty=4, col="brown") 
lines(c(4,4.5),c(4.6,4.6), lty=3, col="red") 
lines(c(4,4.5),c(4.8,4.8), lty=2, col="green") 
grid()
text(4.5,5,paste("Forward:",
                 round(coefficients(model.straw.grain)["grain"],4)),
     col="blue", pos=4)
text(4.5,4.4,paste("Proportional:",
                   round(struct.beta(straw,grain,var(straw)/var(grain)),4)),
     col="brown", pos=4)
text(4.5,4.6, paste("Orthogonal:",
                    round(struct.beta(straw,grain,1),4)),
     col="red", pos=4)
text(4.5,4.8,paste("Reverse:",
                   round(1/coefficients(model.grain.straw)["straw"],4)),
     col="green", pos=4)


###################################################
### code chunk number 45: mhw_1a.Rnw:1130-1132
###################################################
model.straw.grain.0 <- lm(straw ~ grain - 1)
summary(model.straw.grain.0)


###################################################
### code chunk number 46: mhw_1a.Rnw:1145-1166
###################################################
plot(straw ~ grain, main="Mercer-Hall wheat yields",
     xlab="grain (lb. plot-1)", ylab="straw (lb. plot-1)",
     xlim=c(0,ceiling(max(grain))),
     ylim=c(0, ceiling(max(straw))), cex=0.8)
abline(model.straw.grain, col="blue")
abline(model.straw.grain.0, col="red")
grid()
text(4.5,4, paste("    With:",
                  round(coefficients(model.straw.grain)["grain"],2)),
     col="blue", pos=4)
text(4.5,3.4,paste("Without:",
                   round(coefficients(model.straw.grain.0)["grain"],2)),
     col="red", pos=4)
abline(v=mean(grain), col="darkgray", lty=2)
abline(h=mean(straw), col="darkgray", lty=2)
points(mean(grain), mean(straw), cex=2, pch=23, bg="red")
abline(h=coefficients(model.straw.grain)["(Intercept)"],
       col="darkgray", lty=2)
text(1,1,paste("Intercept:",
               round(coefficients(model.straw.grain)["(Intercept)"],2)),
     col="blue", pos=4)


###################################################
### code chunk number 47: mhw_1a.Rnw:1174-1176
###################################################
mean(residuals(model.straw.grain.0))
mean(residuals(model.straw.grain))


###################################################
### code chunk number 48: mhw_1a.Rnw:1203-1210
###################################################
(TSS <- sum((straw-mean(straw))^2))
(TSS0 <- sum(straw^2))
(RSS <- sum(residuals(model.straw.grain)^2))
(RSS0 <- sum(residuals(model.straw.grain.0)^2))
(R2 <- 1 - (RSS/TSS))     
(R20 <- 1 - (RSS0/TSS0))  
rm(TSS, TSS0, RSS, RSS0, R2, R20)


###################################################
### code chunk number 49: mhw_1a.Rnw:1218-1220
###################################################
summary(model.straw.grain.0)$adj.r.squared
summary(model.straw.grain)$adj.r.squared


###################################################
### code chunk number 50: mhw_1a.Rnw:1232-1234
###################################################
sqrt(sum(residuals(model.straw.grain)^2)/(length(straw)))
sqrt(sum(residuals(model.straw.grain.0)^2)/(length(straw)))


###################################################
### code chunk number 51: mhw_1a.Rnw:1386-1388 (eval = FALSE)
###################################################
## mhw <- cbind(mhw, in.north = (r < 11))
## str(mhw)


###################################################
### code chunk number 52: mhw_1a.Rnw:1392-1394
###################################################
if (!exists("in.north", where="mhw")) { mhw <- cbind(mhw, in.north = (r < 11)) }
str(mhw)


###################################################
### code chunk number 53: mhw_1a.Rnw:1400-1402
###################################################
detach(mhw)
attach(mhw)


###################################################
### code chunk number 54: mhw_1a.Rnw:1405-1406
###################################################
summary(in.north)


###################################################
### code chunk number 55: mhw_1a.Rnw:1415-1422
###################################################
# plot halves in appropriate colours
plot(c, r, col=ifelse(in.north,"blue","darkslategrey"), 
     cex=1.3*straw/max(straw), pch=1,
     xlab="Column", ylab="Row", ylim=c(20,1),
     sub="North: blue; South: gray");
title(main="Postplot of straw yields, coded by field half");
abline(h=10.5, lty=2)


###################################################
### code chunk number 56: mhw_1a.Rnw:1446-1454
###################################################
par(mfrow=c(3,1))
boxplot(grain ~ in.north, names=c("S", "N"),
        main="Grain yield", horizontal=T)
boxplot(straw ~ in.north, names=c("S", "N"),
        main="Straw yield", horizontal=T)
boxplot(gsr ~ in.north, names=c("S", "N"),
        main="Grain/straw ratio", horizontal=T)
par(mfrow=c(1,1))


###################################################
### code chunk number 57: mhw_1a.Rnw:1469-1476
###################################################
by(grain, in.north, summary)
by(straw, in.north, summary)
by(gsr, in.north, summary)
# compare statistics
by(grain, in.north, var)
by(straw, in.north, var)
by(gsr, in.north, var)


###################################################
### code chunk number 58: mhw_1a.Rnw:1495-1497
###################################################
# compare the means with a t-test
t.test(straw[in.north], straw[!in.north])


###################################################
### code chunk number 59: mhw_1a.Rnw:1515-1517
###################################################
model.straw.ns <- lm(straw ~ in.north, data=mhw)
summary(model.straw.ns)


###################################################
### code chunk number 60: mhw_1a.Rnw:1526-1527
###################################################
anova(model.straw.ns)


###################################################
### code chunk number 61: mhw_1a.Rnw:1532-1536
###################################################
qqnorm(residuals(model.straw.ns),
       main="Residuals from one-way ANOVA",
       sub="Straw yield vs. field half")
qqline(residuals(model.straw.ns))


###################################################
### code chunk number 62: mhw_1a.Rnw:1596-1598
###################################################
quantile(mhw$grain, p=0.01, type=5)
mean(sort(mhw$grain)[5:6])


###################################################
### code chunk number 63: mhw_1a.Rnw:1626-1627
###################################################
quantile(mhw$grain, p=0.01, type=5)


###################################################
### code chunk number 64: mhw_1a.Rnw:1638-1642
###################################################
boot.q01 <- function(data, indices){
  obs <- data[indices,]
  return(quantile(obs$grain, p=0.01, type=5))
}


###################################################
### code chunk number 65: mhw_1a.Rnw:1655-1658
###################################################
require(boot)
b.q01 <- boot(mhw, boot.q01, R=1024)
print(b.q01)


###################################################
### code chunk number 66: mhw_1a.Rnw:1675-1676
###################################################
plot(b.q01)


###################################################
### code chunk number 67: mhw_1a.Rnw:1718-1720
###################################################
mean(b.q01$t)
b.q01$t0


###################################################
### code chunk number 68: mhw_1a.Rnw:1729-1730
###################################################
(b.q01.ci <- boot.ci(b.q01, conf = 0.95, type=c("norm","basic")))


###################################################
### code chunk number 69: mhw_1a.Rnw:1749-1754 (eval = FALSE)
###################################################
## struct.beta <- function(y, x, lambda) {
##   a <- var(y)-lambda*var(x);
##   c <- var(x,y);
##   return((a + sqrt(a^2 + 4 * lambda * c^2))/(2*c))
## }


###################################################
### code chunk number 70: mhw_1a.Rnw:1759-1761 (eval = FALSE)
###################################################
## beta <- struct.beta(straw,grain,var(straw)/var(grain))
## alpha <- mean(straw)-beta*mean(grain)


###################################################
### code chunk number 71: mhw_1a.Rnw:1770-1777
###################################################
boot.sr <- function (data, indices) {
  obs <- data[indices,]
  beta <- struct.beta(obs$straw,obs$grain,
                      var(obs$straw)/var(obs$grain))
  alpha <- mean(obs$straw)-beta*mean(obs$grain)
  return(c(beta, alpha))
}


###################################################
### code chunk number 72: mhw_1a.Rnw:1782-1784
###################################################
b.sr <- boot(mhw, boot.sr, R=1024)
print(b.sr)


###################################################
### code chunk number 73: mhw_1a.Rnw:1790-1791
###################################################
plot(b.sr, index=1)


###################################################
### code chunk number 74: mhw_1a.Rnw:1798-1799
###################################################
plot(b.sr, index=2)


###################################################
### code chunk number 75: mhw_1a.Rnw:1813-1817
###################################################
mean(b.sr$t[,1])
(b.sr.ci <- boot.ci(b.sr, conf = 0.95, type=c("norm","basic"), index=1))
mean(b.sr$t[,2])
(b.sr.ci <- boot.ci(b.sr, conf = 0.95, type=c("norm","basic"), index=2))


###################################################
### code chunk number 76: mhw_1a.Rnw:1829-1832
###################################################
b.corr <- function(data,indices) { 	obs <- data[indices,]; return(cor(obs$grain, obs$straw)) }
boot.corr <- boot(mhw, b.corr, R=1024)
boot.corr.ci <- boot.ci(boot.corr, conf=0.95, type="basic")


###################################################
### code chunk number 77: mhw_1a.Rnw:1838-1839
###################################################
plot(boot.corr)


###################################################
### code chunk number 78: mhw_1a.Rnw:1892-1895
###################################################
rm(model.grain.straw)
rm(boot.q01, b.q01, b.q01.ci, boot.sr, b.sr, b.sr.ci)
detach(package:boot)


###################################################
### code chunk number 79: mhw_1a.Rnw:1898-1899
###################################################
rm(boot.corr, b.corr, boot.corr.ci)

### R code from vignette source 'mhw_1ar'

###################################################
### code chunk number 1: mhw_1ar.Rnw:63-64
###################################################
mhw.c <- mhw


###################################################
### code chunk number 2: mhw_1ar.Rnw:69-72
###################################################
ix <- (mhw.c$r < 5) & (mhw.c$c <5)
rownames(mhw.c[ix,])
subset(mhw.c, ix)


###################################################
### code chunk number 3: mhw_1ar.Rnw:79-80
###################################################
mhw.c[ix, "grain"] <- mhw.c[ix, "grain"]/4


###################################################
### code chunk number 4: mhw_1ar.Rnw:89-91
###################################################
summary(mhw$grain); sd(mhw$grain)
summary(mhw.c$grain); sd(mhw.c$grain)


###################################################
### code chunk number 5: mhw_1ar.Rnw:97-105
###################################################
par(mfrow=c(1,2))
hist(mhw$grain, xlab="Grain yield, lbs / plot",
     main="Actual", breaks=seq(0,6, by=.25))
rug(mhw$grain)
hist(mhw.c$grain, xlab="Grain yield, lbs / plot",
     main="Contaminated", breaks=seq(0,6, by=.25))
rug(mhw.c$grain)
par(mfrow=c(1,1))


###################################################
### code chunk number 6: mhw_1ar.Rnw:119-127
###################################################
mean(mhw$grain); mean(mhw.c$grain)
(mean(mhw.c$grain) - mean(mhw$grain))/mean(mhw$grain)*100
sd(mhw$grain); sd(mhw.c$grain)
(sd(mhw.c$grain) - sd(mhw$grain))/sd(mhw$grain)*100
median(mhw$grain); median(mhw.c$grain)
(median(mhw.c$grain) - median(mhw$grain))/median(mhw$grain)*100
IQR(mhw$grain); IQR(mhw.c$grain)
(IQR(mhw.c$grain) - IQR(mhw$grain))/IQR(mhw$grain)*100


###################################################
### code chunk number 7: mhw_1ar.Rnw:147-148
###################################################
head(cbind(mhw[, c("grain", "straw")],rank(mhw$grain),rank(mhw$straw)), n=8)


###################################################
### code chunk number 8: mhw_1ar.Rnw:161-167
###################################################
par(mfrow=c(2,2))
plot(rank(mhw$grain), rank(mhw$straw), xlab="Grain rank", ylab="Straw rank", pch=1, main="Original")  
plot(mhw$grain, mhw$straw, xlab="Grain (lbs / plot)", ylab="Straw (lbs / plot)", pch=20, main="Original")  
plot(rank(mhw.c$grain), rank(mhw.c$straw), xlab="Grain rank", ylab="Straw rank", pch=1, main="Contaminated")  
plot(mhw.c$grain, mhw.c$straw, xlab="Grain (lbs / plot)", ylab="Straw (lbs / plot)", pch=20, main="Contaminated")  
par(mfrow=c(1,1))


###################################################
### code chunk number 9: mhw_1ar.Rnw:186-190
###################################################
(c.p <- cor(mhw$grain, mhw.c$straw, method="pearson"))
(cc.p <- cor(mhw.c$grain, mhw.c$straw, method="pearson"))
(c.s <- cor(mhw$grain, mhw.c$straw, method="spearman"))
(cc.s <- cor(mhw.c$grain, mhw.c$straw, method="spearman"))


###################################################
### code chunk number 10: mhw_1ar.Rnw:205-207
###################################################
print(model.straw.grain)
(model.straw.grain.c <- lm(straw ~ grain, data=mhw.c))


###################################################
### code chunk number 11: mhw_1ar.Rnw:219-222
###################################################
par(mfrow=c(2,2))
plot(model.straw.grain.c)
par(mfrow=c(1,1))


###################################################
### code chunk number 12: mhw_1ar.Rnw:236-241
###################################################
with(mhw.c, plot(straw ~ grain))
abline(model.straw.grain.c)
abline(model.straw.grain, lty=2, col="blue")
legend(1.5, 8.5, legend=c("fit", "fit to uncontaminated"),
       lty=1:2, col=c("black","blue"))


###################################################
### code chunk number 13: mhw_1ar.Rnw:272-275
###################################################
require(MASS)
(model.straw.grain.c.r <- lqs(straw ~ grain, data=mhw.c))
sqrt(mean(residuals(model.straw.grain)^2))


###################################################
### code chunk number 14: mhw_1ar.Rnw:284-289
###################################################
with(mhw.c, plot(straw ~ grain))
abline(model.straw.grain.c.r)
abline(model.straw.grain.c, lty=3, col="red")
abline(model.straw.grain, lty=2, col="blue")
legend(1.5, 8.5, legend=c("robust fit", "linear fit", "fit to uncontaminated"), lty=c(1,3,2), col=c("black","red","blue"))


### R code from vignette source 'mhw_1b'

###################################################
### code chunk number 1: mhw_1b.Rnw:57-58
###################################################
summary(model.straw.ns)


###################################################
### code chunk number 2: mhw_1b.Rnw:67-68
###################################################
summary(model.straw.grain)


###################################################
### code chunk number 3: mhw_1b.Rnw:91-93
###################################################
model.straw.ns.grain <- lm(straw ~ in.north + grain, data=mhw)
summary(model.straw.ns.grain)


###################################################
### code chunk number 4: mhw_1b.Rnw:106-118
###################################################
# scatterplot, coloured by zone
plot(straw ~ grain, col=ifelse(in.north,"blue","slategray"), pch=20, xlab="grain (lbs plot-1)", ylab="straw (lbs plot-1)")
title(main="Straw vs. grain yield")
title(sub="N half: blue, S half: grey; whole-field line: red")                  # S
abline(coefficients(model.straw.ns.grain)["(Intercept)"] , coefficients(model.straw.ns.grain)["grain"], col="slategray")
# N
abline(coefficients(model.straw.ns.grain)["(Intercept)"] 
       +  coefficients(model.straw.ns.grain)["in.northTRUE"]
       , coefficients(model.straw.ns.grain)["grain"], col="blue")
# univariate line
abline(model.straw.grain, lty=2, col="red")
grid()


###################################################
### code chunk number 5: mhw_1b.Rnw:140-143
###################################################
summary(model.straw.ns)$adj.r.squared
summary(model.straw.grain)$adj.r.squared
summary(model.straw.ns.grain)$adj.r.squared


###################################################
### code chunk number 6: mhw_1b.Rnw:159-161
###################################################
anova(model.straw.ns.grain, model.straw.ns)
anova(model.straw.ns.grain, model.straw.grain)


###################################################
### code chunk number 7: mhw_1b.Rnw:186-188
###################################################
model.straw.ns.grain.i <- lm(straw ~ in.north * grain, data=mhw)
summary(model.straw.ns.grain.i)


###################################################
### code chunk number 8: mhw_1b.Rnw:207-217
###################################################
plot(straw ~ grain, col=ifelse(in.north,"blue","slategray"), pch=20, xlab="grain (lbs plot-1)", ylab="straw (lbs plot-1)")
title(main="Straw vs. grain, by field half")
title(sub="Interaction: solid lines; additive: dashed lines")
abline(lm(straw ~ grain, subset=in.north), col="blue")
abline(lm(straw ~ grain, subset=!in.north), col="slategray")
abline(coefficients(model.straw.ns.grain)["(Intercept)"] , coefficients(model.straw.ns.grain)["grain"], col="slategray", lty=2)
# N
abline(coefficients(model.straw.ns.grain)["(Intercept)"] 
       +  coefficients(model.straw.ns.grain)["in.northTRUE"]
       , coefficients(model.straw.ns.grain)["grain"], col="blue", lty=2)


###################################################
### code chunk number 9: mhw_1b.Rnw:226-229
###################################################
summary(model.straw.ns.grain)$adj.r.squared
summary(model.straw.ns.grain.i)$adj.r.squared
anova(model.straw.ns.grain.i, model.straw.ns.grain)


###################################################
### code chunk number 10: mhw_1b.Rnw:248-251
###################################################
par(mfrow=c(2,2))
plot.lm(model.straw.ns.grain, which=c(1,2,5,6), id.n=10)
par(mfrow=c(1,1))


###################################################
### code chunk number 11: mhw_1b.Rnw:263-265
###################################################
(selected <- which(abs(rstandard(model.straw.ns.grain)) > 3))
rstandard(model.straw.ns.grain)[selected]


###################################################
### code chunk number 12: mhw_1b.Rnw:270-274
###################################################
mhw.hires <- cbind(mhw[selected,], 
                   sres = rstandard(model.straw.ns.grain)[selected])
rm(selected)
str(mhw.hires)


###################################################
### code chunk number 13: mhw_1b.Rnw:279-280
###################################################
mhw.hires[order(mhw.hires$sres),]


###################################################
### code chunk number 14: mhw_1b.Rnw:285-289
###################################################
plot(c, r, ylim=c(20,1), cex=3*gsr/max(gsr), pch=20, col=ifelse(rstandard(model.straw.ns.grain) > 3, "brown", ifelse(rstandard(model.straw.ns.grain) < (-3), "red", ifelse(in.north, "lightblue", "gray"))), xlab="column", ylab="row")
abline(h=10.5)
title(main="Large residuals, straw yield vs.\ field half and grain yield")
title(sub="Positive: brown; negative: red")


###################################################
### code chunk number 15: mhw_1b.Rnw:314-317
###################################################
model.straw.ns.grain.adj <- lm(straw ~ in.north + grain, data=mhw[-c(15,35),])
summary(model.straw.ns.grain.adj)
summary(model.straw.ns.grain)


###################################################
### code chunk number 16: mhw_1b.Rnw:341-347
###################################################
model.straw.ns.grain.nest <- lm(straw ~ in.north / grain, data=mhw)
summary(model.straw.ns.grain.nest)
plot(straw ~ grain, data=mhw, col=ifelse(mhw$in.north, "blue", "slategray"), pch=20, xlim=c(2.5,5.5), ylim=c(4,9.5))
coef <- coef(model.straw.ns.grain.nest)
abline(coef[1], coef[3], col="slategray")
abline(coef[1]+coef[2], coef[4], col="blue")


###################################################
### code chunk number 17: mhw_1b.Rnw:504-506
###################################################
rm(model.straw.ns, model.straw.grain, model.straw.ns.grain, model.straw.ns.grain.adj, model.straw.ns.grain.i, model.straw.ns.grain.nest)
rm(struct.beta, beta, mhw.hires)

### R code from vignette source 'mhw_1bpc'

###################################################
### code chunk number 1: mhw_1bpc.Rnw:44-47
###################################################
pc <- prcomp(mhw[,c("grain","straw")], scale=T)
# pc <- prcomp(mhw[,c("grain","straw")], scale=F)
summary(pc)


###################################################
### code chunk number 2: mhw_1bpc.Rnw:66-67
###################################################
pc$rotation


###################################################
### code chunk number 3: mhw_1bpc.Rnw:78-80
###################################################
biplot(pc)
grid()


###################################################
### code chunk number 4: mhw_1bpc.Rnw:103-106
###################################################
str(pc$x)
mhw[which.max(pc$x[,"PC1"]),]
mhw[which.min(pc$x[,"PC1"]),]


###################################################
### code chunk number 5: mhw_1bpc.Rnw:123-125
###################################################
mhw[which.max(pc$x[,"PC2"]),]
mhw[which.min(pc$x[,"PC2"]),]

### R code from vignette source 'mhw_1c'

###################################################
### code chunk number 1: mhw_1c.Rnw:65-66
###################################################
head(row.names(mhw), n=10)


###################################################
### code chunk number 2: mhw_1c.Rnw:71-72
###################################################
head(mhw, n=10)


###################################################
### code chunk number 3: mhw_1c.Rnw:101-106
###################################################
dim(mhw)
(n <- dim(mhw)[1])
set.seed(123)
head(index.calib <- sort(sample(1:n, size=floor(n*3/4), replace=F)), n=12)
length(index.calib)


###################################################
### code chunk number 4: mhw_1c.Rnw:115-117
###################################################
head(index.valid <- setdiff(1:n, index.calib), n=12)
length(index.valid)


###################################################
### code chunk number 5: mhw_1c.Rnw:122-123
###################################################
setequal(union(index.calib, index.valid), 1:n)


###################################################
### code chunk number 6: mhw_1c.Rnw:135-137
###################################################
cal.straw.grain <- lm(straw ~ grain, data=mhw, subset=index.calib)
summary(cal.straw.grain)


###################################################
### code chunk number 7: mhw_1c.Rnw:146-150
###################################################
model.straw.grain <- lm(straw ~ grain, data=mhw)
(coef(cal.straw.grain) - coef(model.straw.grain))
((coef(cal.straw.grain) - coef(model.straw.grain))
 /coef(model.straw.grain))*100


###################################################
### code chunk number 8: mhw_1c.Rnw:164-165
###################################################
pred <- predict.lm(cal.straw.grain, newdata=mhw[index.valid,])


###################################################
### code chunk number 9: mhw_1c.Rnw:176-177
###################################################
actual <- mhw[index.valid, "straw"]


###################################################
### code chunk number 10: mhw_1c.Rnw:183-190
###################################################
summary(pred); summary(actual)
par(mfrow=c(1,2))
hist(pred, main="", xlab="Predicted straw yields, lb / plot",
     breaks=seq(4,9.2,by=0.4), freq=F, ylim=c(0,.8))
hist(actual, main="", xlab="Actual straw yields, lb / plot",
     breaks=seq(4,9.2,by=0.4), freq=F, ylim=c(0,.8))
par(mfrow=c(1,1))


###################################################
### code chunk number 11: mhw_1c.Rnw:284-288
###################################################
plot(actual ~ pred, ylab="Actual", xlab="Predicted", asp=1,
     main="Mercer-Hall trial, straw yield, lbs/plot",
     xlim=c(4.5,9), ylim=c(4.5,9));
abline(0,1); grid()


###################################################
### code chunk number 12: mhw_1c.Rnw:301-304
###################################################
(valid.msd <- sum((actual - pred)^2)/length(index.valid))
(valid.msd <- mean((actual - pred)^2))
(valid.rmsep <- sqrt(valid.msd))


###################################################
### code chunk number 13: mhw_1c.Rnw:314-315
###################################################
(rmse <- sqrt(mean(residuals(cal.straw.grain)^2)))


###################################################
### code chunk number 14: mhw_1c.Rnw:324-327
###################################################
(valid.bias <- (mean(pred) - mean(actual)))
(valid.sb <- valid.bias^2)
valid.sb/valid.msd*100


###################################################
### code chunk number 15: mhw_1c.Rnw:347-357
###################################################
regr.actual.pred <- lm(actual ~ pred)
summary(regr.actual.pred)
plot(actual ~ pred, ylab="Actual", xlab="Predicted", asp=1,
     main="Mercer-Hall trial, straw yield, lbs/plot",
     xlim=c(4.5,9), ylim=c(4.5,9));
abline(regr.actual.pred, col="red")
abline(0,1); grid()
text(4.5, 8.5, paste("Gain:", round(coef(regr.actual.pred)[2], 2)),
     pos=4, col="red")
legend(7.5, 5, c("1:1","regression"), lty=1, col=c("black","red"))


###################################################
### code chunk number 16: mhw_1c.Rnw:367-377
###################################################
regr.actual.pred.0 <- lm(I(actual - pred) ~ pred)
summary(regr.actual.pred.0)
plot(I(actual - pred) ~ pred, ylab="Actual - Predicted",
     xlab="Predicted",
     main="Mercer-Hall trial, straw yield, lbs/plot")
grid(); abline(regr.actual.pred.0, col="red"); abline(h=0)
text(5, 1.6, paste("Slope:",
                   round(coef(regr.actual.pred.0)[2], 2)),
     pos=4, col="red")
legend(5, -1, c("1:1","regression"), lty=1, col=c("black","red"))


###################################################
### code chunk number 17: mhw_1c.Rnw:384-385
###################################################
coef(regr.actual.pred)[2] - coef(regr.actual.pred.0)[2]


###################################################
### code chunk number 18: mhw_1c.Rnw:398-399
###################################################
b <- coef(regr.actual.pred)[2]; names(b) <- NULL; print(b)


###################################################
### code chunk number 19: mhw_1c.Rnw:407-408
###################################################
(valid.msd.pred <- mean((pred - mean(pred))^2))


###################################################
### code chunk number 20: mhw_1c.Rnw:413-415
###################################################
(valid.nu <- (1 - b)^2 * valid.msd.pred)
valid.nu/valid.msd*100


###################################################
### code chunk number 21: mhw_1c.Rnw:432-433
###################################################
(valid.msd.actual <- mean((actual - mean(actual))^2))


###################################################
### code chunk number 22: mhw_1c.Rnw:438-442
###################################################
(r2 <- summary(regr.actual.pred)$r.squared)
(r2 <- cor(actual, pred)^2)
(valid.lc <- (1 - r2) * valid.msd.actual)
valid.lc/valid.msd * 100


###################################################
### code chunk number 23: mhw_1c.Rnw:451-452
###################################################
print(valid.msd - (valid.sb + valid.nu + valid.lc))


###################################################
### code chunk number 24: mhw_1c.Rnw:496-498
###################################################
cal.straw.grain.00 <- lm(straw ~ grain - 1, data=mhw, subset=index.calib)
summary(cal.straw.grain.00)


###################################################
### code chunk number 25: mhw_1c.Rnw:508-516
###################################################
plot(straw ~ grain, data=mhw, subset=index.calib, xlim=c(0,6), ylim=c(0,9))
title("Mercer-Hall trial, calibration dataset")
abline(cal.straw.grain, lty=2)
abline(cal.straw.grain.00, col="red")
grid()
legend(4, 1, c("with intercept","no intercept"), lty=c(2,1), col=c("black","red"))
text(0, 2.5, paste("Slope:",round(coef(cal.straw.grain)[2],2)), pos=4)
text(1, 0.5, paste("Slope:",round(coef(cal.straw.grain.00)[1],2)), col="red")


###################################################
### code chunk number 26: mhw_1c.Rnw:528-530
###################################################
pred <- predict.lm(cal.straw.grain.00, newdata=mhw[index.valid,])
summary(pred)


###################################################
### code chunk number 27: mhw_1c.Rnw:538-549
###################################################
regr.actual.pred.00 <- lm(actual ~ pred)
plot(actual ~ pred, ylab="Actual", xlab="Predicted", asp=1,
     main="Mercer-Hall trial, straw yield, lbs/plot",
     xlim=c(4.5,9), ylim=c(4.5,9));
abline(regr.actual.pred.00, col="red")
abline(0,1); grid()
text(4.5, 8.5, paste("Gain:",
                     round(coef(regr.actual.pred.00)[2], 2)),
     pos=4, col="red")
legend(7.5, 5, c("1:1","regression"), lty=1,
       col=c("black","red"))


###################################################
### code chunk number 28: mhw_1c.Rnw:556-561
###################################################
(msd.00 <- mean((actual - pred)^2))
(rmsep.00 <- sqrt(msd.00))
(sb.00 <- (mean(pred) - mean(actual))^2)
(nu.00 <- (1 - coef(regr.actual.pred.00)[2])^2 * mean((pred - mean(pred))^2))
(lc.00 <- (1 - cor(actual, pred)^2) * mean((actual - mean(actual))^2))


###################################################
### code chunk number 29: mhw_1c.Rnw:568-572
###################################################
sb.00/msd.00*100
nu.00/msd.00*100
lc.00/msd.00*100
msd.00 - (sb.00 + nu.00 + lc.00)


###################################################
### code chunk number 30: mhw_1c.Rnw:664-669
###################################################
rm(n, index.valid, index.calib, actual)
rm(cal.straw.grain, pred)
rm(valid.msd, rmse)
rm(regr.actual.pred, regr.actual.pred.0, valid.bias, valid.sb, valid.lc, b, valid.nu, valid.msd.pred, valid.msd.actual, r2)
rm(cal.straw.grain.00, regr.actual.pred.00, msd.00, rmsep.00, sb.00, nu.00, lc.00)

### R code from vignette source 'mhw_1d'

###################################################
### code chunk number 1: mhw_1d.Rnw:78-89
###################################################
Xval <- function(model, dset) {
  pred <- rep(0, nrow(dset))
  n <- length(coefficients(model))
  coef <- matrix(0, nrow=nrow(dset), ncol=n)
  colnames(coef) <- paste("b", as.character(0:(n-1)) , sep="")
  for (i in 1:nrow(dset)) {
    m <- lm(formula(model), data=dset[-i,]);
    pred[i] <- predict(m, newdata=dset[i,])
    coef[i,] <- coefficients(m)
  }
  return(list(pred=pred, coef=coef))}


###################################################
### code chunk number 2: mhw_1d.Rnw:96-98
###################################################
xval.fit <- Xval(model.straw.grain, mhw)
str(xval.fit)


###################################################
### code chunk number 3: mhw_1d.Rnw:106-110
###################################################
lim <- range(xval.fit$pred, mhw$straw)
plot(mhw$straw ~ xval.fit$pred, asp=1, xlim=lim, ylim=lim, xlab="LOOCV prediction", ylab="Actual")
abline(0,1)
grid()


###################################################
### code chunk number 4: mhw_1d.Rnw:117-120
###################################################
xval.res <- xval.fit$pred - mhw$straw
summary(xval.res)
hist(xval.res, main="LOOCV residuals")


###################################################
### code chunk number 5: mhw_1d.Rnw:133-136
###################################################
sqrt(sum(xval.res^2)/nrow(mhw))
sqrt(sum(residuals(model.straw.grain)^2)/(model.straw.grain$df.residual))
print(valid.rmsep)


###################################################
### code chunk number 6: mhw_1d.Rnw:149-151
###################################################
summary(xval.fit$coef, digits=5)
coefficients(model.straw.grain)


###################################################
### code chunk number 7: mhw_1d.Rnw:175-182
###################################################
round(sqrt(sum(xval.res^2)/nrow(mhw)),4)
round(valid.rmsep,4)
round(sqrt(sum(residuals(model.straw.grain)^2)/(model.straw.grain$df.residual)),4)
round(diff(range(xval.fit$coef[,2])),4)
round(coefficients(model.straw.grain)["grain"],4)
round(diff(range(xval.fit$coef[,1])),4)
round(coefficients(model.straw.grain)[1],4)

### R code from vignette source 'mhw_2'

###################################################
### code chunk number 1: mhw_2.Rnw:18-24
###################################################
options(prompt="> ", continue="+ ", digits=5, width=70)
par(mfrow=c(1,1))
# setwd("~/data/Journal/Monographs/StatsMonographs/MHW_Sweave/")
require(sp)
require(gstat)
require(lattice)


###################################################
### code chunk number 2: mhw_2.Rnw:44-50
###################################################
plot(c, r, type="n", xlab="column", ylab="row", ylim=c(20,1),
     main="Layout of the Mercer-Hall uniformity trial")
abline(v=1:25, lty=1, col="darkgray")
abline(h=1:20, lty=1, col="darkgray")
# rownames() gives the plot number
text(c,r, rownames(mhw), cex=.5)


###################################################
### code chunk number 3: mhw_2.Rnw:61-66
###################################################
plot(c, r, pch=21, col="black", bg="lightblue", ylim=c(20,1),
     xlab="column", ylab="row",
     main="Mercer-Hall uniformity trial",
     sub="Area of circles proportional to grain yield",
     cex=2*grain/max(grain))


###################################################
### code chunk number 4: mhw_2.Rnw:73-78
###################################################
# compute cutoff points for the octiles
(q8 <- quantile(grain, seq(0, 1, length=9)))
# classify each observation
grain.c <- cut(grain, q8, include.lowest=T, labels=F)
sort(unique(grain.c))


###################################################
### code chunk number 5: mhw_2.Rnw:85-87
###################################################
# look at the colour ramp: these are RRGGBB from 00 to FF (hex)
terrain.colors(8)


###################################################
### code chunk number 6: mhw_2.Rnw:108-113
###################################################
plot(c, r, pch=20, cex=2, bg="lightblue", ylim=c(20,1),
     xlab="column", ylab="row",
     main="Mercer-Hall uniformity trial",
     sub="Colour of circles from low yield (green) to high (gray)",
     col=terrain.colors(8)[grain.c])


###################################################
### code chunk number 7: mhw_2.Rnw:124-131
###################################################
require(sp)
plot(wireframe(grain ~ r + c, data=mhw, drape=T,
               aspect=c(1,.2), col.regions=bpy.colors(128),
               main="Grain yield, lb. per plot",
               screen= c(z=30, x=-60),
               xlab="N to S", ylab="W to E",
               sub="Looking SE from NW corner of field"))


###################################################
### code chunk number 8: mhw_2.Rnw:140-146
###################################################
plot(wireframe(grain ~ r + c, data=mhw, drape=T,
               aspect=c(1,.08), col.regions=bpy.colors(128),
               main="Grain yield, lb. per plot",
               screen= c(z=270, x=-75), zlab="",
               xlab="N to S", ylab="W to E",
               sub="Looking N from S end of field"))


###################################################
### code chunk number 9: mhw_2.Rnw:164-167
###################################################
ha2ac <- 2.471054
ft2m <- .3048
(field.area <- 10000/ha2ac)


###################################################
### code chunk number 10: mhw_2.Rnw:171-175
###################################################
(plot.area <- field.area/500)
(plot.len <- sqrt(field.area)/20)
(plot.wid <- sqrt(field.area)/25)
rm(ha2ac, ft2m, field.area)


###################################################
### code chunk number 11: mhw_2.Rnw:182-186
###################################################
(tot.len <- plot.len*20)
(tot.wid <- plot.wid*25)
tot.len * tot.wid
rm(tot.len, tot.wid)


###################################################
### code chunk number 12: mhw_2.Rnw:198-200
###################################################
plot.wid/2
plot.len/2


###################################################
### code chunk number 13: mhw_2.Rnw:205-211
###################################################
nrow <- length(unique(r))
ncol <- length(unique(c))
sx <- seq(plot.wid/2, plot.wid/2+(ncol-1)*plot.wid, length=ncol)
sy <- seq(plot.len/2+(nrow-1)*plot.len, plot.len/2, length=nrow)
xy <- expand.grid(x=sx, y=sy)
rm(nrow, ncol, sx, sy)


###################################################
### code chunk number 14: mhw_2.Rnw:231-232
###################################################
require(sp); require(gstat); require(lattice)


###################################################
### code chunk number 15: mhw_2.Rnw:249-252
###################################################
mhw.sp <- mhw
coordinates(mhw.sp) <- xy
summary(mhw.sp)


###################################################
### code chunk number 16: mhw_2.Rnw:261-262
###################################################
save(mhw.sp, file="mhw_spatial.RData")


###################################################
### code chunk number 17: mhw_2.Rnw:280-294
###################################################
mhw.sp.pix <- as(mhw.sp,"SpatialPixelsDataFrame") 
f1 <- spplot(mhw.sp.pix, zcol="grain", cuts=8,
             col.regions=bpy.colors(64),
             main="Grain yield, lb. per plot", key.space="right")
f2 <- spplot(mhw.sp.pix, zcol="straw", cuts=8,
             col.regions=heat.colors(64),
             main="Straw yield, lb. per plot", key.space="right")
f3 <- spplot(mhw.sp.pix, zcol="gsr", cuts=8,
             col.regions=terrain.colors(64),
             main="Grain/Straw ratio", key.space="right")
print(f1, split=c(1,1,2,2), more=T)
print(f2, split=c(2,1,2,2), more=T)
print(f3, split=c(1,2,2,2), more=F)
rm(f1, f2, f3)


###################################################
### code chunk number 18: mhw_2.Rnw:303-308
###################################################
print(spplot(mhw.sp, zcol="grain", pch=15, cex=1.6*grain/max(grain),
             cuts=12,
             col.regions=bpy.colors(64), main="Grain yield, lb. per plot", 
             sub="Symbol size proportional to yield",
             key.space="right"))


###################################################
### code chunk number 19: mhw_2.Rnw:344-346
###################################################
sort(by(grain, r, mean), decreasing=FALSE)
sort(by(grain, c, mean), d=F)


###################################################
### code chunk number 20: mhw_2.Rnw:356-359
###################################################
# show the rows horizontally
boxplot(grain ~ r, horizontal=T,  data=mhw, xlim=c(20,1),
        ylab="Row number", xlab="Grain yield, lb. per plot")


###################################################
### code chunk number 21: mhw_2.Rnw:361-364
###################################################
# and the columns vertically
boxplot(grain ~ c, data=mhw,
        xlab="Column number", ylab="Grain yield, lb. per plot")


###################################################
### code chunk number 22: mhw_2.Rnw:377-379
###################################################
ts1 <- lm(grain ~ coordinates(mhw.sp))
summary(ts1)


###################################################
### code chunk number 23: mhw_2.Rnw:388-389
###################################################
rm(ts1)


###################################################
### code chunk number 24: mhw_2.Rnw:406-409
###################################################
v <- variogram(grain ~ 1, mhw.sp,
               cutoff=plot.wid*10, width=plot.wid)
print(plot(v, plot.numbers=T))


###################################################
### code chunk number 25: mhw_2.Rnw:435-438
###################################################
(vm <- vgm(0.15, "Sph", 5, 0.02))
(vm <- vgm(0.03, "Sph", 20, add.to=vm))
print(plot(v, model= vm, main="Estimated variogram model"))


###################################################
### code chunk number 26: mhw_2.Rnw:443-445
###################################################
(vmf <- fit.variogram(v, vm))
print(plot(v, model= vmf, main="Fitted variogram model"))


###################################################
### code chunk number 27: mhw_2.Rnw:469-477
###################################################
set.seed(4502)
head(mhw$grain)
head(s1 <- sample(mhw$grain, length(mhw$grain), replace=FALSE))
head(s2 <- sample(mhw$grain, length(mhw$grain), replace=FALSE))
head(s3 <- sample(mhw$grain, length(mhw$grain), replace=FALSE))
head(s1)
head(s2)
head(s3)


###################################################
### code chunk number 28: mhw_2.Rnw:490-504
###################################################
par(mfrow=c(2,2))
plot(mhw$c, mhw$r, pch=20, cex=2, bg="lightblue", ylim=c(20,1),
     xlab="column", ylab="row", main="Randomization 1",
     col=terrain.colors(8)[cut(s1, q8, include.lowest=T, labels=F)])
plot(mhw$c, mhw$r, pch=20, cex=2, bg="lightblue", ylim=c(20,1),
     xlab="column", ylab="row", main="Randomization 2",
     col=terrain.colors(8)[cut(s2, q8, include.lowest=T, labels=F)])
plot(mhw$c, mhw$r, pch=20, cex=2, bg="lightblue", ylim=c(20,1),
     xlab="column", ylab="row", main="Randomization 3",
     col=terrain.colors(8)[cut(s3, q8, include.lowest=T, labels=F)])
plot(mhw$c, mhw$r, pch=20, cex=2, bg="lightblue", ylim=c(20,1),
     xlab="column", ylab="row", main="Actual spatial distribution",
     col=terrain.colors(8)[grain.c])
par(mfrow=c(1,1))


###################################################
### code chunk number 29: mhw_2.Rnw:515-526
###################################################
s1 <- data.frame(s1); coordinates(s1) <- xy
s2 <- data.frame(s2); coordinates(s2) <- xy
s3 <- data.frame(s3); coordinates(s3) <- xy
pv <- plot(variogram(grain ~ 1, mhw.sp, cutoff=plot.wid*10,
                     width=plot.wid), main="Real")
p1 <- plot(variogram(s1 ~ 1, s1, cutoff=plot.wid*10,
                     width=plot.wid), main="Simulation 1")
p2 <- plot(variogram(s2 ~ 1, s2, cutoff=plot.wid*10,
                     width=plot.wid), main="Simulation 2")
p3 <- plot(variogram(s3 ~ 1, s3, cutoff=plot.wid*10,
                     width=plot.wid), main="Simulation 3")


###################################################
### code chunk number 30: mhw_2.Rnw:530-534
###################################################
print(p1, split=c(1,1,2,2), more=T)
print(p2, split=c(2,1,2,2), more=T)
print(p3, split=c(1,2,2,2), more=T)
print(pv, split=c(2,2,2,2), more=F)


###################################################
### code chunk number 31: mhw_2.Rnw:548-549
###################################################
rm(xy, q8, grain.c, s1, s2, s3, pv, p1, p2, p3)


###################################################
### code chunk number 32: mhw_2.Rnw:564-570
###################################################
mhw.sp.ns <- split(as.data.frame(mhw.sp), in.north)
coordinates(mhw.sp.ns$T) <- ~x+y
coordinates(mhw.sp.ns$F) <- ~x+y
summary(mhw.sp.ns)
# structure of just the N half
summary(mhw.sp.ns$T)


###################################################
### code chunk number 33: mhw_2.Rnw:579-581
###################################################
v.n <- variogram(grain ~ 1, mhw.sp.ns$T, cutoff=30)
v.s <- variogram(grain ~ 1, mhw.sp.ns$F, cutoff=30)


###################################################
### code chunk number 34: mhw_2.Rnw:586-593
###################################################
g.max = max(v$gamma, v.n$gamma, v.s$gamma)*1.2
plot.vgm.all <- plot(v, plot.numbers=T,
                     main="All", ylim=c(0,g.max))
plot.vgm.N <- plot(v.n, plot.numbers=T,
                   main="N half", ylim=c(0,g.max))
plot.vgm.S <- plot(v.s, plot.numbers=T,
                   main="S half", ylim=c(0,g.max))


###################################################
### code chunk number 35: mhw_2.Rnw:602-609
###################################################
# split screen and show all three
# must save the plots and then put them into one window
# make a stretched window to compare all 3
#windows(h=8, w=20)  # might need smaller numbers on your screen
print(plot.vgm.all, split=c(1,1,3,1), more=T)
print(plot.vgm.N, split=c(2,1,3,1), more=T)
print(plot.vgm.S, split=c(3,1,3,1), more=F)


###################################################
### code chunk number 36: mhw_2.Rnw:616-623
###################################################
(vmS <- vgm(.14, "Sph", 20, .09))
(vmN <- vgm(.08, "Sph", 13 , .11))
(vmSf <- fit.variogram(v.s, vmN))
(vmNf <- fit.variogram(v.n, vmS))
plot.vgm.all <- plot(v, plot.numbers=T, main="All", model=vmf, ylim=c(0,g.max))
plot.vgm.N <- plot(v.n, plot.numbers=T, main="N half", model=vmNf, ylim=c(0,g.max))
plot.vgm.S <- plot(v.s, plot.numbers=T, main="S half", model=vmSf, ylim=c(0,g.max))


###################################################
### code chunk number 37: mhw_2.Rnw:628-631
###################################################
print(plot.vgm.all, split=c(1,1,3,1), more=T)
print(plot.vgm.N, split=c(2,1,3,1), more=T)
print(plot.vgm.S, split=c(3,1,3,1), more=F)


###################################################
### code chunk number 38: mhw_2.Rnw:639-641
###################################################
# when done examining, clean up
rm(g.max, plot.vgm.all, plot.vgm.N, plot.vgm.S)


###################################################
### code chunk number 39: mhw_2.Rnw:651-652
###################################################
rm(v, v.n, v.s, vm, vmf, vmN, vmNf, vmS, vmSf)


###################################################
### code chunk number 40: mhw_2.Rnw:657-658
###################################################
rm(mhw.sp.ns)

### R code from vignette source 'mhw_2a'

###################################################
### code chunk number 1: mhw_2a.Rnw:18-23
###################################################
options(prompt="> ", continue="+ ", digits=5, width=70)
par(mfrow=c(1,1))
require(sp)
require(gstat)
require(lattice)


###################################################
### code chunk number 2: mhw_2a.Rnw:58-63
###################################################
mhw.rc <- mhw; coordinates(mhw.rc) <- ~r +c
v.map <- variogram(grain ~ 1, loc=mhw.rc, map=TRUE, cutoff=10, width=1)
summary(v.map$map$var1)
class(v.map)
plot(v.map, col.regions=bpy.colors(64), xlab="Row-wise (N-S) lag", ylab="Column-wise (W-E) lag")


###################################################
### code chunk number 3: mhw_2a.Rnw:79-84
###################################################
c0 <- var(mhw$grain)
v.map$map$cov <- c0  - v.map$map$var1
v.map$map$cor <- v.map$map$cov/c0
v.map$map$cor[which(is.na(v.map$map$cor))] <- 1
summary(v.map$map$cor)


###################################################
### code chunk number 4: mhw_2a.Rnw:92-100
###################################################
str(v.map$map@data)
n <- sqrt(length(v.map$map$cor))
v.map.mat <- matrix(v.map$map$cor, nrow=n, ncol=n)
plot(wireframe(v.map.mat, drape=T, aspect=c(1,.25),
               screen=c(z=225, x=-60), ylab="Column-wise (W-E) lag",
               xlab="Row-wise (N-S) lag", zlab="rho(h)",
               main="Autocorrelation surface, grain yields",
               col.regions=bpy.colors(72)))


###################################################
### code chunk number 5: mhw_2a.Rnw:122-125
###################################################
(c <- ceiling(n/2))
(v.map.mat[c:n,c])
(v.map.mat[c,c:n])


###################################################
### code chunk number 6: mhw_2a.Rnw:131-138
###################################################
par(mfrow=c(1,2))
str(v.map.mat)
plot(v.map.mat[c,c:n], type="h", ylim=c(-.2,1),main="Along columns (E-W)", ylab=expression(rho), col="blue", xlab="lag (rows)")
abline(h=0)
plot(v.map.mat[c:n,c], type="h", ylim=c(-.2,1), main="Along rows (N-S)", ylab=expression(rho), col="darkgreen", xlab="lag (columns)")
abline(h=0)
par(mfrow=c(1,1))


###################################################
### code chunk number 7: mhw_2a.Rnw:287-291
###################################################
# centre grain yields at zero
# organize as an array -- is this the right order?
m <- max(mhw$r); n <- max(mhw$c)
str(mhw.grain.matrix <- matrix(data=(mhw.rc$grain - mean(mhw.rc$grain)), nrow=m, ncol=n))


###################################################
### code chunk number 8: mhw_2a.Rnw:302-319
###################################################
## p, q lags in row, column direction
## positive column lags
cpq <- function(p, q, mat) {
  s <- 0
  for (i in 1:(m - p))
    for (j in 1:(n - q))
      s <- s + (mat[i,j]) * (mat[i+p, j+q])
  return((1/((m - p) * (n - q))) * s)
}
## negative column lags
cpmq <- function(p, q, mat) {
  s <- 0
  for (i in 1:(m - p))
    for (j in (q+1):n)
      s <- s + (mat[i,j]) * (mat[i+p, j-q])
  return((1/((m - p) * (n - q))) * s)
}


###################################################
### code chunk number 9: mhw_2a.Rnw:331-352
###################################################
## symmetry relations
## c(-p,q) = c(p, -q); c(-p, -q) = c(p, q)
## compute for the desired combination of lags
max.l <- 14; d <- 2*max.l + 1
ch <- matrix(0, d, d)
for (lag.r in 1:max.l)
  for (lag.c in 1:max.l) {
    ## c(p, -q) : lower-left
    ch[(max.l+1) + lag.r, (max.l +1) - lag.c] <- (cpmq(lag.r, lag.c, mhw.grain.matrix))
    ## c(-p, q) : upper-right
    ch[(max.l+1) - lag.r, (max.l+1) + lag.c] <- ch[(max.l+1) + lag.r, (max.l +1) - lag.c]
  }
for (lag.r in 0:max.l)
  for (lag.c in 0:max.l) {
    ## c(p, q) : lower-right, including centre
    ch[(max.l+1) + lag.r, (max.l+1) + lag.c] <- (cpq(lag.r, lag.c, mhw.grain.matrix))
    ## c(-p, -q) : upper-left
    ch[(max.l+1) - lag.r, (max.l+1) - lag.c] <- ch[(max.l+1) + lag.r, (max.l+1) + lag.c]
  }
ch <- ch/var(mhw$grain)
summary(as.vector(ch))


###################################################
### code chunk number 10: mhw_2a.Rnw:362-368
###################################################
plot(wireframe(ch, drape=T, aspect=c(1,.25),
               screen=c(z=225, x=-60), ylab="Column-wise (W-E) lag",
               xlab="Row-wise (N-S) lag", zlab="rho(h)",
               main="Autocorrelation surface, grain yields",
               auto.key=T,
               col.regions=bpy.colors(72)))


###################################################
### code chunk number 11: mhw_2a.Rnw:432-455
###################################################
grs <- function(r=0, s=0, t, L, cor.mat) {
  ## smoothing function: Bartlett lag window
  w <- function(x, y) {
    h <- sqrt(x^2 + y^2)
    return(ifelse(h <= L, 1 - (h/L),0))
  }
  max.p <- dim(cor.mat)[1]; max.q <- dim(cor.mat)[2]
  centre.p <- ((max.p-1)/2)+1; centre.q <- ((max.q-1)/2)+1
  sum <- 0
  for (q in -L:L) {
    if (centre.q+q+1 > max.q) break
    for (p in -L:L) {
      if (centre.p+p+1 > max.p) break
      ## cor.mat is dimensioned 1.. 2L+1
      ## p, q are dimensions -L...0... L
      ## so add offset to the matrix subscripts, from centre out
      s1 <- (cor.mat[centre.p+p,centre.q+q] * w(p,q) * cos((pi/t)*((r*p) + (s*q))))
      #      print(paste("x=",centre.p+p+L+1, "y=",centre.q+q+L+1, "s1=",round(s1,4)))
      sum <- sum + s1
    }
  }
  return(sum/(4*pi^2))
}


###################################################
### code chunk number 12: mhw_2a.Rnw:465-472
###################################################
# theta <- 50; dens <- rep(0, 2*theta+1)
theta <- 50; dens <- rep(0, theta+1)
#for (r in -theta:theta)
# for (r in 0:theta)
#  dens[theta+r+1] <- grs(r, 0, t=theta, L=10 ,cor.mat=ch)
for (s in 0:theta)
  dens[s+1] <- grs(0, s, t=theta, L=10 ,cor.mat=ch)


###################################################
### code chunk number 13: mhw_2a.Rnw:485-489
###################################################
plot(dens ~ seq(0, 0.5, length=theta+1), type="p", main="Spectral density, W-E", sub="window size 10", ylab="density", xlab="frequency, cycles", pch=20, cex=0.6)
dens.smooth <- spline(dens)
lines(dens.smooth$y ~ seq(0, 0.5, length=length(dens.smooth$x)), lty=1)
grid()


###################################################
### code chunk number 14: mhw_2a.Rnw:504-508
###################################################
l.s <- seq(4,14,by=2)
theta <- 50
dens <- matrix(rep(0, length(l.s)*(theta+1)),
               nrow=length(l.s))


###################################################
### code chunk number 15: mhw_2a.Rnw:516-521
###################################################
for (i in 1:length(l.s)) {
  for (s in 0:theta) {
    dens[i,s+1] <- grs(0, s, theta, l.s[i] ,ch)
  }
}


###################################################
### code chunk number 16: mhw_2a.Rnw:530-537
###################################################
plot(dens[1,] ~ seq(0, 0.5, length=theta+1), type="n", main="Spectral density, W-E", ylab="density", xlab="frequency, cycles", ylim=c(min(0,dens), max(dens)*1.1))
for (i in 1:length(l.s)) {
  dens.smooth <- spline(dens[i,])
  lines(dens.smooth$y ~ seq(0, 0.5, length=length(dens.smooth$x)), lty=i)
  text(0,max(dens[i,]),paste("L =",l.s[i],sep=""), pos=3)
}
grid()


###################################################
### code chunk number 17: mhw_2a.Rnw:563-570
###################################################
theta <- 50; dens <- rep(0, theta+1)
for (r in 0:theta)
  dens[r+1] <- grs(r, 0, t=theta, L=10 ,cor.mat=ch)
plot(dens ~ seq(0, 0.5, length=theta+1), type="p", main="Spectral density, N-S", sub="window size 10", ylab="density", xlab="frequency, cycles", pch=20, cex=0.6)
dens.smooth <- spline(dens)
lines(dens.smooth$y ~ seq(0, 0.5, length=length(dens.smooth$x)), lty=1)
grid()


###################################################
### code chunk number 18: mhw_2a.Rnw:588-599
###################################################
theta=25
dens <- matrix(rep(0, (theta+1)^2), nrow=theta+1)
for (s in 0:theta)
  for (r in 0:theta)
    dens[r+1,s+1] <- grs(r, s, theta, 10 ,ch)
wireframe(dens, drape=T, aspect=c(1,.35), screen=c(z=225, x=-60),
          xlab="N-S frequency",
          ylab="E-W frequency", zlab="density",
          auto.key=T,
          col.regions=topo.colors(72)
)


###################################################
### code chunk number 19: mhw_2a.Rnw:607-608
###################################################
rm(mhw.rc, v.map, c0, v.map.mat, m, n, c, max.l, lag.r, lag.c, r, s, theta, dens, dens.smooth, l.s, grs, ch)


### R code from vignette source 'mhw_2g.Rnw'

###################################################
### code chunk number 1: mhw_2g.Rnw:18-24
###################################################
options(prompt="> ", continue="+ ", digits=5, width=70)
par(mfrow=c(1,1))
# setwd("~/data/Journal/Monographs/StatsMonographs/MHW_Sweave/")
require(sp)
require(gstat)
require(lattice)


###################################################
### code chunk number 2: mhw_2g.Rnw:34-35
###################################################
coef(model.straw.grain <- lm(straw ~ grain, data=mhw.sp))


###################################################
### code chunk number 3: mhw_2g.Rnw:54-59
###################################################
mhw.sp$msg.res <- residuals(model.straw.grain)
mhw.sp.pix <- as(mhw.sp,"SpatialPixelsDataFrame")
spplot(mhw.sp.pix, zcol="msg.res", col.regions=bpy.colors(64),
       main="Linear model residuals",
       sub="straw ~ grain, lb. per plot")


###################################################
### code chunk number 4: mhw_2g.Rnw:75-77
###################################################
vr <- variogram(msg.res ~ 1, loc=mhw.sp, cutoff=plot.wid*10, width=plot.wid)
plot(vr, pl=T, pch=20, cex=1.5)


###################################################
### code chunk number 5: mhw_2g.Rnw:93-95
###################################################
(vgmr <- fit.variogram(vr, model=vgm(0.15, "Sph", 20, 0.05)))
plot(vr, model=vgmr, pch=20, cex=1.5)


###################################################
### code chunk number 6: mhw_2g.Rnw:247-248
###################################################
dnorm(x=c(-1.96,-1,-0.5,0,.5,1,1.96), mean=0, sd=1)


###################################################
### code chunk number 7: mhw_2g.Rnw:254-256
###################################################
s <- seq(0.4, 2, by=.2)
data.frame(sd=s, p=dnorm(x=1, mean=0, sd=s))


###################################################
### code chunk number 8: mhw_2g.Rnw:269-280
###################################################
tmp <- rainbow(length(s))
curve(dnorm(x, mean=0, s[1]), -3, 3, col=tmp[1],
      main="Normal probability density",
      sub="Varying the standard deviation",
      ylab="density",xlab="residual")
for (i in 2:length(s))
  curve(dnorm(x, mean=0, sd=s[i]), -3, 3,
        col=tmp[i], add=T)
grid()
abline(v=1, lty=2)
legend(-3, 1, s, lty=1, col=tmp)


###################################################
### code chunk number 9: mhw_2g.Rnw:402-409
###################################################
like <- function(beta0, beta1, sigma, x, y) {
  s2 <- sigma^2; n <- length(y)
  # compute fits given the coefficients
  pred <- beta0 + beta1 * x
  loglike <- -(n/2)*(log(2*pi)) - (n/2)*(log(s2)) - (1/(2*s2))*(sum((y-pred)^2))
  return(loglike)	
} # like


###################################################
### code chunk number 10: mhw_2g.Rnw:418-420
###################################################
coefficients(lm(straw ~ grain, data=mhw))
summary(lm(straw ~ grain, data=mhw))$sigma


###################################################
### code chunk number 11: mhw_2g.Rnw:425-433
###################################################
coef <- round(coefficients(lm(straw ~ grain, data=mhw)),5)
(beta0 <- coef[1]*seq(0.9,1.1,by=.02))
(beta1 <- coef[2]*seq(0.9,1.1,by=.02))
(sigma <- round(summary(lm(straw ~ grain, data=mhw))$sigma,5)*seq(0.9,1.1,by=.02))
beta <- expand.grid(beta0=beta0, beta1=beta1, sigma=sigma)
beta$loglik <- 0
dim(beta)
rm(coef, beta0, beta1, sigma)


###################################################
### code chunk number 12: mhw_2g.Rnw:442-446
###################################################
for (i in 1:length(beta$loglik))
  beta$loglik[i] <- like(beta[i,"beta0"], beta[i,"beta1"], beta[i,"sigma"], mhw$grain, mhw$straw)
summary(beta$loglik)
(beta.mle <- beta[which.max(beta$loglik),])


###################################################
### code chunk number 13: mhw_2g.Rnw:462-479
###################################################
par(mfrow=c(1,3))
tmp <- beta[(beta$sigma==beta.mle$sigma),]
tmp <- tmp[(tmp$beta1==beta.mle$beta1),]
plot(tmp$loglik ~ tmp$beta0, type="b", xlab="intercept", ylab="log-likelihood", main="Constant slope, s.d.")
grid()
abline(v=beta.mle$beta0)
tmp <- beta[(beta$sigma==beta.mle$sigma),]
tmp <- tmp[(tmp$beta0==beta.mle$beta0),]
plot(tmp$loglik ~ tmp$beta1, type="b", xlab="slope", ylab="log-likelihood", main="Constant intercept, s.d.")
grid()
abline(v=beta.mle$beta1)
tmp <- beta[(beta$beta1==beta.mle$beta1),]
tmp <- tmp[(tmp$beta0==beta.mle$beta0),]
plot(tmp$loglik ~ tmp$sigma, type="b", xlab="s.d.", ylab="log-likelihood", main="Constant intercept, slope")
grid()
abline(v=beta.mle$sigma)
par(mfrow=c(1,3))


###################################################
### code chunk number 14: mhw_2g.Rnw:487-489
###################################################
tmp <- beta[(beta$sigma==beta.mle$sigma),]
wireframe(loglik ~ beta0 + beta1, data=tmp, aspect=c(1,.5), drape=T, main="Log-likelihood, Constant s.d.")


###################################################
### code chunk number 15: mhw_2g.Rnw:502-503
###################################################
rm(like, beta, i, beta.mle, tmp)


###################################################
### code chunk number 16: mhw_2g.Rnw:673-675
###################################################
require(nlme)
help(gls)


###################################################
### code chunk number 17: mhw_2g.Rnw:692-700
###################################################
system.time(
  model.gls.straw.grain <-
    gls(model=straw ~ grain,
        data=as.data.frame(mhw.sp),
        correlation=corSpher(
          value=c(vgmr[2,"range"],vgmr[1,"psill"]),
          form=~x+y, nugget=T))
)


###################################################
### code chunk number 18: mhw_2g.Rnw:707-708
###################################################
summary(model.gls.straw.grain)


###################################################
### code chunk number 19: mhw_2g.Rnw:731-733
###################################################
coefficients(model.gls.straw.grain)
coefficients(model.straw.grain)


###################################################
### code chunk number 20: mhw_2g.Rnw:753-757
###################################################
logLik(model.gls.straw.grain)
logLik(model.straw.grain)
AIC(model.gls.straw.grain)
AIC(model.straw.grain)


###################################################
### code chunk number 21: mhw_2g.Rnw:775-780
###################################################
plot(straw ~ grain, data=mhw, pch=20,
     sub="black: OLS; red: GLS")
grid()
abline(model.straw.grain)
abline(model.gls.straw.grain, col="red")


###################################################
### code chunk number 22: mhw_2g.Rnw:806-824 (eval = FALSE)
###################################################
## (vgmr.exp <- fit.variogram(vr, model=vgm(0.15, "Exp", 20*3, 0.05)))
## plot(vr, model=vgmr.exp)
## model.gls.straw.grain.exp <-
##   gls(model=straw ~ grain,
##       data=as.data.frame(mhw.sp),
##       correlation=corExp(
##         value=c(vgmr.exp[2,"range"],vgmr.exp[1,"psill"]),
##         form=~x+y, nugget=T))
## summary(model.gls.straw.grain.exp)
## coefficients(model.gls.straw.grain)-
##   coefficients(model.gls.straw.grain.exp)
## plot(straw ~ grain, data=mhw, pch=20,
##      sub="black: OLS; red: GLS (corSpher), green: GLS (corExp)")
## grid()
## abline(model.straw.grain)
## abline(model.gls.straw.grain, col="red")
## abline(model.gls.straw.grain.exp, col="darkgreen")
## rm(model.gls.straw.grain.exp, vgmr.exp)


###################################################
### code chunk number 23: mhw_2g.Rnw:888-889
###################################################
library(spgwr)


###################################################
### code chunk number 24: mhw_2g.Rnw:900-901
###################################################
(bw <- gwr.sel(straw ~ grain, data=mhw.sp, adapt=F, verbose=F))


###################################################
### code chunk number 25: mhw_2g.Rnw:912-914
###################################################
(vmf.gsr <- fit.variogram(v.gsr <- variogram(gsr ~ 1, loc=mhw.sp),
                          model=vgm(0.004, "Sph", 10, 0.002)))


###################################################
### code chunk number 26: mhw_2g.Rnw:929-930
###################################################
(gwr.sg <- gwr(straw ~ grain, data=mhw.sp, bandwidth=bw))


###################################################
### code chunk number 27: mhw_2g.Rnw:947-952
###################################################
gwr.coef <- as(gwr.sg$SDF,"SpatialPixelsDataFrame")
print(spplot(gwr.coef, zcol="grain",
             col.regions=bpy.colors(64),
             key.space="right", cuts=8,
             main="Slope: straw ~ grain"))


###################################################
### code chunk number 28: mhw_2g.Rnw:966-970
###################################################
print(spplot(gwr.coef, zcol="gwr.e",
             col.regions=bpy.colors(64),
             key.space="right", cuts=8,
             main="Slope: straw ~ grain"))


###################################################
### code chunk number 29: mhw_2g.Rnw:975-979
###################################################
vr.gwr <- variogram(gwr.e ~ 1,
                    loc=as(gwr.coef, "SpatialPointsDataFrame"))
(vmf.r.gwr <- fit.variogram(vr.gwr,
                            model=vgm(0.1, "Sph", 5, 0.2)))


###################################################
### code chunk number 30: mhw_2g.Rnw:992-995
###################################################
mhw.sp$gls.res <- residuals(model.gls.straw.grain)
vr.gls <- variogram(gls.res ~ 1, loc=mhw.sp, cutoff= plot.wid*10, width=plot.wid)
(vmf.r.gls <- fit.variogram(vr.gls, model=vgm(0.1, "Sph", 5, 0.2)))


###################################################
### code chunk number 31: mhw_2g.Rnw:1001-1016
###################################################
ylim.plot=c(0, max(vr.gwr$gamma, vr.gls$gamma, vr$gamma))
plot(gamma ~ dist, data=vr.gwr, ylim=ylim.plot,
     type="b", lty=2, col="red", xlab="separation, m",
     ylab="semivariance, (lbs plot-1)^2",
     main="Regression model residuals")
lines(variogramLine(vmf.r.gwr, maxdist=max(vr.gwr$dist)),
      col="red")
points(gamma ~ dist, data=vr, type="b", lty=2, col="blue")
lines(variogramLine(vgmr, maxdist=max(vr.gwr$dist)),
      col="blue")
points(gamma ~ dist, data=vr.gls, type="b", lty=2, col="green")
lines(variogramLine(vmf.r.gls, maxdist=max(vr.gwr$dist)),
      col="green")
legend(20,0.15,c("OLS", "GLS", "GWR"), lty=1,
       col=c("blue","green","red"))


### R code from vignette source 'mhw_3'

###################################################
### code chunk number 1: mhw_3.Rnw:30-31
###################################################
round(100 * sd(mhw$grain)/mean(mhw$grain),1)


###################################################
### code chunk number 2: mhw_3.Rnw:40-42
###################################################
cv <- function(x) { round(100*sd(x) / mean(x),1) }
class(cv)


###################################################
### code chunk number 3: mhw_3.Rnw:51-52
###################################################
cv(mhw$grain)


###################################################
### code chunk number 4: mhw_3.Rnw:81-82
###################################################
plot.len; plot.wid; plot.area


###################################################
### code chunk number 5: mhw_3.Rnw:89-94
###################################################
plots <- data.frame(acre.fraction=c(500, 250, 125, 100, 50, 25, 10),
                    adj.rows=c(1, 2, 4, 1, 2, 4, 10),
                    adj.col=c(1, 1, 1, 5, 5, 5, 5))
row.names(plots) <- c("1/500", "1/250", "1/125", "1/100", "1/50", "1/25", "1/10")
str(plots)


###################################################
### code chunk number 6: mhw_3.Rnw:99-103
###################################################
plots <- cbind(plots, len = plot.len*plots$adj.row)
plots <- cbind(plots, wid = plot.wid*plots$adj.col)
plots <- cbind(plots, area = plots$len * plots$wid)
plots


###################################################
### code chunk number 7: mhw_3.Rnw:120-121
###################################################
head(mhw$r%%2, 20)


###################################################
### code chunk number 8: mhw_3.Rnw:126-130
###################################################
tmp <- unstack(mhw, grain ~ r%%2); str(tmp)
grain.250 <- tmp$X0 + tmp$X1; rm(tmp)
str(grain.250)
cv(grain.250)


###################################################
### code chunk number 9: mhw_3.Rnw:139-143
###################################################
plots.250 <- data.frame(r = seq(1.5, 19.5, by=2),
                        c=rep(1:25, each=10),
                        grain = grain.250)
str(plots.250)


###################################################
### code chunk number 10: mhw_3.Rnw:152-160
###################################################
plot(plots.250$c, plots.250$r, pch=20, cex=2,
     bg="lightblue", xlab="column", ylab="row",
     main="Grain yield of 1/250 acre plots",
     sub="Colour of circles from low yield (green) to high (gray)",
     xlim=c(1, 25), ylim=c(20, 1),
     col=terrain.colors(8)[cut(grain,
                               quantile(grain, seq(0, 1, length=9)),
                               include.lowest=T, labels=F)])


###################################################
### code chunk number 11: mhw_3.Rnw:176-185
###################################################
tmp <- unstack(mhw, grain ~ r%%4); str(tmp)
grain.125 <- apply(tmp, 1, sum)
rm(tmp)
str(grain.125)
cv(grain.125)
plots.125 <- data.frame(r = seq(2, 18, by=4),
                        c=rep(1:25, each=10),
                        grain = grain.125)
str(plots.125)


###################################################
### code chunk number 12: mhw_3.Rnw:191-199
###################################################
tmp <- unstack(mhw, grain ~ c %% 5)
grain.100 <- apply(tmp, 1, sum)
rm(tmp)
cv(grain.100)
plots.100 <- data.frame(r = rep(1:20, each=5),
                        c=seq(3, 23, by=5),
                        grain = grain.100)
str(plots.100)


###################################################
### code chunk number 13: mhw_3.Rnw:206-214
###################################################
tmp <- unstack(plots.100, grain ~ r %% 2)
grain.50 <-  apply(tmp, 1, sum)
rm(tmp)
cv(grain.50)
plots.50 <- data.frame(r = rep(seq(1.5, 19.5, by=2), each=5),
                       c=seq(3, 23, by=5),
                       grain = grain.50)
str(plots.50)


###################################################
### code chunk number 14: mhw_3.Rnw:219-227
###################################################
tmp <- unstack(plots.100, grain ~ r %% 4)
grain.25 <-  apply(tmp, 1, sum)
rm(tmp)
cv(grain.25)
plots.25 <- data.frame(r = rep(seq(2.5, 18.5, by=4), each=5),
                       c=seq(3, 23, by=5),
                       grain = grain.25)
str(plots.25)


###################################################
### code chunk number 15: mhw_3.Rnw:232-240
###################################################
tmp <- unstack(plots.100, grain ~ r %% 10)
grain.10 <-  apply(tmp, 1, sum)
rm(tmp)
cv(grain.10)
plots.10 <- data.frame(r = rep(seq(5.5, 15.5, by=10), each=5),
                       c=seq(3, 23, by=5),
                       grain = grain.10)
str(plots.10)


###################################################
### code chunk number 16: mhw_3.Rnw:253-260
###################################################
summary(mhw$grain)
summary(plots.250$grain)/2
summary(plots.125$grain)/4
summary(plots.100$grain)/5
summary(plots.50$grain)/10
summary(plots.25$grain)/20
summary(plots.10$grain)/50


###################################################
### code chunk number 17: mhw_3.Rnw:268-287
###################################################
print(size.cv <- data.frame(area = plots$area, cv = c(
  cv(mhw$grain), cv(plots.250$grain), cv(plots.125$grain),
  cv(plots.100$grain), cv(plots.50$grain), cv(plots.25$grain),
  cv(plots.10$grain))))
plot(size.cv$area, size.cv$cv, xlab="Plot size, m^2",
     ylab="Coefficient of variation, %",
     main="Plot size vs. CV, Mercer-Hall grain", type="b",
     xlim=c(0,600))
grid()
text(size.cv$area, size.cv$cv, pos=4,
     paste(
       plots$adj.rows,
       " row",
       ifelse(plots$adj.rows==1,"","s"),
       ", ",
       plots$adj.col,
       " column",
       ifelse(plots$adj.col==1,"","s"),
       sep=""))


###################################################
### code chunk number 18: mhw_3.Rnw:297-303 (eval = FALSE)
###################################################
## rm(cv,
##    plot.len, plot.wid, plot.area,plots,
##    plots.250, plots.125, plots.100, plots.50,
##    plots.25, plots.10,
##    grain.250, grain.125, grain.100, grain.50,
##    grain.25, grain.10, size.cv)


###################################################
### code chunk number 19: mhw_3.Rnw:320-321
###################################################
plots[,c("len", "wid", "area")]


###################################################
### code chunk number 20: mhw_3.Rnw:355-360
###################################################
rm(cv,
   plots,
   plots.250, plots.125, plots.100, plots.50, plots.25, plots.10, grain.250, grain.125, grain.100, grain.50, grain.25,  grain.10,
   size.cv
)

### R code from vignette source 'mhw_b'

###################################################
### code chunk number 1: mhw_b.Rnw:22-23 (eval = FALSE)
###################################################
## colours()


###################################################
### code chunk number 2: mhw_b.Rnw:41-45
###################################################
plot(seq(1:length(colors())), rep(2, length(colours())), type="h",
     lwd=2, col=colors(), ylim=c(0,1), xlab="Colour number",
     ylab="", yaxt="n",
     main="Colours available with the colour() function") 


###################################################
### code chunk number 3: mhw_b.Rnw:51-55 (eval = FALSE)
###################################################
## abline(h=0.5, lwd=3)
## (selected <- identify(seq(1:length(colors())),
##                       rep(0.5, length(colors()))))
## colors()[selected]; rm(selected)


###################################################
### code chunk number 4: mhw_b.Rnw:70-71
###################################################
palette(); palette()[2]


###################################################
### code chunk number 5: mhw_b.Rnw:76-78
###################################################
boxplot(mhw$straw ~ mhw$r, col=mhw$r, xlab="row",
        ylab="Straw yield, lbs plot-1")


###################################################
### code chunk number 6: mhw_b.Rnw:85-89
###################################################
palette(gray(seq(0,.9,len=20))); palette()
boxplot(mhw$straw ~ mhw$r, col=mhw$r,
        xlab="row", ylab="Straw yield, lbs plot-1")
palette("default"); palette()


###################################################
### code chunk number 7: mhw_b.Rnw:94-95
###################################################
as.numeric("0xe6")/as.numeric("0xff")

# end of script;