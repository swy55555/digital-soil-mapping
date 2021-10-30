### R code from vignette source 'cr_1.Rnw'

###################################################
### code chunk number 1: cr_1.Rnw:20-24
###################################################
options(prompt="> ", continue="+ ", digits=5, width=70, show.signif.stars=T)
par(mfrow=c(1,1))
rm(list=ls())
setwd("~/data/Journal/Monographs/StatsMonographs/CorRegr_Sweave/")


###################################################
### code chunk number 2: cr_1.Rnw:58-59 (eval = FALSE)
###################################################
## file.show("obs.csv")


###################################################
### code chunk number 3: cr_1.Rnw:93-94
###################################################
obs <- read.csv("./ds/obs.csv")


###################################################
### code chunk number 4: cr_1.Rnw:97-98 (eval = FALSE)
###################################################
## obs <- read.csv("obs.csv")


###################################################
### code chunk number 5: cr_1.Rnw:101-103
###################################################
str(obs)
row.names(obs)


###################################################
### code chunk number 6: cr_1.Rnw:127-129
###################################################
obs$zone <- as.factor(obs$zone)
obs$wrb1 <- factor(obs$wrb1, labels=c("a", "c", "f"))


###################################################
### code chunk number 7: cr_1.Rnw:143-144
###################################################
save(obs, file="obs.RData")


###################################################
### code chunk number 8: cr_1.Rnw:149-150 (eval = FALSE)
###################################################
## load(file="obs.RData")


###################################################
### code chunk number 9: cr_1.Rnw:189-190
###################################################
plot.id <- 1:dim(obs)[1]


###################################################
### code chunk number 10: cr_1.Rnw:195-197
###################################################
t.obs <- cbind(plot.id, obs[, 1:6])
str(t.obs)


###################################################
### code chunk number 11: cr_1.Rnw:204-213
###################################################
t.layers <- cbind(plot.id = rep(plot.id, 3),
                  reshape(obs, direction="long",
                          drop=c("e", "n", "elev", "zone", "wrb1", "LC"),
                          varying=list(c("Clay1","Clay2","Clay5"),
                                       c("CEC1", "CEC2", "CEC5"), c("OC1", "OC2", "OC5")),
                          times=c("1", "2", "5")))
names(t.layers)[2:5] <- c("layer", "Clay", "CEC", "OC")
t.layers$layer <- as.factor(t.layers$layer)
str(t.layers)


###################################################
### code chunk number 12: cr_1.Rnw:227-229
###################################################
rm(plot.id)
save(t.obs, t.layers, file="t.RData")

### R code from vignette source 'cr_2.Rnw'

###################################################
### code chunk number 1: cr_2.Rnw:32-33
###################################################
while (is.element("obs", search())) detach(obs)


###################################################
### code chunk number 2: cr_2.Rnw:36-38
###################################################
attach(obs)
summary(Clay1); summary(Clay2); summary(Clay5)


###################################################
### code chunk number 3: cr_2.Rnw:53-55
###################################################
stem(Clay1); hist(Clay1)
hist(Clay1)


###################################################
### code chunk number 4: cr_2.Rnw:66-69
###################################################
hist(Clay1, breaks=seq(0, 96, by=8), col="darkgray", border="black",
     main="Clay proportion in surface soil, weight %")
rug(Clay1)


###################################################
### code chunk number 5: cr_2.Rnw:80-89
###################################################
h <- hist(Clay1, breaks=seq(0, 96, by=8), plot=F)
str(h)
plot(h, col = heat.colors(length(h$mids))[length(h$count)-rank(h$count)+1],
     ylim = c(0, max(h$count)+5),
     main="Clay proportion in surface soil, weight %",
     sub="Counts shown above bar, actual values shown with rug plot")
rug(Clay1)
text(h$mids, h$count, h$count, pos=3)
rm(h)


###################################################
### code chunk number 6: cr_2.Rnw:100-101
###################################################
obs[Clay1 > 65, ]


###################################################
### code chunk number 7: cr_2.Rnw:107-108
###################################################
(ix <- which(Clay1 > 65)); obs[ix, ]


###################################################
### code chunk number 8: cr_2.Rnw:149-167
###################################################
par(mfrow=c(2,2))
boxplot(Clay1, notch=T, horizontal=T,
        main="Boxplot of Clay 0-10cm")
#
hist(Clay1, freq=F, breaks=seq(0,100,5),
     main="Probability density for Clay 0-10cm")
lines(density(Clay1),lwd=2)
lines(density(Clay1, adj=.5),lwd=1)
lines(density(Clay1, adj=2),lwd=1.5)
#
qqnorm(Clay1, main="QQ plot for Clay 0-10cm vs Normal distribution",
       ylab="Clay %, 0-10cm")
qqline(Clay1, col=4)
#
qqnorm(log(Clay1), main="QQ plot for Clay 0-10cm vs lognormal distribution",
       ylab="log(Clay %), 0-10cm")
qqline(log(Clay1), col=4)
par(mfrow=c(1,1))


###################################################
### code chunk number 9: cr_2.Rnw:205-206
###################################################
t.test(Clay1, mu=30, conf.level=.99)

### R code from vignette source 'cr_3.Rnw'

###################################################
### code chunk number 1: cr_3.Rnw:176-195
###################################################
par(mfrow=c(2,2))   # 2x2 matrix of plots in one figure
#
plot(Clay1,Clay2); abline(0,1,lty=2);
title("default axes, subsoil vs. topsoil")
abline(h=mean(Clay2)); abline(v=mean(Clay1))
#
plot(Clay1,Clay2,xlim=c(0,100),ylim=c(0,100),pch=20); abline(0,1,lty=2)
title("axes 0..100, subsoil vs. topsoil")
abline(h=mean(Clay2)); abline(v=mean(Clay1))
#
plot(Clay2,Clay1); abline(0,1,lty=2)
title("default axes, topsoil vs. subsoil")
abline(h=mean(Clay1)); abline(v=mean(Clay2))
#
plot(Clay2,Clay1,xlim=c(0,100),ylim=c(0,100),pch=20); abline(0,1,lty=2)
title("axes 0..100, topsoil vs. subsoil")
abline(h=mean(Clay1)); abline(v=mean(Clay2))
#
par(mfrow=c(1,1))  # reset to 1 plot per figure


###################################################
### code chunk number 2: cr_3.Rnw:213-236
###################################################
par(mfrow=c(2,2))
#
plot(Clay1, Clay2, xlim=c(0,80), ylim=c(0,80),
     pch=as.numeric(wrb1))
abline(0,1,lty=2)
abline(h=mean(Clay2)); abline(v=mean(Clay1))
legend(60, 20, legend=levels(wrb1), pch=1:nlevels(wrb1), bty="n")
#
plot(Clay1, Clay2, xlim=c(0,80), ylim=c(0,80), pch=20,
     col=as.numeric(wrb1))
legend(60, 20, legend=levels(wrb1), pch=20,
       col=1:nlevels(wrb1), bty="n")
abline(0, 1, lty=2)
abline(h=mean(Clay2)); abline(v=mean(Clay1))
#
plot(Clay1, Clay2, xlim=c(0,80), ylim=c(0,80),
     pch=as.numeric(wrb1), col=as.numeric(wrb1))
abline(0, 1, lty=2, col=5)
abline(h=mean(Clay2)); abline(v=mean(Clay1))
legend(60, 20, levels(wrb1), pch=1:nlevels(wrb1),
       col=1:nlevels(wrb1), bty="n")
#
par(mfrow=c(1,1))


###################################################
### code chunk number 3: cr_3.Rnw:274-280
###################################################
sum((Clay2-mean(Clay2))*(Clay1-mean(Clay1)))/(length(Clay2)-1)
cov(Clay1,Clay2)
sd(Clay1); sd(Clay2)
cov(Clay1,Clay2)/(sd(Clay1)*sd(Clay2))
cor(Clay1,Clay2)
cor.test(Clay1,Clay2)


###################################################
### code chunk number 4: cr_3.Rnw:328-332
###################################################
plot(Clay2 ~ Clay1, pch=20,col=as.numeric(wrb1))
legend(60, 20, legend=levels(wrb1), pch=20, col=1:nlevels(wrb1), bty="n")
lines(lowess(Clay1, Clay2), lwd=2, col="blue")
abline(lm(Clay2 ~ Clay1), lty=2)


###################################################
### code chunk number 5: cr_3.Rnw:346-351
###################################################
plot(Clay1,Clay2,pch=20,col=as.numeric(wrb1))
legend(60, 20, legend=levels(wrb1),pch=20, col=1:3,bty="n")
for (f in c(0.1, .5, 2/3, 1)) {
  lines(lowess(Clay1, Clay2, f=f), lwd=2) }
abline(lm(Clay2 ~ Clay1), lty=2)


###################################################
### code chunk number 6: cr_3.Rnw:379-381
###################################################
lm21<-lm(Clay2 ~ Clay1)
summary(lm21)


###################################################
### code chunk number 7: cr_3.Rnw:409-413
###################################################
plot(Clay1, Clay2, pch=20)
title("Ordinary least-squares regression, subsoil vs. topsoil clay")
abline(lm21)  
segments(Clay1, Clay2, Clay1, fitted(lm21), lty=2)


###################################################
### code chunk number 8: cr_3.Rnw:433-434
###################################################
lm12<-lm(Clay1 ~ Clay2) ; summary(lm12)


###################################################
### code chunk number 9: cr_3.Rnw:484-487
###################################################
s2x<-var(Clay1); s2x
s2y<-var(Clay2); s2y
sxy<-var(Clay1,Clay2); sxy


###################################################
### code chunk number 10: cr_3.Rnw:495-498
###################################################
sum((Clay1-mean(Clay1))^2)/(length(Clay1)-1)
sum((Clay2-mean(Clay2))^2)/(length(Clay2)-1)
sum((Clay2-mean(Clay2))*(Clay1-mean(Clay1)))/(length(Clay1)-1)


###################################################
### code chunk number 11: cr_3.Rnw:525-529
###################################################
byx<-sxy/s2x; byx
ayx<-mean(Clay2)-byx*mean(Clay1); ayx
bxy<-sxy/s2y; bxy
axy<-mean(Clay1)-bxy*mean(Clay2); axy


###################################################
### code chunk number 12: cr_3.Rnw:568-572
###################################################
plot(fitted(lm21),Clay2,pch=20,xlab="Fitted",ylab="Observed",
     xlim=c(5,85),ylim=c(5,85),main="Observed vs. Fitted Clay %, 0-10cm")
abline(0,1)
segments(fitted(lm21),Clay2,fitted(lm21),fitted(lm21))


###################################################
### code chunk number 13: cr_3.Rnw:600-609
###################################################
plot(fitted(lm21), resid(lm21), pch=20, xlab="Fitted", ylab="Residual",
     main="Regression Residuals vs. Fitted Values, subsoil clay %")
sdres <- sd(residuals(lm21))
for (j in -3:3) abline(h=j*sqrt(var(resid(lm21))), col=abs(j)+1)
ix<-which(abs(resid(lm21))>2*sdres)
text(fitted(lm21)[ix], resid(lm21)[ix], ix, pos=4)
cbind(obs[ix,c("Clay1","Clay2")], fit=round(fitted(lm21)[ix],1),
      resid=round(resid(lm21)[ix],1))
rm(sdres, ix)


###################################################
### code chunk number 14: cr_3.Rnw:631-632
###################################################
stem(residuals(lm21), scale=2)


###################################################
### code chunk number 15: cr_3.Rnw:650-652
###################################################
qqnorm(residuals(lm21))
qqline(residuals(lm21))


###################################################
### code chunk number 16: cr_3.Rnw:674-675
###################################################
shapiro.test(residuals(lm21))


###################################################
### code chunk number 17: cr_3.Rnw:712-731
###################################################
par(mfrow=c(1,2))
h <- hatvalues(lm21)
hi.lev <- which(h>3*mean(h)); hi.lev
Clay1[hi.lev]
(sort.list(Clay1))[(length(Clay1)-5):length(Clay1)]
(sort.list(Clay1))[1:5]
#
plot(Clay1,h);  abline(v=mean(Clay1),lty=2)
abline(h=seq(1:3)*mean(h),col=c(1,4,2))
points(Clay1[hi.lev],hatvalues(lm21)[hi.lev],pch=20,col=2,cex=2.5)
text(Clay1[hi.lev],hatvalues(lm21)[hi.lev],paste("obs", hi.lev),adj=1.5)
#
plot(Clay1,Clay2); abline(lm21)
points(Clay1[hi.lev],Clay2[hi.lev],pch=20,col=2,cex=2.5)
text(Clay1[hi.lev],Clay2[hi.lev],paste("obs", hi.lev),adj=1.5)
#
lm21.2 <- lm(Clay2~Clay1,subset=-hi.lev)
round(coefficients(lm21), 3); round(coefficients(lm21.2), 3)
par(mfrow=c(1,1))


###################################################
### code chunk number 18: cr_3.Rnw:752-756
###################################################
sqrt(sum(residuals(lm21)^2)/length(residuals(lm21)))
sqrt(sum(residuals(lm21.2)^2)/length(residuals(lm21.2)))
1-(sum(residuals(lm21)^2)/sum((Clay2-mean(Clay2))^2))
1-(sum(residuals(lm21.2)^2)/sum((Clay2[-hi.lev]-mean(Clay2[-hi.lev]))^2))


###################################################
### code chunk number 19: cr_3.Rnw:822-823
###################################################
round(6.0354+0.9821*55,0)  


###################################################
### code chunk number 20: cr_3.Rnw:828-830
###################################################
pred <- predict(lm21,data.frame(Clay1 = 55),se.fit=T); str(pred)
round(pred$fit + qt(c(.025,.975), pred$df) * pred$se.fit, 1)


###################################################
### code chunk number 21: cr_3.Rnw:858-863
###################################################
pframe <- data.frame(Clay1=seq(min(Clay1), max(Clay1), by=1))
pred.c <- predict(lm21, pframe, interval="confidence", level=.95)
str(pred.c)
pred.p <- predict(lm21, pframe, interval="prediction", level=.95)
str(pred.p)


###################################################
### code chunk number 22: cr_3.Rnw:872-873
###################################################
pred.c[55+1-min(Clay1),]; pred.p[55+1-min(Clay1),]


###################################################
### code chunk number 23: cr_3.Rnw:890-898
###################################################
plot(pframe$Clay1,type="n",pred.c[,"fit"],xlab="Clay 0-10cm",
     ylab="Clay 20-30cm",xlim=c(0,80),ylim=c(0,80))
lines(pframe$Clay1,pred.c[,"fit"],lwd=2)
lines(pframe$Clay1,pred.c[,"lwr"],col=2,lwd=1.5)
lines(pframe$Clay1,pred.c[,"upr"],col=2,lwd=1.5)
lines(pframe$Clay1,pred.p[,"lwr"],col=4,lwd=1.5)
lines(pframe$Clay1,pred.p[,"upr"],col=4,lwd=1.5)
points(Clay1,Clay2)


###################################################
### code chunk number 24: cr_3.Rnw:930-941
###################################################
require(MASS)
lm21.r <- lqs(Clay2 ~ Clay1)
lm21 <- lm(Clay2 ~ Clay1)
class(lm21.r)
class(lm21)
summary(lm21.r)
summary(lm21)
coefficients(lm21.r)
coefficients(lm21)
1-sum(residuals(lm21.r)^2)/sum((Clay2-mean(Clay2))^2)
1-sum(residuals(lm21)^2)/sum((Clay2-mean(Clay2))^2)


###################################################
### code chunk number 25: cr_3.Rnw:950-954
###################################################
plot(Clay2 ~ Clay1, data=obs, xlab="Topsoil clay %", ylab="Subsoil clay %", main="Two regressions")
abline(lm21, lty=2)
abline(lm21.r, lty=1)
legend(50,30, legend=c("Robust","Least-squares"), lty=1:2)


###################################################
### code chunk number 26: cr_3.Rnw:1069-1076
###################################################
eqn18 <- function(y, x, lambda) {
  a <- var(y)-lambda*var(x);
  c <- var(x,y);
  (a + sqrt(a^2 + 4 * lambda * c^2))/(2*c)
}
eqn18(Clay2,Clay1,1)
eqn18(Clay2,Clay1,var(Clay2)/var(Clay1))


###################################################
### code chunk number 27: cr_3.Rnw:1083-1086
###################################################
lm21$coeff[2]
1/(lm21$coeff[2])
eqn18(Clay2,Clay1,1)


###################################################
### code chunk number 28: cr_3.Rnw:1091-1113
###################################################
s2x <- var(Clay1); s2y <- var(Clay2); sxy <- var(Clay1,Clay2)
byx <- sxy/s2x; ayx <- mean(Clay2)-byx*mean(Clay1)
bxyi <- 1/(sxy/s2y); axyi <- mean(Clay2)-bxyi*mean(Clay1)
b <- sqrt(s2y/s2x); a <- mean(Clay2)-b*mean(Clay1)
bp <- eqn18(Clay2,Clay1,1)
ap <- mean(Clay2)-bp*mean(Clay1)
plot(Clay1,Clay2,xlim=c(5,80),ylim=c(8,80))
par(adj=0.5); title("Correlation and regression","Clay 10-20 cm vs. Clay 0-10 cm")
par(adj=0); abline(ayx,byx,col=1,lty=1)
text(40,8,paste("Clay2 on Clay1; b=",round(byx,4),"; a=",round(ayx,2),sep=""),
     col=1)
abline(axyi,bxyi,col=2,lty=2)
text(40,12,paste("Clay1 on Clay2; b=",round(bxyi,4),"; a=",round(axyi,2),sep=""),
     col=2)
abline(a,b,col=3,lty=3)
text(40,16,paste("Proportional variance; b=",round(b,4),"; a=",round(a,2),sep=""),
     col=3)
abline(ap,bp,col=4,lty=4)
text(40,20,paste("Equal variance; b=",round(bp,4),"; a=",round(ap,2),sep=""),
     col=4)
text(8,70,paste("r = ",round(cor(Clay1,Clay2),4),"; r**2 = ",
                round((cor(Clay1,Clay2)^2)*100,1),"%",sep=""))


###################################################
### code chunk number 29: cr_3.Rnw:1142-1149
###################################################
pc <- princomp(cbind(Clay1,Clay2))
pc$loadings
b <- pc$loadings["Clay2","Comp.1"]/pc$loadings["Clay1","Comp.1"]; b
b <- pc$loadings[2,1]/pc$loadings[1,1]; b
a <- mean(Clay2)-b*mean(Clay1); a
pc$sdev
as.numeric(round(pc$sdev[1]/sum(pc$sdev)*100,1))


###################################################
### code chunk number 30: cr_3.Rnw:1169-1179
###################################################
par(mfrow=c(1,2))
plot(Clay1,  CEC1)
plot(Clay1,  CEC1,
     pch=as.numeric(wrb1)+14,
     col=as.numeric(wrb1), cex=1.5)
title("CEC vs clay, topsoil")
legend(10, 26, levels(wrb1),
       pch=as.numeric(levels(wrb1))+14,
       col=as.numeric(levels(wrb1)))
par(mfrow=c(1,1))


###################################################
### code chunk number 31: cr_3.Rnw:1193-1194
###################################################
cor.test(CEC1, Clay1)  


###################################################
### code chunk number 32: cr_3.Rnw:1209-1212
###################################################
model.cec1 <- lm(CEC1 ~ Clay1); summary(model.cec1)
plot(Clay1, CEC1)
abline(model.cec1)


###################################################
### code chunk number 33: cr_3.Rnw:1225-1242
###################################################
plot(fitted(model.cec1),CEC1,pch=20, xlab="Fitted", ylab="Observed",
     xlim=range(CEC1),ylim=range(CEC1))
title("Observed vs. Fitted topsoil CEC"); abline(0,1)
segments(fitted(model.cec1),CEC1,fitted(model.cec1),fitted(model.cec1))
#
sdres  <-  sd(residuals(model.cec1))
plot(fitted(model.cec1), resid(model.cec1), pch=20, xlab="Fitted", ylab="Residual",
     main="Regression Residuals vs. Fitted Values, topsoil CEC")
for (j in -3:3) abline(h=j*sqrt(var(resid(model.cec1))), col=abs(j)+1)
ix <- which(abs(resid(model.cec1))>2*sdres)
text(fitted(model.cec1)[ix], resid(model.cec1)[ix], ix, pos=4)
cbind(obs[ix,c("Clay1","Clay2")], fit=round(fitted(model.cec1)[ix],1),
      resid=round(resid(model.cec1)[ix],1))
rm(sdres, ix)
#
qqnorm(residuals(model.cec1))
qqline(residuals(model.cec1))


###################################################
### code chunk number 34: cr_3.Rnw:1264-1268
###################################################
head(CEC1, n=10)
head(rank(CEC1), n=10)
head(Clay1, n=10)
head(rank(Clay1), n=10)


###################################################
### code chunk number 35: cr_3.Rnw:1275-1277
###################################################
cor(rank(CEC1),rank(Clay1))
cor(CEC1,Clay1, method="spearman")


###################################################
### code chunk number 36: cr_3.Rnw:1282-1283
###################################################
cor(CEC1,Clay1)


###################################################
### code chunk number 37: cr_3.Rnw:1344-1345
###################################################
cor(Clay2,Clay1)^2

### R code from vignette source 'cr_4.Rnw'

###################################################
### code chunk number 1: cr_4.Rnw:48-49
###################################################
pairs( ~ Clay1 + OC1 + CEC1, data=obs)


###################################################
### code chunk number 2: cr_4.Rnw:71-72
###################################################
names(obs)


###################################################
### code chunk number 3: cr_4.Rnw:77-79
###################################################
cov(obs[c(10,7,13)])
cor(obs[c(10,7,13)])


###################################################
### code chunk number 4: cr_4.Rnw:96-100
###################################################
cor(residuals(lm(CEC1 ~ Clay1)), residuals(lm(OC1 ~ Clay1)))
cor(residuals(lm(CEC1 ~ OC1)), residuals(lm(Clay1 ~ OC1)))
cor(CEC1, OC1)
cor(CEC1, Clay1)


###################################################
### code chunk number 5: cr_4.Rnw:113-131
###################################################
par(mfrow=c(1,2))
par(adj=0.5)
plot(CEC1 ~ Clay1, pch=20, cex=1.5, xlim=c(0,100),
     xlab="Clay %",
     ylab="CEC, cmol+ (kg soil)-1")
abline(h=mean(CEC1), lty=2); abline(v=mean(Clay1), lty=2)
title("Simple Correlation, Clay vs. CEC 0-10 cm")
text(80, 4, cex=1.5, paste("r =",round(cor(Clay1, CEC1), 3)))
mr.1 <- residuals(lm(CEC1 ~ OC1)); mr.2 <-residuals(lm(Clay1 ~ OC1))
plot(mr.1 ~ mr.2, pch=20, cex=1.5, xlim=c(-50, 50),
     xlab="Residuals, Clay vs. OC, %",
     ylab="Residuals, CEC vs. OC, cmol+ (kg soil)-1")
abline(h=mean(mr.1), lty=2); abline(v=mean(mr.2), lty=2)
title("Partial Correlation, Clay vs. CEC, correcting for OC 0-10 cm")
text(25, -6, cex=1.5, paste("r =",round(cor(mr.1, mr.2), 3)))
par(adj=0)
rm(mr.1, mr.2)
par(mfrow=c(1,1))


###################################################
### code chunk number 6: cr_4.Rnw:147-155
###################################################
p.cor <- function(x){ 
  inv <- solve(var(x)) 
  sdi <- diag(1/sqrt(diag(inv)))
  p.cor.mat <- -(sdi %*% inv %*% sdi)
  diag(p.cor.mat) <- 1
  rownames(p.cor.mat) <- colnames(p.cor.mat) <- colnames(x)
  return(p.cor.mat) }
p.cor(obs[c(10,7,13)])


###################################################
### code chunk number 7: cr_4.Rnw:190-194
###################################################
lmcec.null<-lm(CEC1 ~ 1); summary(lmcec.null)
lmcec.oc<-lm(CEC1 ~ OC1); summary(lmcec.oc)
lmcec.clay<-lm(CEC1 ~ Clay1); summary(lmcec.clay)
lmcec.oc.cl<-lm(CEC1 ~ OC1 + Clay1); summary(lmcec.oc.cl)


###################################################
### code chunk number 8: cr_4.Rnw:236-240
###################################################
summary(lmcec.null)$adj.r.squared
summary(lmcec.oc)$adj.r.squared
summary(lmcec.clay)$adj.r.squared
summary(lmcec.oc.cl)$adj.r.squared


###################################################
### code chunk number 9: cr_4.Rnw:251-252
###################################################
AIC(lmcec.null); AIC(lmcec.oc); AIC(lmcec.clay); AIC(lmcec.oc.cl) 


###################################################
### code chunk number 10: cr_4.Rnw:264-266
###################################################
(a <- anova(lmcec.oc.cl, lmcec.clay))
diff(a$RSS)/a$RSS[2]


###################################################
### code chunk number 11: cr_4.Rnw:274-276
###################################################
(a <- anova(lmcec.oc.cl, lmcec.oc))
diff(a$RSS)/a$RSS[2]


###################################################
### code chunk number 12: cr_4.Rnw:299-315
###################################################
par(mfrow=c(1,2))
tmp <- qqnorm(residuals(lmcec.oc.cl), pch=20,
              main="Normal Q-Q plot, residuals from lm(CEC1 ~ OC1 + Clay1)")
qqline(residuals(lmcec.oc.cl))
diff <- (tmp$x - tmp$y)
### label the residuals that deviate too far from the line
text(tmp$x, tmp$y, ifelse((abs(diff) > 3), names(diff), ""), pos=2)
rm(tmp,diff)
### observed vs. fitted
#
plot(CEC1 ~ fitted(lmcec.oc.cl), pch=20,
     xlim=c(0,30), ylim=c(0,30),
     xlab="Fitted",ylab="Observed",
     main="Observed vs. Fitted CEC, 0-10cm")
abline(0,1); grid(col="black")
par(mfrow=c(1,1))


###################################################
### code chunk number 13: cr_4.Rnw:357-359
###################################################
# let stepwise pick the best from a full model
lms <- step(lm(Clay2 ~ Clay1 + CEC1 + OC1))


###################################################
### code chunk number 14: cr_4.Rnw:379-380
###################################################
lms <- step(lm(Clay5 ~ Clay1 + CEC1 + OC1 + Clay2 + CEC2 + OC2, data=obs))


###################################################
### code chunk number 15: cr_4.Rnw:405-408
###################################################
lm5z <- lm(Clay5 ~ zone);  summary(lm5z)
lm51 <- lm(Clay5 ~ Clay1); summary(lm51)
lm5z1 <- lm(Clay5 ~ zone + Clay1);  summary(lm5z1)


###################################################
### code chunk number 16: cr_4.Rnw:450-451
###################################################
stem(residuals(lm5z1))


###################################################
### code chunk number 17: cr_4.Rnw:465-471
###################################################
res.lo <- which(residuals(lm5z1) < -12)
res.hi <- which(residuals(lm5z1) > 9)
obs[res.lo, ]
predict(lm5z1)[res.lo]
obs[res.hi, ]
predict(lm5z1)[res.hi]


###################################################
### code chunk number 18: cr_4.Rnw:522-524
###################################################
require(car)
vif(lm(Clay5 ~ Clay1 + CEC1 + OC1 + Clay2 + CEC2 + OC2, data=obs))


###################################################
### code chunk number 19: cr_4.Rnw:538-540
###################################################
vif(lm(Clay5 ~ Clay1 + CEC1 + OC1 + CEC2 + OC2, data=obs))
vif(lm(Clay5 ~ Clay2 + CEC1 + OC1 + CEC2 + OC2, data=obs))


###################################################
### code chunk number 20: cr_4.Rnw:551-553
###################################################
AIC(lm(Clay5 ~ Clay1 + CEC1 + OC1 + CEC2 + OC2, data=obs))
AIC(step(lm(Clay5 ~ Clay1 + CEC1 + OC1 + CEC2 + OC2, data=obs), trace=0))


###################################################
### code chunk number 21: cr_4.Rnw:558-560
###################################################
AIC(lm(Clay5 ~ Clay2 + CEC1 + OC1 + CEC2 + OC2, data=obs))
AIC(step(lm(Clay5 ~ Clay2 + CEC1 + OC1 + CEC2 + OC2, data=obs) , trace=0))


###################################################
### code chunk number 22: cr_4.Rnw:572-573
###################################################
(lms.2 <- step(lm(Clay5 ~ Clay2 + CEC1 + OC1 + CEC2 + OC2, data=obs)))


###################################################
### code chunk number 23: cr_4.Rnw:588-589
###################################################
vif(lms)


###################################################
### code chunk number 24: cr_4.Rnw:605-608
###################################################
AIC(lms)
AIC(update(lms, . ~ . - Clay1))
AIC(update(lms, . ~ . - Clay2))


###################################################
### code chunk number 25: cr_4.Rnw:628-652
###################################################
# scatterplot, coloured by zone
plot(Clay5 ~ Clay1, col=as.numeric(zone), pch=20)
# zone 1
abline(coefficients(lm5z1)["(Intercept)"] , coefficients(lm5z1)["Clay1"])
# zone 2
for (iz in 2:4) {
  abline(coefficients(lm5z1)["(Intercept)"] 
         +  coefficients(lm5z1)[iz]
         , coefficients(lm5z1)["Clay1"], col=iz) }
# univariate line
abline(lm51, lty=2, lwd=1.5)
# legend
text(70, 30, pos=2,
     paste("Slopes: parallel:",
           round(coefficients(lm5z1)["Clay1"],3),
           "; univariate:",
           round(coefficients(lm51)["Clay1"],3)));
text(70, 26, pos=2,
     paste("   AIC: parallel:", floor(AIC(lm5z1)),
           "; univariate:", floor(AIC(lm51))));
text(70, 22, pos=2,
     paste("Pr(>F) parallel is not better:",
           round(anova(lm5z1,lm51)$"Pr(>F)"[2],)))
for (iz in 1:4) { text(65, 50-(3*iz), paste("zone",iz), col=iz) }


###################################################
### code chunk number 26: cr_4.Rnw:682-684
###################################################
lm51.z <- lm(Clay5 ~ Clay1 * zone)
summary(lm51.z)


###################################################
### code chunk number 27: cr_4.Rnw:729-743
###################################################
plot(Clay1, Clay5, xlim=c(10,80), ylim=c(10,80), pch=20, 
     cex=1.5, col=as.numeric(zone),xlab="Topsoil clay %", ylab="Subsoil clay %");
title("Subsoil vs. topsoil clay, by zone");
text(65, 40, "Slope of regression");
for (z in 1:4) {
  m <- lm(Clay5 ~ Clay1, subset=(zone==z));
  text(65, 40-(3*z), paste("zone",z,":",round(coefficients(m)[2], 3)), col=z);
  m.l <- loess(Clay5 ~ Clay1, subset=(zone==z), span=100);
  lines(y=c(min(m.l$fitted), max(m.l$fitted)), x=c(min(m.l$x), max(m.l$x)), col=z);
};
m <- lm(Clay5 ~ Clay1);
abline(m, col=6, lwd=1.5, lty=2);
text(65, 25, paste("overall:", round(coefficients(m)[2], 3)), col=6);
rm(m, m.l, z)


###################################################
### code chunk number 28: cr_4.Rnw:750-765
###################################################
plot(Clay1, Clay5, xlim=c(10,80), ylim=c(10,80), pch=as.numeric(zone), 
     xlab="Topsoil clay %", ylab="Subsoil clay %");
title("Subsoil vs. topsoil clay, by zone");
legend(10,75, pch=1:4, lty=1:4, legend=1:4)
text(65, 40, "Slopes:");
for (z in 1:4) {
  m <- lm(Clay5 ~ Clay1, subset=(zone==z));
  text(65, 40-(3*z), paste("zone",z,":",round(coefficients(m)[2], 3)));
  m.l <- loess(Clay5 ~ Clay1, subset=(zone==z), span=100);
  lines(y=c(min(m.l$fitted), max(m.l$fitted)), x=c(min(m.l$x), max(m.l$x)), lty=z, lwd=1.5);
};
m <- lm(Clay5 ~ Clay1);
abline(m);
text(65, 25, paste("overall:", round(coefficients(m)[2], 3)));
rm(m, m.l, z)


###################################################
### code chunk number 29: cr_4.Rnw:796-797
###################################################
lm51.z.n <- lm(Clay5 ~ zone/Clay1); summary(lm51.z.n)


###################################################
### code chunk number 30: cr_4.Rnw:818-820
###################################################
coefficients(lm51.z.n)["zone4:Clay1"] -
  (coefficients(lm51.z)["Clay1"] + coefficients(lm51.z)["Clay1:zone4"])  


###################################################
### code chunk number 31: cr_4.Rnw:840-841
###################################################
summary(lm5z); summary(lm51.z.n); summary(lm51.z)


###################################################
### code chunk number 32: cr_4.Rnw:854-857
###################################################
model.matrix(lm5z1)[15:22,]
model.matrix(lm51.z)[15:22,]
model.matrix(lm51.z.n)[15:22,]

### R code from vignette source 'cr_5.Rnw'

###################################################
### code chunk number 1: cr_5.Rnw:54-58
###################################################
sort(Clay5)
order(Clay5)
Clay5[order(Clay5)]
zone[order(Clay5)]


###################################################
### code chunk number 2: cr_5.Rnw:69-71
###################################################
by(Clay5,zone,range)
by(Clay5,zone,function(x) max(x)-min(x))


###################################################
### code chunk number 3: cr_5.Rnw:85-86
###################################################
boxplot(Clay5~zone, notch=T, horizontal=T, xlab="Clay %, 30-50cm", ylab="zone")


###################################################
### code chunk number 4: cr_5.Rnw:110-111
###################################################
by(Clay5, zone, summary)


###################################################
### code chunk number 5: cr_5.Rnw:129-130
###################################################
lmz<-lm(Clay5~zone); summary(lmz)


###################################################
### code chunk number 6: cr_5.Rnw:148-150
###################################################
summary(aov(lmz))
coefficients(aov(lmz))


###################################################
### code chunk number 7: cr_5.Rnw:254-258
###################################################
pairwise.t.test(Clay5, zone, p.adj="none")
pairwise.t.test(Clay5, zone, p.adj="holm")
pairwise.t.test(Clay5, zone, p.adj="none",pool.sd=F)
pairwise.t.test(Clay5, zone, p.adj="holm",pool.sd=F)


###################################################
### code chunk number 8: cr_5.Rnw:295-302
###################################################
mean(Clay5)
(means <- by(Clay5, zone, mean))
(tss <- sum((Clay5 - mean(Clay5))^2))
(rss <- sum((Clay5 - means[zone])^2))
(gss <- sum(((means-mean(Clay5))^2)*by(Clay5, zone, length)))
(gss+rss - tss)
1-(rss/tss)

### R code from vignette source 'cr_6.Rnw'

###################################################
### code chunk number 1: cr_6.Rnw:29-32
###################################################
plot(e, n, cex=Clay5*3/max(Clay5), pch=20, col=as.numeric(zone), asp=1)
grid(lty=1)  
title("Postplot of topsoil clay %, by soil type")


###################################################
### code chunk number 2: cr_6.Rnw:115-116
###################################################
while (is.element("package:spatial", search())) detach(package:spatial)


###################################################
### code chunk number 3: cr_6.Rnw:119-123
###################################################
require(spatial)
clay5.ls<-surf.ls(1, e, n, Clay5)
summary(clay5.ls)
clay5.ls$beta


###################################################
### code chunk number 4: cr_6.Rnw:135-139
###################################################
(predict(clay5.ls,
         diff(range(e))/2 + min(e),
         diff(range(n))/2 + min(n)))
clay5.ls$beta[1]


###################################################
### code chunk number 5: cr_6.Rnw:181-186
###################################################
c <- correlogram(clay5.ls, 50, plotit=F)
str(c)
plot(c, ylim=c(-.2, .6), xlim=c(0,12000), pch=20, col="blue")
text(c$x, c$y, round(c$y, 2), pos=3)
abline(h=0)


###################################################
### code chunk number 6: cr_6.Rnw:216-221
###################################################
# estimate function by eye: exponential
plot(c, ylim=c(-.2, .6), xlim=c(0,12000), pch=20, col="blue")
abline(h=0)
d <- seq(100,12000, by=100)
lines(d, expcov(d, d=600, alpha=.4), col="blue")


###################################################
### code chunk number 7: cr_6.Rnw:224-225
###################################################
rm(c, d)


###################################################
### code chunk number 8: cr_6.Rnw:230-235
###################################################
# fits fairly well at close range
# now fit the GLS surface
clay5.gls<-surf.gls(1, expcov, d=600, alpha=.4, e, n, Clay5)
summary(clay5.gls)
clay5.gls$beta


###################################################
### code chunk number 9: cr_6.Rnw:251-252
###################################################
while (is.element("package:lattice", search())) detach(package:lattice)


###################################################
### code chunk number 10: cr_6.Rnw:255-256
###################################################
require(lattice)


###################################################
### code chunk number 11: cr_6.Rnw:263-273
###################################################
xmin <- min(e); xmax <- max(e); ymin <- min(n); ymax <- max(n); res <- 40
clay5.ts <- trmat(clay5.ls, xmin, xmax, ymin, ymax, res)
clay5.gts <- trmat(clay5.gls, xmin, xmax, ymin, ymax, res)
eqscplot(clay5.gts, type="n",
         main="OLS and GLS trend surfaces, subsoil clay %", xlab="E", ylab="N")
contour(clay5.gts, level=seq(20, 80, 4), add=T)
contour(clay5.ts, level=seq(20, 80, 4), add=T, lty=2, col="blue")
grid(lty=1)
points(e, n, cex=Clay5*2.5/max(Clay5), pch=23, bg=3)
rm(clay5.ts, clay5.gts, xmin, xmax, ymin, ymax, res)


###################################################
### code chunk number 12: cr_6.Rnw:291-304
###################################################
xmin <- min(e); xmax <- max(e); ymin <- min(n); ymax <- max(n); res <- 40
clay5.gls <- surf.gls(1, expcov, d=600, alpha=.4, e, n, Clay5)
clay5.gls.resid <- resid(clay5.gls)
clay5.gts <- trmat(clay5.gls, xmin, xmax, ymin, ymax, res)
eqscplot(clay5.gts, type="n",
         main="Residuals from GLS 1st-order trend surface, subsoil clay %",
         sub="Red: negative; Green: positive",
         xlab="E", ylab="N")
contour(clay5.gts, level=seq(20, 80, 4), add=T)
grid(lty=1)
points(e, n, cex=abs(clay5.gls.resid)*2.5/max(abs(clay5.gls.resid)),
       pch=23, bg=ifelse(clay5.gls.resid < 0, 3, 2))
rm(clay5.gls, clay5.gts, clay5.gls.resid, xmin, xmax, ymin, ymax, res)


###################################################
### code chunk number 13: cr_6.Rnw:338-349
###################################################
xmin <- min(e); xmax <- max(e); ymin <- min(n); ymax <- max(n); res <- 40
clay5.gls2 <- surf.gls(2, expcov, d=600, alpha=.4, e, n, Clay5)
summary(clay5.gls2)
clay5.gts2 <- trmat(clay5.gls2, xmin, xmax, ymin, ymax, res)
eqscplot(clay5.gts2, type="n",
         main="GLS 2nd-order trend surface, subsoil clay %", xlab="E", ylab="N")
contour(clay5.gts2, level=seq(20, 80, 4), add=T)
grid(lty=1)
points(e, n, cex=Clay5*2.5/max(Clay5), pch=23, bg=3)
clay5.gls2$beta
rm(clay5.gls2, clay5.gts2, xmin, xmax, ymin, ymax, res)


###################################################
### code chunk number 14: cr_6.Rnw:381-383
###################################################
while (is.element("package:gstat", search())) detach(package:gstat)
while (is.element("package:sp", search())) detach(package:sp)


###################################################
### code chunk number 15: cr_6.Rnw:386-389
###################################################
require(sp)
require(gstat)
search()


###################################################
### code chunk number 16: cr_6.Rnw:399-400
###################################################
str(obs)


###################################################
### code chunk number 17: cr_6.Rnw:421-425
###################################################
class(obs)
obs.sp <- obs
coordinates(obs.sp) <- ~ e + n
class(obs.sp)


###################################################
### code chunk number 18: cr_6.Rnw:441-444
###################################################
str(obs.sp)
head(obs.sp@data)
summary(obs.sp@data)


###################################################
### code chunk number 19: cr_6.Rnw:460-462
###################################################
v <- variogram(Clay5 ~ 1, obs.sp)
print(plot(v, pl=T, pch=20, col="blue", cex=1.5))


###################################################
### code chunk number 20: cr_6.Rnw:492-494
###################################################
m <- vgm(100, "Exp", 15000/3, 50)
print(plot(v, pl=T, pch=20, col="blue", cex=1.5, model=m))


###################################################
### code chunk number 21: cr_6.Rnw:504-508
###################################################
(m.f <- fit.variogram(v, m))
str(m.f)
attr(m.f, "SSErr")
print(plot(v, pl=T, pch=20, col="blue", cex=1.5, model=m.f))


###################################################
### code chunk number 22: cr_6.Rnw:540-543
###################################################
diff(range(e))/1000
diff(range(n))/1000
diff(range(e)) * diff(range(n)) / 10^6


###################################################
### code chunk number 23: cr_6.Rnw:550-556
###################################################
res <- 500
g500 <- expand.grid(e = seq(min(e), max(e), by=res), n = seq(min(n), max(n), by=res))
coordinates(g500) <- ~ e + n
gridded(g500) <- T
str(g500)
rm(res)


###################################################
### code chunk number 24: cr_6.Rnw:562-564
###################################################
k.o <- krige(Clay5 ~ 1, obs.sp, g500, m.f)
str(k.o)


###################################################
### code chunk number 25: cr_6.Rnw:576-584
###################################################
plot.1 <- spplot(k.o, zcol="var1.pred",
                 main="OK prediction of Clay %, 30-50 cm", col.regions=bpy.colors(128),
                 pretty=T)
plot.2 <- spplot(k.o, zcol="var1.var",
                 main="OK prediction variance of Clay %, 30-50 cm", col.regions=cm.colors(128),
                 pretty=T)
print(plot.1, split=c(1,1,2,1), more=T)
print(plot.2, split=c(2,1,2,1), more=F)


###################################################
### code chunk number 26: cr_6.Rnw:624-628
###################################################
k.o.500 <- krige(Clay5 ~ 1, obs.sp, g500, m.f, block=c(500, 500))
str(k.o.500)
summary(k.o$var1.pred - k.o.500$var1.pred)
summary(k.o.500$var1.var / k.o$var1.var)

### R code from vignette source 'cr_8.Rnw'

###################################################
### code chunk number 1: cr_8.Rnw:39-43
###################################################
pc <- prcomp(obs[,c("CEC1","Clay1","OC1")])
class(pc)
str(pc)
summary(pc)


###################################################
### code chunk number 2: cr_8.Rnw:58-60
###################################################
pc.s <- prcomp(obs[c(10,7,13)], scale=T)
summary(pc.s)


###################################################
### code chunk number 3: cr_8.Rnw:71-72
###################################################
pc.s$rotation


###################################################
### code chunk number 4: cr_8.Rnw:91-102
###################################################
pc.s <- prcomp(obs[c(10,7,13)], scale=T, retx=T)
plot(pc.s$x[,1], pc.s$x[,2], pch=20,
     xlab="Standardised PC 1", ylab="Standardised PC 2")
abline(h=0); abline(v=0)
abline(h=2, col="red", lty=2); abline(v=3, col="red", lty=2)
abline(h=-2, col="red", lty=2); abline(v=-3, col="red", lty=2)
(pts <- which((abs(pc.s$x[,1]) >= 3) | (abs(pc.s$x[,2]) >= 2) ))
points(pc.s$x[pts,1], pc.s$x[pts,2], pch=21, col="red", bg="blue")
text(pc.s$x[pts,1], pc.s$x[pts,2], pts, pos=4, col="red")
pc.s$x[pts, c(1,2)]
obs[pts, c(10,7,13)]


###################################################
### code chunk number 5: cr_8.Rnw:126-128
###################################################
pc <- prcomp(obs[,c("CEC1","Clay1","OC1")], retx=TRUE, center=FALSE)
summary(as.matrix(obs[,c("CEC1","Clay1","OC1")])%*%pc$rotation - pc$x)


###################################################
### code chunk number 6: cr_8.Rnw:149-151
###################################################
obs.reconstruct <- pc$x %*% solve(pc$rotation)
summary(obs.reconstruct - obs[,c("CEC1","Clay1","OC1")])


###################################################
### code chunk number 7: cr_8.Rnw:165-174
###################################################
dim(solve(pc$rotation)[1,])
dim(solve(pc$rotation)[1,,drop=T])
dim(solve(pc$rotation)[1,,drop=F])
obs.reconstruct.1 <- pc$x[,1,drop=F] %*% solve(pc$rotation)[1,,drop=F]
summary(obs[,c("CEC1","Clay1","OC1")] - obs.reconstruct.1)
obs.reconstruct.2 <- pc$x[,1:2] %*% solve(pc$rotation)[1:2,]
summary(obs[,c("CEC1","Clay1","OC1")] - obs.reconstruct.2)
obs.reconstruct <- pc$x[,1:3] %*% solve(pc$rotation)[1:3,]
summary(obs[,c("CEC1","Clay1","OC1")] - obs.reconstruct)


###################################################
### code chunk number 8: cr_8.Rnw:188-196
###################################################
resid.reconstruct.1 <- obs[,c("CEC1","Clay1","OC1")] - pc$x[,1,drop=F] %*% solve(pc$rotation)[1,,drop=F]
summary(resid.reconstruct.1)
head(sort(resid.reconstruct.1[,"OC1"]))
head(sort(resid.reconstruct.1[,"OC1"], decreasing=T))
resid.reconstruct.2 <- obs[,c("CEC1","Clay1","OC1")] - pc$x[,1:2] %*% solve(pc$rotation)[1:2,]
summary(resid.reconstruct.2)
head(sort(resid.reconstruct.2[,"OC1"]))
head(sort(resid.reconstruct.2[,"OC1"], decreasing=T))


###################################################
### code chunk number 9: cr_8.Rnw:204-212
###################################################
par(mfrow=c(1,2))
ymax <- round(max(resid.reconstruct.1[,"OC1"],resid.reconstruct.2[,"OC1"]))
ymin <- round(min(resid.reconstruct.1[,"OC1"],resid.reconstruct.2[,"OC1"]))
plot(resid.reconstruct.1[,"OC1"] ~ obs[,"OC1"], main="Residuals, 1 PC reconstruction", xlab="Topsoil organic carbon, %", ylab="Residual, % OC", ylim=c(ymin,ymax))
abline(h=0, lty=2)
plot(resid.reconstruct.2[,"OC1"] ~ obs[,"OC1"], main="Residuals, 2 PC reconstruction", xlab="Topsoil organic carbon, %", ylab="Residual, % OC", ylim=c(ymin,ymax))
abline(h=0, lty=2)
par(mfrow=c(1,1))


###################################################
### code chunk number 10: cr_8.Rnw:223-231
###################################################
par(mfrow=c(1,2))
ymax <- round(max(resid.reconstruct.1[,"CEC1"],resid.reconstruct.2[,"CEC1"]))
ymin <- round(min(resid.reconstruct.1[,"CEC1"],resid.reconstruct.2[,"CEC1"]))
plot(resid.reconstruct.1[,"CEC1"] ~ obs[,"CEC1"], main="Residuals, 1 PC reconstruction", xlab="Topsoil CEC, cmol+ kg-1 soil", ylab="Residual, % CEC", ylim=c(ymin,ymax))
abline(h=0, lty=2)
plot(resid.reconstruct.2[,"CEC1"] ~ obs[,"CEC1"], main="Residuals, 2 PC reconstruction", xlab="Topsoil CEC, cmol+ kg-1 soil", ylab="Residual, % CEC", ylim=c(ymin,ymax))
abline(h=0, lty=2)
par(mfrow=c(1,1))


###################################################
### code chunk number 11: cr_8.Rnw:241-243
###################################################
range(resid.reconstruct.1[,"CEC1"])
range(resid.reconstruct.2[,"CEC1"])


###################################################
### code chunk number 12: cr_8.Rnw:254-256
###################################################
resid.reconstruct.2 <- scale(obs[,c("CEC1","Clay1","OC1")]) - pc.s$x[,1:2] %*% solve(pc.s$rotation)[1:2,]
summary(resid.reconstruct.2)


###################################################
### code chunk number 13: cr_8.Rnw:261-267
###################################################
par(mfrow=c(1,2))
plot(resid.reconstruct.2[,"OC1"] ~ scale(obs[,"OC1"]), main="Residuals, 2 PC reconstruction", xlab="Topsoil organic carbon, standardized", ylab="Residual, % OC standardized")
abline(h=0, lty=2)
plot(resid.reconstruct.2[,"CEC1"] ~ scale(obs[,"CEC1"]), main="Residuals, 2 PC reconstruction", xlab="Topsoil CEC, standardized", ylab="Residual, CEC standardized")
abline(h=0, lty=2)
par(mfrow=c(1,1))


###################################################
### code chunk number 14: cr_8.Rnw:316-324
###################################################
par(mfrow=c(2,2))
biplot(pc.s, main="Biplot, Standardized PCs 1 and 2",
       pc.biplot=T, cex=c(.9,1.2))
biplot(pc.s, main="Variables only, Standardized PCs 1 and 2",
       pc.biplot=T, cex=c(0.3,1.2), xlabs=rep("o", dim(pc.s$x)[1]))
biplot(pc.s, main="Observations only, Standardized PCs 1 and 2",
       pc.biplot=T, var.axes=F, cex=c(1,0.1))
par(mfrow=c(1,1))


###################################################
### code chunk number 15: cr_8.Rnw:349-350
###################################################
obs[c(1,2,78,81),c(7,10,13)]


###################################################
### code chunk number 16: cr_8.Rnw:387-390
###################################################
pc9 <- prcomp(obs[7:15], scale=T)
summary(pc9)
screeplot(pc9, main="Screeplot, 9 principal components")


###################################################
### code chunk number 17: cr_8.Rnw:409-414
###################################################
par(mfrow=c(2,2))
biplot(pc9, pc.biplot=T, main="Standardized PCs 1 and 2")
biplot(pc9, choice=2:3, pc.biplot=T, main="Standardized PCs 2 and 3")
biplot(pc9, choice=c(1,3), pc.biplot=T, main="Standardized PCs 1 and 3")
par(mfrow=c(1,1))


###################################################
### code chunk number 18: cr_8.Rnw:447-448
###################################################
(fa <- factanal(obs[7:15], 3))


###################################################
### code chunk number 19: cr_8.Rnw:470-471
###################################################
(fa <- update(fa, factors = 4))


###################################################
### code chunk number 20: cr_8.Rnw:482-496
###################################################
par(mfrow=c(2,2))
plot(loadings(fa), xlim=c(-.1,1.1), ylim=c(-.1,1.1), type="n",
     main="Loadings 1 and 2, 4-factor model")
text(loadings(fa),dimnames(loadings(fa))[[1]])
plot(loadings(fa)[,c(1,3)], xlim=c(-.1,1.1), ylim=c(-.1,1.1), type="n",
     main="Loadings 1 and 3, 4-factor model")
text(loadings(fa)[,c(1,3)],dimnames(loadings(fa))[[1]])
plot(loadings(fa)[,2:3], xlim=c(-.1,1.1), ylim=c(-.1,1.1), type="n",
     main="Loadings 2 and 3, 4-factor model")
text(loadings(fa)[,2:3],dimnames(loadings(fa))[[1]])
plot(loadings(fa)[,3:4], xlim=c(-.1,1.1), ylim=c(-.1,1.1), type="n",
     main="Loadings 3 and 4, 4-factor model")
text(loadings(fa)[,3:4],dimnames(loadings(fa))[[1]])
par(mfrow=c(1,1))

