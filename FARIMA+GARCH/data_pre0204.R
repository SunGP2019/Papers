library(rugarch)
dd<-read.csv("I://dropbox//Dropbox/Shouhuai-related/Zheyuan-Research/data/cyber_event_binned_data.csv")
setwd("I://dropbox//Dropbox/Shouhuai-related/Zheyuan-Research/Paper-1/")

xspec = ugarchspec(mean.model = list(armaOrder = c(0,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")
fit1=ugarchfit(xspec,dd[1:266,2],solver = "hybrid")
fit.att=fitted(fit1)
sig.att=sigma(fit1)
sres.att=residuals(fit1)/sig.att

xspec = ugarchspec(mean.model = list(armaOrder = c(1,1),arfima=F), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")
fit2=ugarchfit(xspec,dd[1:266,3],solver = "hybrid")
fit.len=fitted(fit2)
sig.len=sigma(fit2)


sres.len=residuals(fit2)/sig.len
?cor.test
cor.test(sres.att,sres.len,method = c("pearson", "kendall", "spearman"))
cor.test(sres.att,sres.len,method = c("kendall"))
cor.test(as.numeric(sres.att)),as.numeric(sres.len),method = c("pearson", "kendall", "spearman"))
cor.test(as.numeric(sres.att),as.numeric(sres.len),method = c("kendall"))
cor.test(as.numeric(sres.att),as.numeric(sres.len),method = c("pearson", "kendall", "spearman"))

xspec1 = ugarchspec(mean.model = list(armaOrder = c(0,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")
roll1=ugarchroll(xspec1,dd[,2],forecast.length=100,solver = "hybrid",refit.every = 1, refit.window = c("recursive"),
calculate.VaR = TRUE, VaR.alpha = c(0.05,.1,.2,.3,.4,.5),keep.coef = TRUE)


report(roll1, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)
report(roll1, type="VaR", VaR.alpha = 0.05, conf.level = 0.9)
report(roll1, type="VaR", VaR.alpha = 0.1, conf.level = 0.95)
report(roll1, type="VaR", VaR.alpha = 0.2, conf.level = 0.95)
report(roll1, type="VaR", VaR.alpha = 0.3, conf.level = 0.95)
require(devtools)
install_bitbucket("rugarch","alexiosg")
report(roll1, type="VaR", VaR.alpha = 0.2, conf.level = 0.95)
install.packages("rugarch")
report(roll1, type="VaR", VaR.alpha = 0.2, conf.level = 0.95)
library(rugarch)
install.packages("rugarch")

########################################################################
library(rugarch)
dd<-read.csv("I://dropbox//Dropbox/Shouhuai-related/Zheyuan-Research/data/cyber_event_binned_data.csv")
setwd("I://dropbox//Dropbox/Shouhuai-related/Zheyuan-Research/Paper-1/")

xspec1 = ugarchspec(mean.model = list(armaOrder = c(0,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")
roll1=ugarchroll(xspec1,dd[,2],forecast.length=100,solver = "hybrid",refit.everysetwd("I://dropbox//Dropbox/Shouhuai-related/Zheyuan-Research/output-count/")
                 = 1, refit.window = c("recursive"),
                 calculate.VaR = TRUE, VaR.alpha = c(0.05,.1,.2,.3,.4,.5),keep.coef = TRUE)
report(roll1, type="VaR", VaR.alpha = 0.2, conf.level = 0.95)

setwd("I://dropbox//Dropbox/Shouhuai-related/Zheyuan-Research/Paper-1/output-sum/")

dd[,4]=dd[,3]*dd[,2]

for(p in 0:5){
for(q in 0:5){
xspec = ugarchspec(mean.model = list(armaOrder = c(p,q),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")
fit1=ugarchfit(xspec,dd[1:266,4],solver = "hybrid")
capture.output(fit1,file=paste("ar",p,"ma",q,".txt",sep=""))
}
}

xspec1 = ugarchspec(mean.model = list(armaOrder = c(1,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")


xspec2 = ugarchspec(mean.model = list(armaOrder = c(0,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")

roll1=ugarchroll(xspec1,dd[,4]/100,forecast.length=100,solver = "hybrid",refit.every = 1, refit.window = c("recursive"),
                 calculate.VaR = TRUE, VaR.alpha = c(0.05,.9,.95),keep.coef = TRUE)


roll2=ugarchroll(xspec2,dd[,4]/100,forecast.length=100,solver = "hybrid",refit.every = 1, refit.window = c("recursive"),
                calculate.VaR = TRUE, VaR.alpha = c(0.05,.9,.95),keep.coef = TRUE)


report(roll1, type="fpm")
report(roll2, type="fpm")


report(roll1, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)


pit(roll1)
sum(abs(roll1@forecast$density[,"Mu"]-roll1@forecast$density[,"Realized"]))/sum(roll1@forecast$density[,"Realized"])


fit1=ugarchfit(xspec1,dd[1:266,4],solver = "hybrid")
fit.att=fitted(fit1)
sig.att=sigma(fit1)
sres.att=residuals(fit1)/sig.att

round(coef(fit1),3)
round(fit1@fit$se.coef,3)


sim1=ugarchsim(fit2,n.sim = 266, n.start = 0, m.sim = 5000,startMethod = c("unconditional"))

lower95=as.numeric(quantile(fit2,.025))
upper95=as.numeric(quantile(fit2,.975))


pdf(file ="fit_total.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[1:266,4]),type="b",xlab="Time (week)",ylab="Total report length",ylim=c(0,max(upper95)))
lines(ts(fit.att),col="red",pch=3,type="b")  ###fitted values########
lines(ts(lower95),col="blue",pch=3,type="l")  ###fitted values########
lines(ts(upper95),col="blue",pch=3,type="l")  ###fitted values########
par(op)
dev.off()



pdf(file ="acf_total1.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
acf(as.numeric(sres.att))
par(op)
dev.off()


pdf(file ="acf_att.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
acf(as.numeric(sres.att))
par(op)
dev.off()
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[,2]),type="b",xlab="Time (week)",ylab="Number of attacks")
lines(ts(fit.att),col="red",pch=3,type="b")  ###fitted values########
par(op)

pdf(file ="acf_len.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
acf(as.numeric(sres.len))
par(op)
dev.off()

op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[1:266,2]),type="b",xlab="time (week)",ylab="number of attacks")
lines(ts(fit.att),col="red",pch=3,type="b")  ###fitted values########
par(op)

pdf(file ="fit_att.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[1:266,2]),type="b",xlab="Time (week)",ylab="Number of attacks")
lines(ts(fit.att),col="red",pch=3,type="b")  ###fitted values########
par(op)
dev.off()
? ugarchspec
? ugarchsim



op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[1:266,2]),type="b",xlab="time (week)",ylab="number of attacks")
lines(ts(fit.att),col="red",pch=3,type="b")  ###fitted values########
lines(ts(lower95),col="green",pch=3,type="b")  ###fitted values########
lines(ts(upper95),col="green",pch=3,type="b")  ###fitted values########
par(op)
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[1:266,2]),type="b",xlab="time (week)",ylab="number of attacks")
lines(ts(fit.att),col="red",pch=3,type="b")  ###fitted values########
lines(ts(lower95),col="blue",pch=3,type="l")  ###fitted values########
lines(ts(upper95),col="blue",pch=3,type="l")  ###fitted values########
par(op)
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[1:266,2]),type="b",xlab="time (week)",ylab="number of attacks",ylim=c(0,max(upper95)))
lines(ts(fit.att),col="red",pch=3,type="b")  ###fitted values########
lines(ts(lower95),col="blue",pch=3,type="l")  ###fitted values########
lines(ts(upper95),col="blue",pch=3,type="l")  ###fitted values########
par(op)

pdf(file ="fit_att.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[1:266,2]),type="b",xlab="Time (week)",ylab="Number of attacks",ylim=c(0,max(upper95)))
lines(ts(fit.att),col="red",pch=3,type="b")  ###fitted values########
lines(ts(lower95),col="blue",pch=3,type="l")  ###fitted values########
lines(ts(upper95),col="blue",pch=3,type="l")  ###fitted values########
par(op)
dev.off()

pdf(file ="fit_len.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[1:266,3]),type="b",xlab="Time (week)",ylab="Average report length",ylim=c(0,max(upper95)))
lines(ts(fit.len),col="red",pch=3,type="b")  ###fitted values########
lines(ts(lower95),col="blue",pch=3,type="l")  ###fitted values########
lines(ts(upper95),col="blue",pch=3,type="l")  ###fitted values########
par(op)
dev.off()



xspec = ugarchspec(mean.model = list(armaOrder = c(1,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")
fit2=ugarchfit(xspec,dd[1:266,3],solver = "hybrid")
lower95=as.numeric(quantile(fit2,.025))
upper95=as.numeric(quantile(fit2,.975))
plot(fit2)

xspec1 = ugarchspec(mean.model = list(armaOrder = c(1,1),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")
roll1=ugarchroll(xspec1,dd[,3],forecast.length=100,solver = "hybrid",refit.every = 1, refit.window = c("recursive"),
calculate.VaR = TRUE, VaR.alpha = c(0.05,.1,.2,.3,.4,.5),keep.coef = TRUE)
report(roll1, type="VaR", VaR.alpha = 0.05, conf.level = 0.95)
report(roll1, type="fpm")
pit(roll1)
sum(abs(roll1@forecast$density[,"Mu"]-roll1@forecast$density[,"Realized"]))/sum(roll1@forecast$density[,"Realized"])
xspec1 = ugarchspec(mean.model = list(armaOrder = c(1,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")
 

library(GAS)
roll1@forecast
roll1@forecast$VaR[,1]
ob.att=dd[1:266,4]
var.for=roll1@forecast$VaR[,3] 
#ob.att=roll1@forecast$realized
BacktestVaR(ob.att, var.for,.95, Lags = 4)
BacktestVaR(ob.att, var.for,.05, Lags = 4)


upper95=as.numeric(quantile(fit2,.975))




xspec1 = ugarchspec(mean.model = list(armaOrder = c(1,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")

roll1=ugarchroll(xspec1,dd[,4]/100,forecast.length=100,solver = "hybrid",refit.every = 1, refit.window = c("recursive"),
              calculate.VaR = TRUE, VaR.alpha = c(0.05,.9,.95),keep.coef = TRUE)
pred=roll1@forecast$density$Mu
var.for=roll1@forecast$VaR[,3] 

pdf(file ="pred_total.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[267:366,4]/100,start=267),type="b",xlab="Time (week)",ylab="Total report length (*100)")
lines(ts(as.numeric(pred),start=267),col="red",pch=3,type="b")  ###fitted values########
lines(ts(var.for,start=267),col="blue",pch=3,type="l")  ###fitted values########
par(op)
dev.off()




xspec1 = ugarchspec(mean.model = list(armaOrder = c(0,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")

roll1=ugarchroll(xspec1,dd[,2],forecast.length=100,solver = "hybrid",refit.every = 1, refit.window = c("recursive"),
                 calculate.VaR = TRUE, VaR.alpha = c(0.05,.9,.95),keep.coef = TRUE)
pred=roll1@forecast$density$Mu
var.for=roll1@forecast$VaR[,3] 

pdf(file ="pred_att.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[267:366,2],start=267),type="b",xlab="Time (week)",ylab="Number of attacks")
lines(ts(as.numeric(pred),start=267),col="red",pch=3,type="b")  ###fitted values########
lines(ts(var.for,start=267),col="blue",pch=3,type="l")  ###fitted values########
par(op)
dev.off()



xspec1 = ugarchspec(mean.model = list(armaOrder = c(1,1),arfima=F), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")

roll1=ugarchroll(xspec1,dd[,3],forecast.length=100,solver = "hybrid",refit.every = 1, refit.window = c("recursive"),
                 calculate.VaR = TRUE, VaR.alpha = c(0.05,.9,.95),keep.coef = TRUE)

pred=roll1@forecast$density$Mu
var.for=roll1@forecast$VaR[,3] 

pdf(file ="pred_len.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)
plot(ts(dd[267:366,3],start=267),type="b",xlab="Time (week)",ylab="Average report length")
lines(ts(as.numeric(pred),start=267),col="red",pch=3,type="b")  ###fitted values########
lines(ts(var.for,start=267),col="blue",pch=3,type="l")  ###fitted values########
par(op)
dev.off()













var.for
ob.att
ob.att=dd[267:366,4]/100
BacktestVaR(ob.att, var.for,.05, Lags = 4)
BacktestVaR(ob.att, var.for,.95, Lags = 4)
BacktestVaR(ob.att, var.for,.05, Lags = 4)
report(roll1, type="var")
report(roll1, VaR.alpha = 0.05, conf.level = 0.95)


pred=roll1@forecast$density$Mu
sig.all=roll1@forecast$density$Sigma
skew.all=roll1@forecast$density$Skew
shape.all=roll1@forecast$density$Shape

dmatrix = cbind(as.numeric(pred),as.numeric(sig.all), 
                as.numeric(skew.all),  as.numeric(shape.all))

colnames(dmatrix) = c("mu", "sigma", "skew", "shape")

 
# Transform to Uniform
uvector = apply(cbind(ob.att,dmatrix), 1, FUN = function(x) pdist("sstd", q = x[1],
                                                                mu = x[2], sigma = x[3], skew = x[4], shape = x[5]))

# hist(uvector)
# transform to N(0,1)
nvector = qnorm(uvector)
test1 = BerkowitzTest(data = nvector, lags = 1, significance = 0.05)
test2 = BerkowitzTest(data = nvector, alpha = 0.05, significance = 0.05, 
                      tail.test=TRUE)
test3 = BerkowitzTest(data = nvector, alpha = 0.01, significance = 0.05, 
                      tail.test=TRUE)


PIT_test(uvector)


#########################################################################
dd<-read.csv("I://dropbox//Dropbox/Shouhuai-related/Zheyuan-Research/data/cyber_event_binned_data.csv")
require(vcd)
require(MASS)
dd_att90=dd[dd[,2]>quantile(dd[,2],.7),]

nn=dim(dd_att90)[1]
dd_att90$inter_att=c(0,dd_att90[2:nn,1]-dd_att90[1:(nn-1),1])

attinter90=dd_att90$inter_att[-1]

acf(attinter90,plot=F)
Box.test(attinter90)




fit1 <- fitdistr(attinter90, "weibull") 
ks.test(attinter90, "pexp", fit1$estimate) 


qqexp <-  function(y, line=FALSE, ...) { 
  y <- y[!is.na(y)]
  n <- length(y)
  x <- qexp(c(1:n)/(n+1))
  m <- mean(y)
  if (any(range(y)<0)) stop("Data contains negative values")
  ylim <- c(0,max(y))
  qqplot(x, y, xlab="Exponential plotting position",ylim=ylim,ylab="Ordered sample", ...)
  if (line) abline(0,m,lty=2)
  invisible()
}

library(car)
qqPlot(attinter90,"weibull", fit1$estimate) 



