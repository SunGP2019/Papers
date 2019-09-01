####
library(rugarch)
dd<-read.csv("/Users/mxu2/Dropbox/Shouhuai-related/Zheyuan-Research/data/cyber_event_binned_data.csv")

dd<-read.csv("I://dropbox//Dropbox/Shouhuai-related/Zheyuan-Research/data/cyber_event_binned_data.csv")

setwd("I://dropbox//Dropbox/Shouhuai-related/Zheyuan-Research/output-count/")

setwd("I://dropbox//Dropbox/Shouhuai-related/Zheyuan-Research/Paper-1/")
library(rugarch)

for(p in 0:5){
  for(q in 0:5){
    
    xspec = ugarchspec(mean.model = list(armaOrder = c(p,q),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")
    
    fit1=ugarchfit(xspec,dd[1:266,2],solver = "hybrid")
    
    capture.output(fit1,file=paste("ar",p,"ma",q,".txt",sep=""))
 
    
  }
}


xspec = ugarchspec(mean.model = list(armaOrder = c(0,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")

fit1=ugarchfit(xspec,dd[1:266,2],solver = "hybrid")

lower95=as.numeric(quantile(fit1,.025))
upper95=as.numeric(quantile(fit1,.975))

plot(fit1)

fit.att=fitted(fit1)
sig.att=sigma(fit1)
sres.att=residuals(fit1)/sig.att

obs.att=dd[1:266,2]

plot(ts(obs.att),type="o")
lines(ts(fit.att),col="red")

 
pdf(file ="acf_att.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6) 
acf(as.numeric(sres.att))
par(op)
dev.off()

pdf(file ="fit_att.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)  
plot(ts(dd[1:266,2]),type="b",xlab="time (week)",ylab="number of attacks",ylim=c(0,max(upper95)))
lines(ts(fit.att),col="red",pch=3,type="b")  ###fitted values########
lines(ts(lower95),col="blue",pch=3,type="l")  ###fitted values########
lines(ts(upper95),col="blue",pch=3,type="l")  ###fitted values########
par(op)
dev.off()

######################################################################xspec = ugarchspec(mean.model = list(armaOrder = c(0,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")

xspec = ugarchspec(mean.model = list(armaOrder = c(1,1),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")

fit2=ugarchfit(xspec,dd[1:266,3],solver = "hybrid")

lower95=as.numeric(quantile(fit2,.025))
upper95=as.numeric(quantile(fit2,.975))

plot(fit2)

fit.len=fitted(fit2)
sig.len=sigma(fit2)
sres.len=residuals(fit2)/sig.len

obs.len=dd[1:266,3]

 pdf(file ="acf_len.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6) 
acf(as.numeric(sres.len))
par(op)
dev.off()

pdf(file ="fit_len.pdf")
op<-par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=1.6)  
plot(ts(dd[1:266,3]),type="b",xlab="time (week)",ylab="average report length",ylim=c(0,max(upper95)))
lines(ts(fit.len),col="red",pch=3,type="b")  ###fitted values########
lines(ts(lower95),col="blue",pch=3,type="l")  ###fitted values########
lines(ts(upper95),col="blue",pch=3,type="l")  ###fitted values########
par(op)
dev.off()
#########################################dependence test#########
cor.test(as.numeric(sres.att),as.numeric(sres.len),method = c("pearson", "kendall", "spearman"))

cor.test(as.numeric(sres.att),as.numeric(sres.len),method = c("kendall"))




###########################rolling prediction###############

xspec1 = ugarchspec(mean.model = list(armaOrder = c(0,0),arfima=TRUE), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),distribution.model = "sstd")

roll1=ugarchroll(xspec1,dd[,2],forecast.length=100,solver = "hybrid",refit.every = 1, refit.window = c("recursive"),
                 calculate.VaR = TRUE, VaR.alpha = c(0.05,.1,.2,.3,.4,.5),keep.coef = TRUE)

report(roll1, type="VaR", VaR.alpha = 0.2, conf.level = 0.95)

###########use VaR backtest#######
library(GAS)

var.for=roll1@forecast$VaR[,1]
ob.att=dd[267:366,2]


BacktestVaR(ob.att, var.for,.05, Lags = 4)

report(roll1, VaR.alpha = 0.05, conf.level = 0.95)
report(roll1, type="fpm") 

#pit(roll1)

#show(roll1)
sum(abs(roll1@forecast$density[,"Mu"]-roll1@forecast$density[,"Realized"]))/sum(roll1@forecast$density[,"Realized"])




fit2=ugarchfit(xspec,dd[,3],solver = "hybrid")

r1=as.numeric(residuals(fit1)/sigma(fit1))
r2=as.numeric(residuals(fit2)/sigma(fit2))

cor.test(r1,r2,method="kendall")

cor.test(r1,r2,method="spearman")

ss <-  rdist("sstd",n=5000, mu = 0, sigma = 1,skew=coef(fit1)["skew"],shape=coef(fit1)["shape"]) 


sigmall=as.numeric(sigma(fit1))

est=as.numeric(fitted(fit1))
MAE <- sum(abs(count.data$Counts-est))/length(count.data$Counts)
MAE

sMAPE <- 1/length(count.data$Count) * sum(abs(count.data$Count- est)/(count.data$Count + est))
sMAPE

counts.no.zero <- rbind(eventdata[1:4,], eventdata[6:61,],
                        eventdata[63:74,], eventdata[76:366,])
est.no.zero.weeks <- c(est[1:4], est[6:61],
                       est[63:74], est[76:366])
MAPE <- (1/length(counts.no.zero$event.count)) *
  sum(abs(counts.no.zero$event.count - est.no.zero.weeks)/counts.no.zero$event.count)
MAPE





