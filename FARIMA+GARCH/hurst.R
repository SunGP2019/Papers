#estimate the hurst effect
library(fArma)

file_list <- list.files()
i<-0
n<-length(file_list)

hurst<-rep(0,n) #R/S method
agv<-rep(0,n) #aggvarFit 
peng<-rep(0,n)#peng
per<-rep(0,n) #periodogram method
box<-rep(0,n) #modifed periodogram
wave<-rep(0,n) #wavelet


for(file in file_list) {
i<-i+1
x<-read.table(file, header=F, sep="")
time<-x[,2]
z<-ts(time)

l1<-rsFit(z)      #R/S method
l2<-aggvarFit(z) #AggvarFit
l3<-pengFit(z)   #peng  variance of residuals
l4<-boxperFit(z) #modiefied periodogram method
l5<-waveletFit(z) #wavelet analysis
l6<-perFit(z)     

hurst[i]<-l1@fit$coefficients[2]
agv[i]<-l2@fit$coefficients[2]
peng[i]<-l3@fit$coefficients[2]
box[i]<-l4@fit$coefficients[2]
wave[i]<-l5@fit$coefficients[2]
per[i]<-l6@fit$coefficients[2]

rm(x)
rm(z)
rm(l1)
rm(l2)
rm(l3)
rm(l4)
rm(l5)
rm(l6)
}


#show the result
cbind(data.frame(R/S=hurst,AGV=agv,Peng=peng,Per=Per,Box=box,wave=wave))


peng<-peng/2
agv<-(agv+2)/2
per<-(2-per)/2
