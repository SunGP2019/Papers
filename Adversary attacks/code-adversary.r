install.packages("rugarch")
install.packages("forecast")
install.packages("fGarch")
install.packages("TSA")
install.packages("fitdistrplus")
install.packages("tseries")
install.packages("ggplot2")
install.packages("vcd")
install.packages("MASS")
install.packages("actuar")
install.packages("evmix")
install.packages("gtools")
install.packages("devtools")
install.packages("fracdiff")
install.packages("brms")
install.packages("pacman")
install.packages("zoo")
install.packages("stats")
install.packages("texmex")
install.packages("ggformula")
install.packages("dplyr")
install.packages("mosaic")

install.packages("formatR")
install.packages("htmltools")
install.packages("caTools")
install.packages("bitops")
install.packages("base64enc")
install.packages("rprojroot")
install.packages("rmarkdown")
install.packages("evaluate")
install.packages("stringi")

install.packages("car")
install.packages("Rdonlp2", repos="http://R-Forge.R-project.org")


library(rugarch)
library(forecast)
library(fGarch)
library(TSA)
library(fitdistrplus)
library(tseries)
library(ggplot2)
library(vcd)
library(MASS)
library(actuar)
library(evmix)
library(gtools)
library(devtools)
library(fracdiff)
library(brms)
library(pacman)
library(zoo)
library(stats)
library(texmex)
library(ggformula)
library(dplyr)
library(mosaic)

library("formatR")
library("htmltools")
library("caTools")
library("bitops")
library("base64enc")
library("rprojroot")
library("rmarkdown")
library("evaluate")
library("stringi")

library(car)
library(Rdonlp2)


#read file

data.table::fread

readr::read_csv


options(scipen=200)
y<-rep(1,20)
n<-length(y)
x<-c(1:n)
par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=2)
plot(x,y,ylab='Average length of report',xlab='Time',type='l',ylim=c(0,5), lwd=4)
points(x,y,pch=16)
y<-rep(3,20)
n<-length(y)
x<-c(1:n)
lines(x,y,type='l',lwd=4)
points(x,y,pch=16)



***************************************FIT***********************************************


spec = ugarchspec(variance.model = list(model = "sGARCH", 
                                        garchOrder = c(1, 1),
                                        submodel = NULL, 
                                        external.regressors = NULL, 
                                        variance.targeting = FALSE),
                  mean.model     = list(armaOrder = c(4, 0), 
                                        include.mean = TRUE, 
                                        archm = FALSE,
                                        archpow = 1, 
                                        arfima = TRUE, 
                                        external.regressors = NULL, 
                                        archex = FALSE),
                                        distribution.model = "sstd",
                                        start.pars = list(), 
                                        fixed.pars = list())

garch <- ugarchfit(spec = spec, data =x, solver="gosolnp")

xx<-coredata(fitted(garch))


***********************************************PREDICTION*****************************************

spec = ugarchspec(variance.model = list(model = "sGARCH", 
                                        garchOrder = c(1, 1),
                                        submodel = NULL, 
                                        external.regressors = NULL, 
                                        variance.targeting = FALSE),
                  mean.model     = list(armaOrder = c(1, 0), 
                                        include.mean = TRUE, 
                                        archm = FALSE,
                                        archpow = 1, 
                                        arfima = TRUE, 
                                        external.regressors = NULL, 
                                        archex = FALSE),
                                        distribution.model = "sstd",
                                        start.pars = list(), 
                                        fixed.pars = list())

fit = ugarchfit(spec, data = x, out.sample = 100, solver="gosolnp")
pred = ugarchforecast(fit, n.ahead = 1, n.roll = 99)
t<-as.numeric(fitted(pred))

par(mar=c(5,5,1,1),cex.axis=1.5,font.axis=2,font.lab=2,cex.main=2,cex.lab=2)

plot(x[266:366],type='l',xlab='Time', ylab='No. of Attacks', lwd=2)
lines(t,col='red',lwd=2)


^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^OTHERS^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

arfit<-arima(y,order=c(2,0,0))

pre_ar<-predict(arima(y,order=c(2,0,0)),n.ahead=12)

prediction = forecast(arfit,)




^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^optimal attacks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(Rsymphony)

obj <- c(1, 1, 1, 1, 1, -1, -1, -1, -1, -1)

mat<-matrix(c(0,    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
              0,    0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
              0,    0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
              0,    0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
              0,    0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
              0.08, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
              0.01, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 
              0.19, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
              0.22, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
              0.36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),nrow=11)


dir <- c("<=", "==", "==", "==", "==", "==", "<=", "<=", "<=", "<=", "<=")
rhs <- c(95,80,94,195,185,101,80,94,195,185,101)
max <- F
types <- c("I", "I", "I", "I", "I", "I", "I", "I", "I", "I", "I")
Rsymphony_solve_LP(obj, mat, dir, rhs, max = max)



library(Rdonlp2)
p = c(79, 93，194，184，100)    
par.l = c(0,0,100,100,100); par.u = c(80,94,195,185,101)　


fn = function(x){
   (80-x[1])^2+(94-x[2])^2+(195-x[3])^2+(185-x[4])^2+(101-x[5])^2
}


A = matrix(c(0.08,0.01,0.19,0.22,0.36),1,byrow=TRUE)
lin.l = c(0); lin.u = c(96)  

ret = donlp2(p, fn, par.u=par.u, par.l=par.l, A, lin.l=lin.l, lin.u=lin.u)

ret$par



^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^maximum targets^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
library(Rdonlp2)

fn1 = function(x){
  (x2[tp+1]-x[1])+(x2[tp+2]-x[2])+(x2[tp+3]-x[3])+(x2[tp+4]-x[4])+(x2[tp+5]-x[5])
}

fn2 = function(x){
  (x2[tp+1]-x[1])+(x2[tp+2]-x[2])+(x2[tp+3]-x[3])+(x2[tp+4]-x[4])
}

fn3 = function(x){
  (x2[tp+1]-x[1])+(x2[tp+2]-x[2])+(x2[tp+3]-x[3])
}

fn4 = function(x){
  (x2[tp+1]-x[1])+(x2[tp+2]-x[2])
}

fn5 = function(x){
  (x2[tp+1]-x[1])
}

#m1<-c(a5,a4,a3,a2,a1)

#m2<-c(1, a4,a3,a2,a1)

#m3<-c(1, 1, a3,a2,a1)

#m4<-c(1, 1, 1, a2,a1)

#m5<-c(1, 1, 1, 1, a1)

#switch(sum, A<-matrix(m5,1,byrow=TRUE),
#            A<-matrix(m4,1,byrow=TRUE),
#            A<-matrix(m3,1,byrow=TRUE),
#            A<-matrix(m2,1,byrow=TRUE),
#            A<-matrix(m1,1,byrow=TRUE),)


n<-200
t<-20


for (jj in 1:5){

kk<-0  #check if greedy algorithm is the best strategy

x1<-round(rlnorm(n+t, 5, 2))
write.table(x1,paste('x',jj,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)
s1<-rep(0,t)
s2<-rep(0,t)
l<-rep(1,n+t)  #lable manipulated or not
x2<-x1
pointer<-n+1  #pointer
c1<-0 #cost
c2<-0 #cost
d<-5
#estimate the AR model here.

gamma<-0.7 # the coefficient of effectiveness

a1<-0.36
a2<-0.22
a3<-0.19
a4<-0.01
a5<-0.08



while (pointer<=(n+t)) {
  sum<-0
  tp<-pointer-1
  while ((l[tp]>=0) && (pointer-tp<=d)){
  sum<-sum+l[tp]
    tp<-tp-1
  }

  #p = x2[(tp+1): (pointer-1)]   #initial values
  p = x2[(pointer-d): (pointer-1)]

  if (sum==d){
    par.l<-rep(0,d)
  } else{
      par.l = c(x2[(pointer-d): (pointer-sum-1)], rep(0,sum)); 
  }
  par.u = p　#the range of $x't$

  #switch(sum, fn<-fn5, fn<-fn4, fn<-fn3, fn<-fn2, fn<-fn1)

  fn = function(x){
    (x1[pointer-5]-x[1])+(x1[pointer-4]-x[2])+(x1[pointer-3]-x[3])+(x1[pointer-2]-x[4])+(x1[pointer-1]-x[5])
  }

  A = matrix(c(a5,a4,a3,a2,a1),1,byrow=TRUE)

  lin.l = c(gamma*x2[pointer]); lin.u = c(gamma*x2[pointer])  

  ret = donlp2(p, fn, par.u=par.u, par.l=par.l, A, lin.l=lin.l, lin.u=lin.u)

  if (ret$message=="KT-conditions satisfied, no further correction computed" || 
    ret$message=="KT-conditions satisfied, computed correction small"){
    for (i in 1:d){
      x2[(pointer-d-1+i)]<-floor(ret$par[i])
    }
    l[(tp+1):(pointer-1)]<-0
    l[pointer]<--1 #non-manipulable
    s1[(pointer-n)]<-1
    pointer<-pointer+2 #the next prediction is not attackable
  } else{
    pointer<-pointer+1 #jump to the next target
  }

}

c1<-sum(x1-x2)

y<-x2
s2<-s1
m<-sum(s1)
k<-min(which(s2!=0))-1

while ((k<=m) && (k!=Inf)){

  pointer<-tail(which(s2!=0),1) #find the last 1 in s2
  s2[pointer]<-0
  pointer<-pointer+n
  l[pointer]<-1
  tp<-pointer-min((pointer-which(l==-1)))  #find the closest -1 in l as tp
  if (tp==-Inf){
    break;
  }
  l[(tp+1):pointer]<-1
  x2[(tp+1):pointer-1]<-x1[(tp+1):pointer-1]
  pointer<-pointer+1
  c2<-0

  while (pointer<=(n+t)) {
    sum<-0
    tp<-pointer-1
    while ((l[tp]>=0) && (pointer-tp<=d)){ # l[tp] always exits
      sum<-sum+l[tp]
      tp<-tp-1
    }

    p = x2[(pointer-d): (pointer-1)]
    if (sum==d){
      par.l<-rep(0,d)
    } else{
      par.l = c(x2[(pointer-d): (pointer-sum-1)], rep(0,sum)); 
    }
    par.u = p　#the range of $x't$

    fn = function(x){
      (x1[pointer-5]-x[1])+(x1[pointer-4]-x[2])+(x1[pointer-3]-x[3])+(x1[pointer-2]-x[4])+(x1[pointer-1]-x[5])
    }

    A = matrix(c(a5,a4,a3,a2,a1),1,byrow=TRUE)

    lin.l = c(gamma*x2[pointer]); lin.u = c(gamma*x2[pointer])  

    ret = donlp2(p, fn, par.u=par.u, par.l=par.l, A, lin.l=lin.l, lin.u=lin.u)

    if (ret$message=="KT-conditions satisfied, no further correction computed" || 
      ret$message=="KT-conditions satisfied, computed correction small"){
      for (i in 1:d){
        x2[(pointer-d-1+i)]<-floor(ret$par[i])
      }
      l[(tp+1):(pointer-1)]<-0
      l[pointer]<--1 #non-manipulable
      s2[(pointer-n)]<-1
      pointer<-pointer+2 #the next prediction is not attackable
    } else{
      pointer<-pointer+1 #jump to the next target
    }

  }

  c2<-sum(x1-x2)

  if (sum(s2)>sum(s1)){
    s1<-s2
    c1<-c2
    kk<-100
  } else if ((sum(s2)==sum(s1)) && (c2<c1)){
    s1<-s2
    c1<-c2
    kk<-100
  }

k<-min(which(s2!=0))-1
m<-sum(s1)
}

write.table(s1,paste('s',jj,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)
write.table(kk,paste('k',jj,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)

}

