require(fracdiff)
#setwd("C:/Users/zcd896/Dropbox/Sajad's papers/Early Warning Paper/R codes for early warning paper/code/LRD-test-code")
setwd("~/Dropbox/Sajad's papers/Early Warning Paper/R codes for early warning paper/code/LRD-test-code")
source("whitten-aic.R")

#setwd("C:/Users/zcd896/Dropbox/Sajad's papers/Early Warning Paper/Early Warning Paper - Results/March 2013/D1/net 10/Consecutive/D1-60 sec-10-IP 16777216 --- 20971519/txt source files")
#setwd("C:/Users/zcd896/Dropbox/Sajad's papers/Early Warning Paper/Early Warning Paper - Results/March 2013/D1/net 10/Consecutive/D1-120sec-10-IP-16777216---20971519")
#setwd("~/Dropbox/Sajad's papers/Early Warning Paper/Early Warning Paper - Results/March 2013/D1/net 10/Consecutive/D1-60 sec-10-IP 16777216 --- 20971519/txt source files")
#setwd("~/Dropbox/Sajad's papers/Early Warning Paper/Early Warning Paper - Results/March 2013/D2/net 10/Consecutive/D2-60sec-10-IP 16777216 --- 20971519/txt source files")
#setwd("~/Dropbox/Sajad's papers/Early Warning Paper/Early Warning Paper - Results/September 2014/D1/net 10/consecutive/D1-60sec-10-IP-16777216---20971519")
#setwd("~/Dropbox/Sajad's papers/Early Warning Paper/Early Warning Paper - Results/September 2014/D2/net 10/consecutive/D2-60sec-10-IP-16777216---20971519")
#setwd("~/Dropbox/Sajad's papers/Early Warning Paper/Early Warning Paper - Results/October 2014/D1/net 10/consecutive/D1-60sec-10-IP-16777216---20971519")
setwd("~/Dropbox/Sajad's papers/Early Warning Paper/Early Warning Paper - Results/October 2014/D2/net 10/consecutive/D2-60sec-10-IP-16777216---20971519")

file_list <- list.files()

n.file<-length(file_list)
test.s<-mat.or.vec(1, 5)  #vector to store summary statistics: d, w1, w2, significant at 1%: 

i<-0
for(file in file_list){
i<-i+1
y<-read.table(file, header=F, sep="")
n.data<-length(y[,3])
x<-matrix(y[,3], n.data, 1, byrow = TRUE)

x<-data[,2]
x<-log(x +.5)
x<-x-mean(x)
n<-length(x)


x<-filterx(x,n)


#select the truncation
m<-round(n^(0.7))

#the trimming proportions
trm1<-round(0.02*m)
trm2<-round(0.05*m)


freq<-seq(1,n,by=1)*(2*pi/n)
xf<-fft(x)
px<-(Re(xf)^2+Im(xf)^2)/(2*pi*n)
perdx<-px[2:n]             

#local Whittle likelihood
fn<-function(h)
{
lambda<-freq[1:m]^(2*h-1)
Gh=mean(perdx[1:m]*lambda)
Rh=log(Gh)-(2*h-1)*mean(log(freq[1:m]))
return(Rh)
}
#est<-optimize(fn,c(0,1.5),tol = 0.00001)
est<-optimize(fn,c(0,1.5),tol = 0.000001)
hhat<-est$minimum

lambda_hat<-freq[1:m]^(2*hhat-1)
Ghat=mean(perdx[1:m]*lambda_hat[1:m])
stat<-rep(0,m)

#now compute the statistic
comp1<-(perdx[1:m]*lambda_hat[1:m])/Ghat
comp2<-log(freq[1:m])-mean(log(freq[1:m])) 
stat<-cumsum((comp1-1)*comp2)/sqrt(sum(comp2^2))
stat2<-cumsum((comp1-1))/sqrt(sum(comp2^2))

z1<-max(abs(stat[trm1:m]))
z2<-max(abs(stat[trm2:m]))

test.s[i,1]<-hhat
test.s[i,2]<-z1
test.s[i,4]<-z2

if(z1>1.517)
  {test.s[i,3]<-1
  }  ###1 means spurious long memory 
else
  {test.s[i,3]<-0}

if(z2>1.426)
  {test.s[i,5]<-1
  	} else
  {test.s[i,5]<-0}
}

line1="October 2014- D2- after early warning"
write(line1,file="~/Dropbox/Sajad's papers/Early Warning Paper/R codes for early warning paper/Sajad's Output/LRD/LRD_Test_Results.txt",append=TRUE)
write.table(test.s, file="~/Dropbox/Sajad's papers/Early Warning Paper/R codes for early warning paper/Sajad's Output/LRD/LRD_Test_Results.txt",  sep="\t", append=TRUE)
line2="______________________________________________________________________________________\n"
write(line2,file="~/Dropbox/Sajad's papers/Early Warning Paper/R codes for early warning paper/Sajad's Output/LRD/LRD_Test_Results.txt",append=TRUE)

