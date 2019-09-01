library(Rdonlp2)

n<-200
t<-20


for (jj in 1:100){

kk<-0  #check if greedy algorithm is the best strategy

x1<-round(rnorm(n+t, 100, 20))
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
  x2[(tp+1):(n+t)]<-x1[(tp+1):(n+t)]
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
    y<-x2
  } else if ((sum(s2)==sum(s1)) && (c2<c1)){
    s1<-s2
    c1<-c2
    kk<-100
    y<-x2
  }

k<-min(which(s2!=0))-1
m<-sum(s1)
}

write.table(s1,paste('s',jj,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)
write.table(c1,paste("cost",jj,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)
write.table(y,paste('x2',jj,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)
write.table(kk,paste('kk',jj,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)

}
