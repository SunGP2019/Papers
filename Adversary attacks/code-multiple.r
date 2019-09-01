library(Rglpk)  #do not use this package, use Rsymphony

for (ii in 1:100){

s1<-read.table(paste("s",ii,".txt"))
x1<-read.table(paste("x",ii,".txt")) 
x2<-read.table(paste("x2",ii,".txt"))
s1<-s1[,1]
x1<-x1[,1]
x2<-x2[,1]

n<-200
t<-20
d<-5
gamma<-0.7
a1<-0.36
a2<-0.22
a3<-0.19
a4<-0.01
a5<-0.08
label<-1
y<-x2[196:220]

while (label==1){

e<-sum(s1)+1
s<-t(combn(t,e,function(x)replace(numeric(t),x,1)))

label<-0

loop<-nrow(s)
cost<-Inf

for (j in 1:loop){

A<-numeric(0)
rhs<-numeric(0)

e<-sum(s[j,])
mani<-t+d

dir <- rep("==",e)

for (i in 1:t){
	if (s[j,i]==1){
		h<-c(rep(0,i-1),a5,a4,a3,a2,a1,rep(0,20-i+1))
		A<-c(A,h)
		rhs<-c(rhs,(x1[n+i]*gamma))
	}
}

#the constrains for the targets

s2<-c(rep(0,d),s[j,])

for (i in 1:mani){
	h<-c(rep(0,i-1),1,rep(0,24-i+1))
	A<-c(A,h)
	rhs<-c(rhs,x1[n-d+i])
	if (s2[i]==1){
		dir<-c(dir, "==")
	} else {
		dir<-c(dir, "<=")
	}
}

#only reduce xt

obj <- rep(1,25)

mat = matrix(A,e+mani,byrow=TRUE)

types<-rep("I",25)

max <- TRUE


rett<-Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = max)

gc()

mc<-sum(x1[196:220]-rett$solution)

if ((rett$status==0) && (mc<cost)){
	label<-1
	cost<-sum(x1[196:220]-rett$solution)
	y<-rett$solution
	s1<-s[j,]
}

}  # end for loop

#}  # end while

c1<-sum(x1[196:220]-y)

write.table(s1,paste('ms',ii,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)
write.table(c1,paste("mcost",ii,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)
write.table(y,paste('mx2',ii,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)

}  # end for ii


^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^use Rsymphony packages to solve^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

setwd("C:/Users/Administrator/Desktop/paper-adversary/r 3.6.1/norm")

library(Rsymphony)

memory.limit()
memory.limit(3000)

for (ii in 1:100){

s1<-read.table(paste("s",ii,".txt"))
x1<-read.table(paste("x",ii,".txt")) 
x2<-read.table(paste("x2",ii,".txt"))

s1<-s1[,1]
x1<-x1[,1]
x2<-x2[,1]

n<-200
t<-20
d<-5
gamma<-0.7
a1<-0.36
a2<-0.22
a3<-0.19
a4<-0.01
a5<-0.08
label<-1
y<-x2[196:220]

while (label==1){

e<-sum(s1)+1
s<-t(combn(t,e,function(x)replace(numeric(t),x,1)))

label<-0

loop<-nrow(s)
cost<-Inf

for (j in 1:loop){

A<-numeric(0)
rhs<-numeric(0)

e<-sum(s[j,])
mani<-t+d

dir <- rep("==",e)

for (i in 1:t){
	if (s[j,i]==1){
		h<-c(rep(0,i-1),a5,a4,a3,a2,a1,rep(0,20-i+1))
		A<-c(A,h)
		rhs<-c(rhs,(x1[n+i]*gamma))
	}
}

#the constrains for the targets

s2<-c(rep(0,d),s[j,])

for (i in 1:mani){
	h<-c(rep(0,i-1),1,rep(0,24-i+1))
	A<-c(A,h)
	rhs<-c(rhs,x1[n-d+i])
	if (s2[i]==1){
		dir<-c(dir, "==")
	} else {
		dir<-c(dir, "<=")
	}
}

#only reduce xt

obj <- rep(1,25)

mat = matrix(A,e+mani,byrow=TRUE)

types<-rep("I",25)

max <- TRUE

rett<-Rsymphony_solve_LP(obj, mat, dir, rhs, max = max)

mc<-sum(x1[196:220]-rett$solution)

if ((rett$status==0) && (mc<cost)){
	label<-1
	cost<-sum(x1[196:220]-rett$solution)
	y<-floor(rett$solution)
	s1<-s[j,]
}

}  # end for loop

}  # end while

c1<-sum(x1[196:220]-y)

write.table(s1,paste('ms',ii,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)
write.table(c1,paste("mcost",ii,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)
write.table(y,paste('mx2',ii,'.txt'),append=FALSE,row.names = FALSE,  col.names=FALSE)

}  # end for ii