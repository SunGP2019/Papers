dat <- read.csv("fpfn_realdat_new.csv")
dat <- signif(dat,3)
nseq <- c(seq(5,35,by=10),47)
# sequence of half-range for random pp,pn
dseq <- c(0.01,0.02,0.03,0.04,seq(0.05, 0.2, by=0.05))
nsample <- 1000

m <- 100000
pi1 <- 0.58579
m1 <- m*pi1
m0 <- m-m1
datdir <- "C:\\Users\\pangdu\\Documents\\shouhuai\\"

nbiaslistdelta <- biaslistdelta <- nrbiaslistdelta <- rbiaslistdelta <- as.list(NULL)
for (delta in dseq) {
# List of bias and RB of naive and adjusted methods
nbiaslist <- biaslist <- nrbiaslist <- rbiaslist <- as.list(NULL)
for (n in nseq) {
  nbiasmat <- biasmat <- ncovmat <- covmat <- as.list(NULL)
  datfile <- paste(datdir,"outapp_n", n, "_delta",100*delta,".R",sep="")
  ## True FPRs and FNRs
  pp <- dat$pp[1:n] 
  pn <- dat$pn[1:n]
  ## True positive and negative predictive values
  qp <- pi1*(1-pn)/(pi1*(1-pn)+(1-pi1)*pp)
  qn <- (1-pi1)*(1-pp)/((1-pi1)*(1-pp)+pi1*pn)
  source(datfile)
  # bias and estimate from naive and adjusted methods
  pi1bias <- apply(epi1mat-pi1,2,mean)
  ppbias <- apply(t(eppmat)-c(pp,pp),1,mean)
  pnbias <- apply(t(epnmat)-c(pn,pn),1,mean)
  qpbias <- apply(t(eqpmat)-c(qp,qp),1,mean)
  qnbias <- apply(t(eqnmat)-c(qn,qn),1,mean)
  nbiaslist[[n]] <- c(pi1bias[1],ppbias[1:n],pnbias[1:n],qpbias[1:n],qnbias[1:n])
  biaslist[[n]] <- c(pi1bias[2],ppbias[(1:n)+n],pnbias[(1:n)+n],qpbias[(1:n)+n],qnbias[(1:n)+n])
  nrbiaslist[[n]] <- signif(abs(nbiaslist[[n]]/c(pi1,pp,pn,qp,qn))*100,5)
  rbiaslist[[n]] <- signif(abs(biaslist[[n]]/c(pi1,pp,pn,qp,qn))*100,5)
}
nbiaslistdelta[[100*delta]] <- nbiaslist
biaslistdelta[[100*delta]] <- biaslist
nrbiaslistdelta[[100*delta]] <- nrbiaslist
rbiaslistdelta[[100*delta]] <- rbiaslist
}
save(dat,nseq,nbiaslistdelta,biaslistdelta,nrbiaslistdelta,rbiaslistdelta,file="outapp_final32_revison.R")



