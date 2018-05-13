xx=read.table("67hhs_xdata.dat",header=FALSE)
yy=read.table("67hhs_ydata.dat",header=FALSE)
hh=levels(factor(xx[,1]))
nhh=length(hh)
psdata=NULL

for (i in 1:nhh) {
	ybrd=yy[yy[,1]==hh[i],2]
	ysku=yy[yy[,1]==hh[i],3]
	x=as.matrix(xx[xx[,1]==hh[i],c(6:11)])
	price=xx[xx[,1]==hh[i],12]
	ounce=xx[xx[,1]==hh[i],13]
	avail=xx[xx[,1]==hh[i],5]
	psdata[[i]]=list(ybrd=ybrd,ysku=ysku,x=x,price=price,ounce=ounce,avail=avail)
		}

cat("Finished Reading data",fill=TRUE)
flush.console()



brdind=list(c(1:5),c(6:10),c(11:14))
nsku=14

# skus included (all in 12 oz):
# Miller lite (1:5)
#  6 pack cans
#  6 pack non-returnable bottle
#  12 pack can
#  12 pack bottle
#  24 pack can
#
# Bud lite (6:10)
#  6 pack cans
#  6 pack non-returnable bottle
#  12 pack can
#  12 pack bottle
#  24 pack can
#
# Coors lite (11:14)
#  6 pack cans
#  6 pack non-returnable bottle
#  12 pack can
#  12 pack bottle

Data=list(psdata=psdata,nbrd=3,nsku=nsku,brdind=brdind)
nxvar=6
nu=nxvar+3
Prior=list(nu=nu,V0=nu*diag(rep(1,nxvar)),betabarbar=as.vector(rep(0,nxvar)),
      Abeta=.01*diag(rep(1,nxvar)),taubar=0,Atau=.01)
Mcmc=list(sbeta=.2,stau=.2,R=30000,keep=1,initbudget=30)

out=rpackSize(Data,Prior,Mcmc)

