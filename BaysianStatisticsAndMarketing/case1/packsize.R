#
# -----------------------------------------------------------------------------
#
rpackSize=
function(Data,Prior,Mcmc){
#
# purpose: run packsize model (homothetic version) 
#
# Arguments:
#   Data contains a list of (psdata, nbrd,nsku,brdind)
#      psdata is a list of lists (one list per unit)
#      psdata[[i]]=list(ybrd,ysku,x,price,ounce.avail)
#         ybrd is index of brand chosen, ybrd=1,..,nbrd)
#         ysku is index of sku chosen
#         x is a matrix that is n_i x nsku by nxvar
#         price is a n_i x nsku vector of price
#         ounce is a n_i x nsku vector of ounces of each sku
#         avail is n_i x nsku indicator of availability  = -99 if not avail, else 0
#      nbrd is number of brnds
#      nsku is number of skus
#      brdind is a list of length nbrd
#         each element of brdind is also a list indices corresponding to brand i
#             e.g. for three brands there would be three lists, list one for brd 1...
#   Prior contains a list of (nu,V0,betabarbar,Abeta,taubar,Atau)
#      beta_i ~ N(betabar,Vbeta)
#      betabar ~ N(betabarbar,Abeta^-1)
#      Vbeta ~ IW(nu,V0)
#      tau ~ N(taubar,Atau_1)
#   Mcmc is a list of (sbeta,stau,R,initbudget,keep)
#      sbeta is scale factor for RW increment for beta_is
#      stau is std dev of random walk increment for tau
#      R is number of draws
#      initbudget is initial value for tau (budgetary allotment)
#      keep every keepth draw
#
# Output:
#      a list of betabardraw (R/keep x nxvar), Vbetadraw (R/keep x nxvar**2), 
#         taudraw (R/keep x nunits) array
#         betadraw is a nunits x nxvar x R/keep array of draws of betas
#         nunits=length(psdata)
#
#  define functions needed
#
# ------------------------------------------------------------------------
#
loglike=
function(ysku,ybrd,x,price,t,ounce,avail,brdind,beta){
# function computer log likelihood of data for packsize model
# packdata contains ysku,ybrd,x,price,ounce,avail
# ysku is an n vector of sku choice (1,...,j=90) 
# ybrd is an n vector of brand choice (1,...,3)
# brdind is a list of brand-sku vectors, brdind=list(c(1:31),c(32:63),c(64:90))
# x is nj x k matrix of xvalues for each of j alt on each of n occassions
# price is a nj x 1 vector of prices for each of j alt on each of n occassions
# t is the budgetary allotment
# ounce is a nj x 1 vector of quantities in ounces
# avail is nj x 1 vector indicating availability of brand (0=avail, -99=not)
# beta is vector of coef.: brand dummies (bud, coors), bottle, disp, feat, alpha
# make x have one "blank col" e.g. nh x (k+1)  this is to avoid cbinds with x to add in log(t-price)
#   these can be expensive if x is a large matrix

n=length(ysku)
nsku=length(avail)/n
nbrd=length(brdind)
tmp=t-price
x[,ncol(x)]=log(ifelse(tmp<.001,exp(-99),tmp))
xbeta=x%*%beta + log(ounce)
xbeta=xbeta+avail     # note the different coding for avail is required here
xbeta=matrix(xbeta,byrow=T,ncol=nsku)    # note xbeta is now n x nsku
zbeta=matrix(double(n*nbrd),ncol=nbrd)
for (i in 1:nbrd) {
   zbeta[,i]=apply(matrix(xbeta[,brdind[[i]]],ncol=length(brdind[[i]])),1,max)
}
# now sub in with x'beta for actual brand chosen
bind=cbind(1:n,ybrd)
sind=cbind(1:n,ysku)
zbeta[bind]=xbeta[sind]
zbeta=exp(zbeta)
iota=c(rep(1,nbrd))
denom=log(zbeta%*%iota)
sum(log(zbeta[bind])-denom)

}
#-------------------------------------------------------------------------------

psdata=Data$psdata
nhh=length(psdata)
nbrd=Data$nbrd
nsku=Data$nsku
nxvar=ncol(psdata[[1]]$x)

taubar=Prior$taubar
Atau=Prior$Atau
betabarbar=Prior$betabarbar
Abeta=Prior$Abeta
V0=Prior$V0
nu=Prior$nu

R=Mcmc$R
keep=Mcmc$keep
sbeta=Mcmc$sbeta
stau=Mcmc$stau
initbudget=Mcmc$initbudget


#
# initialize storage for draws
#
Vbetadraw=matrix(double(floor(R/keep)*nxvar*nxvar),ncol=nxvar*nxvar)
betadraw=array(double(floor(R/keep)*nhh*nxvar),dim=c(nhh,nxvar,floor(R/keep)))
betabardraw=matrix(double(floor(R/keep)*nxvar),ncol=nxvar)
taudraw=matrix(double(floor(R/keep)*nhh),ncol=nhh)
oldbetas=matrix(double(nhh*nxvar),ncol=nxvar)
oldtau=rep(initbudget,nhh)
oldVbeta=diag(rep(1,nxvar))
oldVbetai=diag(rep(1,nxvar))
oldbetabar=double(nxvar)

betad = array(0,dim=c(nxvar))
betan = array(0,dim=c(nxvar))
reject = array(0,dim=c(R))
llike=array(0,dim=c(R))
iota=matrix(rep(1,nhh),ncol=1)

itime=proc.time()[3]
cat("MCMC Iteration (est time to end - min)",fill=TRUE)
flush.console()
for (j in 1:R) {
	rej = 0
	logl = 0
	sV = sbeta*oldVbeta
	root=t(chol(sV))

#	Draw B-h|B-bar, V

	for (i in 1:nhh) {

		betad = oldbetas[i,]
		betan = betad + root%*%rnorm(nxvar)
		tbd=oldtau[i]
		tbn=rnorm(1, mean=tbd, sd=stau)
# data		
		lognew = loglike(psdata[[i]]$ysku,psdata[[i]]$ybrd,psdata[[i]]$x,psdata[[i]]$price,
				tbn,psdata[[i]]$ounce,psdata[[i]]$avail,brdind,betan)
		logold = loglike(psdata[[i]]$ysku,psdata[[i]]$ybrd,psdata[[i]]$x,psdata[[i]]$price,
				tbd,psdata[[i]]$ounce,psdata[[i]]$avail,brdind,betad)
# heterogeneity
		logknew = (-.5*(t(betan-oldbetabar) %*% oldVbetai %*% (betan-oldbetabar)))
		logkold = (-.5*(t(betad-oldbetabar) %*% oldVbetai %*% (betad-oldbetabar)))
# budgetary allotment
		lnew = (-.5*Atau*(tbn-taubar)^2)
		lold = (-.5*Atau*(tbd-taubar)^2)
# MH step
		alpha = exp(lognew + logknew + lnew - logold - logkold - lold)
		if(alpha=="NaN") alpha=-1
		u = runif(n=1,min=0, max=1)
		if(u < alpha) { 
			oldbetas[i,] = betan
			oldtau[i] = tbn
			logl = logl + lognew } else {
		 	logl = logl + logold
			rej = rej+1  }
		}
# 	Draw B-bar|B-h,V
#	betabar ~ N(betabarbar,Abeta-1)
		vv=chol2inv(chol(nhh*oldVbetai+Abeta))
		mu=vv%*%(nhh*oldVbetai%*%(t(oldbetas)%*%iota/nhh)+Abeta%*%betabarbar)
		oldbetabar=mu + t(chol(vv))%*%rnorm(nxvar)

#	Draw V|B-h,B-bar
#	Prior is IW(nu,V0)
		S=crossprod(oldbetas-t(array(oldbetabar,dim=c(nxvar,nhh))))
		W=bayesm::rwishart(n = nhh+nu,chol2inv(chol(V0+S)))
		oldVbeta=W$IW
		oldVbetai=W$W
	reject[j] = rej/nhh
	llike[j] = logl

	if((j%%100)==0) 
          {
           ctime=proc.time()[3]
           timetoend=((ctime-itime)/j)*(R-j)
           cat(" ",j," (",round(timetoend/60,1),")",fill=TRUE)
           flush.console() }
	mkeep=j/keep
	if(mkeep*keep == (floor(mkeep)*keep))
          {betabardraw[mkeep,]=oldbetabar
           Vbetadraw[mkeep,]=as.vector(oldVbeta)
           betadraw[,,mkeep]=oldbetas
           taudraw[mkeep,]=oldtau}

}
ctime=proc.time()[3]
cat(" Total Time Elapsed: ",round((ctime-itime)/60,2),fill=TRUE)

list(betadraw=betadraw,Vbetadraw=Vbetadraw,betabardraw=betabardraw,taudraw=taudraw,llike=llike,reject=reject)
}


