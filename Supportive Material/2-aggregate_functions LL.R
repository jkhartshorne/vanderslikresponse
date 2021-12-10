# learningcurves_models.R is a script we've got from Tianhu Chen and Joshua Hartshorne.
# Only part of of functions included here, will be used

smooth2<-function(tempdata,w,cutoff){
	
	# smooth2(somedat[somedat$Eng_little=="monoeng",c("t","elogit")],2,0)
	# tempdata <- somedat[somedat$Eng_little=="monoeng",c("t","elogit")]
	 w = 2
	 cutoff = 10 # WAS INACTIVE AS cutoff=0
	
	colnames(tempdata)<-c("age","resp")
	
	 age = c(min(tempdata$age) : max(tempdata$age)) # Was inactive
	
	binned <- data.frame(
		age = c(min(tempdata$age) : max(tempdata$age)),
		N=0,Nr=0,resp=0,SE=0,ymin=0,ymax=0)
	
	for (a in binned$age){
		binned$N[binned$age==a]<-length(tempdata$age[tempdata$age>(a-w-1) & tempdata$age<(a+w+1)])
		binned$Nr[binned$age==a]<-length(tempdata$age[tempdata$age==a])
		binned$resp[binned$age==a]<-mean(tempdata$resp[tempdata$age>(a-w-1) & tempdata$age<(a+w+1)])
		binned$SE[binned$age==a]<-sd(tempdata$resp[tempdata$age>(a-w-1) & tempdata$age<(a+w+1)])
	}
	binned$SE<-binned$SE/(binned$N^.5)
	binned$ymin<-binned$resp-binned$SE
	binned$ymax<-binned$resp+binned$SE
	#new version of cutoff uses the cutoff to choose continuous ages for which there are at least that many at each age.	
	if (min(binned$N)<cutoff){
#		#first, get rid of low values
		t<-binned[1:15,]
		t<-rbind(t,c(0,0,0,0,0,0,0))
		lowa<-max(t$age[t$N<cutoff])		
		t<-binned[binned$age>lowa,]
		t<-rbind(t,c(1000,0,0,0,0,0,0))
		cuta<-(min(t$age[t$N<cutoff]))
	  binned<-binned[binned$age<cuta & binned$age>lowa,]
	}
#	binned<-binned[binned$N>cutoff,] #old version. 
	return(binned)
}

compress<-function(tofit){
	tofit$N<-1
	tf<-summaryBy(N~Eng_little+te+t+e+elogit,data=tofit,FUN=sum,keep.names=TRUE)
	tf$E<-1
	return(tf)
}
binme_old<-function(somedat,elogit=TRUE, binned){	
	
	somedat<-somedat[somedat$t<=70,] #no one undergoing cognitive decline

	binned_m<-cbind(smooth2(somedat[somedat$Eng_little=="monoeng",c("t","elogit")],2,0),AoFE=0)
	binned_s<-cbind(smooth2(somedat[somedat$Eng_little=="bileng",c("t","elogit")],2,0),AoFE=0)
	binned_m$Eng_little<-"monoeng"
	binned_s$Eng_little<-"bileng"
	binned_non<-binned_s
	for (y in 4:30){assign(paste("binned_f",y,sep=""),cbind(smooth2(somedat[somedat$Eng_little=="little" & somedat$te==y,c("t","elogit")],2,0),AoFE=y))}
	binned_non<-rbind(binned_f4,binned_f5,binned_f6,binned_f7,binned_f8,binned_f9,binned_f10,binned_f11,binned_f12,binned_f13,binned_f14,binned_f15,binned_f16,binned_f17,binned_f18,binned_f19,binned_f20,binned_f21,binned_f22,binned_f23,binned_f24,binned_f25,binned_f26,binned_f27,binned_f28,binned_f29,binned_f30)
	binned_non$Eng_little<-"little"
	
	jump<-3
	for (y in seq(1,30,jump)){assign(paste("binned_i",y,sep=""),cbind(smooth2(somedat[somedat$Eng_little=="lot" & somedat$te>=y & somedat$te<(y+jump),c("t","elogit")],2,0),AoFE=y+(jump-1)/2))}
	binned_imm<-rbind(binned_i1,binned_i4,binned_i7,binned_i10,binned_i13,binned_i16,binned_i19,binned_i22,binned_i25,binned_i28)
	binned_imm$Eng_little<-"lot"

		somebins<-rbind(binned_m, binned_s, binned_non, binned_imm) 
	
	colnames(binned)[8]<-"AoFE"
	binned$keep<-1
	somebins<-merge(somebins,binned[,c("AoFE","age","Eng_little","keep")],by.x=c("AoFE","age","Eng_little"),by.y=c("AoFE","age","Eng_little"))
	colnames(somebins)<-c("te","t","Eng_little","N","Nr","elogit")
	somebins<-somebins[,c("te","t","Eng_little","elogit")]
	somebins$e<-somebins$t-somebins$te
	somebins$E<-1
	somebins$N<-1
		
	somebins$Eng_little <- as.factor(somebins$Eng_little)
	somebins <- somebins[complete.cases(somebins$Eng_little), ]
	somebins$groups <- NULL
	somebins$groups[somebins$Eng_little == "monoeng"] <- "monolinguals"
	somebins$groups[somebins$Eng_little == "lot"] <- "immersion"
	somebins$groups[somebins$Eng_little == "little"] <- "non-immersion"
	somebins$groups[somebins$Eng_little == "bileng"] <- "bilinguals"
	somebins$groups <- as.factor(somebins$groups)
	somebins <- somebins[complete.cases(somebins$groups), ]
	
	somebins$age <- somebins$t
	somebins$ability <- somebins$elogit
	
	return(somebins)
}


# Different sigmoid functions. Only few are used
# Most part of the remaining functions are not used 

exp.flat<-function(t,te=0,E=1,r=.1){
	((1-exp(-E*r*max((t-te),0))))*2+1.5
}
opt.exp.flat<-function(PAR){
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(exp.flat(tf$t[i],tf$te[i],tf$E[i],PAR[1])-tf$elogit[i])^2}))
	# sum(sapply(1:length(tofit$t),function(i){(exp.flat(tofit$t[i],tofit$te[i],tofit$E[i],PAR)-tofit$elogit[i])^2})) #slower and gives same result
}
opt.exp.flat.E<-function(PAR){
	#PAR: c(r, sim.E, imm.E, non.E)
	pop<-c("monoeng","bileng","lot","little")
	disfac<-c(1,PAR[2:4])
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(exp.flat(tf$t[i],tf$te[i],disfac[grep(tf$Eng_little[i],pop)],PAR[1])-tf$elogit[i])^2}))
	# sum(sapply(1:length(tofit$t),function(i){(exp.flat(tofit$t[i],tofit$te[i],disfac[grep(tofit$Eng_little[i],pop)],PAR[1])-tofit$elogit[i])^2})) #slower and gives same result
}

exp.disc<-function(t,te=0,E=1,tc=0,r0,r1){
	val=0
	if(t<=tc & te<=tc){val<-max((1-exp(-E*r0*(max(t-te,0)))),0)}
	if(t>tc & te<=tc){val<-max((exp(-E*r1*(max(t-tc,0)))*((1-exp(-E*r0*(max(tc-te,0))))-1)+1),0)}
	if(t>=te & te>=tc){val<-max((1-exp(-E*r1*(max(t-te,0)))),0)}
	return(val*2+1.5)
}
opt.exp.disc<-function(PAR){
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(exp.disc(tf$t[i],tf$te[i],tf$E[i],PAR[1],PAR[2],PAR[3])-tf$elogit[i])^2}))
	# sum(sapply(1:length(tofit$t),function(i){(exp.disc(tofit$t[i],tofit$te[i],tofit$E[i],PAR[1],PAR[2],PAR[3])-tofit$elogit[i])^2})) #slower and gives same result
}
opt.exp.disc.E<-function(PAR){
	#PAR = c(tc,r0,r1,sim.E,imm.E,non.E)
	pop<-c("monoeng","bileng","lot","little")
	disfac<-c(1,PAR[4:6])
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(exp.disc(tf$t[i],tf$te[i],disfac[grep(tf$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3])-tf$elogit[i])^2}))
	# sum(sapply(1:length(tofit$t),function(i){(exp.disc(tofit$t[i],tofit$te[i],disfac[grep(tofit$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3])-tofit$elogit[i])^2})) #slower and gives same result
}
exp.disc.x<-function(t,te=0,E=1,xc=0,r0,r1){
	val=0
	if(t<=te+xc){val<-1-exp(-E*r0*(t-te))}
	if(t>te+xc){val<-((1-exp(-E*r0*xc))-1)*exp(-E*r1*(t-te-xc))+1}
	return(val*2+1.5)
}
opt.exp.disc.x<-function(PAR){
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(exp.disc.x(tf$t[i],tf$te[i],tf$E[i],PAR[1],PAR[2],PAR[3])-tf$elogit[i])^2}))
	# sum(sapply(1:length(tofit$t),function(i){(exp.disc.x(tofit$t[i],tofit$te[i],tofit$E[i],PAR[1],PAR[2],PAR[3])-tofit$elogit[i])^2})) #slower and gives same result
}
opt.exp.disc.E.x<-function(PAR){
	#PAR = c(xc,r0,r1,sim.E,imm.E,non.E)
	pop<-c("monoeng","bileng","lot","little")
	disfac<-c(1,PAR[4:6])
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(exp.disc.x(tf$t[i],tf$te[i],disfac[grep(tf$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3])-tf$elogit[i])^2}))
	# sum(sapply(1:length(tofit$t),function(i){(exp.disc.x(tofit$t[i],tofit$te[i],disfac[grep(tofit$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3])-tofit$elogit[i])^2})) #slower and gives same result
}

exp.cont<-function(t,te=0,E=1,tc=0,r0=.5,alpha=0,d=0){
	val=0
	if(t<=tc & te<=tc){val<-max((1-exp(-E*r0*max(t-te,0))),0)}
	if(t>tc & te<=tc){val<-max((1-exp(-E*r0*(tc-te))-1)*exp(-E*r0*((t-tc)+(1/alpha)*log((1+exp(-alpha*d))/(1+exp(alpha*(t-tc-d))))))+1,0)}
	if(t>=te & te>=tc){val<-max(1-exp(-E*r0*((t-te)+(1/alpha)*log((1+exp(alpha*(te-tc-d)))/(1+exp(alpha*(t-tc-d)))))),0)}
	return(val*2+1.5)
}
opt.exp.cont<-function(PAR){
	#PAR=c(tc,r0,alpha,d)
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(exp.cont(tf$t[i],tf$te[i],tf$E[i],PAR[1],PAR[2],PAR[3],PAR[4])-tf$elogit[i])^2}))
	# sum(sapply(1:length(tofit$t),function(i){(exp.cont(tofit$t[i],tofit$te[i],tofit$E[i],PAR[1],PAR[2],PAR[3],PAR[4])-tofit$elogit[i])^2})) #slower and gives same result
}
opt.exp.cont.E<-function(PAR){
	#PAR=c(tc,r0,alpha,d,sim.E,imm.E,non.E)
	pop<-c("monoeng","bileng","lot","little")
	disfac<-c(1,PAR[5:7])
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(exp.cont(tf$t[i],tf$te[i],disfac[grep(tf$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3],PAR[4])-tf$elogit[i])^2}))
	# sum(sapply(1:length(tofit$t),function(i){(exp.cont(tofit$t[i],tofit$te[i],disfac[grep(tofit$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3],PAR[4])-tofit$elogit[i])^2})) #slower and gives same result
}
exp.cont.x<-function(t,te=0,E=1,xc=0,r0=.5,alpha=0,d=0){
	val=0
	if(t<=te+xc){val<-1-exp(-E*r0*(t-te))}
	if(t>te+xc){val<-((1-exp(-E*r0*xc))-1)*exp(-E*r0*(t-te-xc+(1/alpha)*log((1+exp(-alpha*d))/(1+exp(alpha*(t-te-xc-d))))))+1}
	return(val*2+1.5)
}
opt.exp.cont.x<-function(PAR){
	#PAR=c(xc,r0,alpha,d)
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(exp.cont.x(tf$t[i],tf$te[i],tf$E[i],PAR[1],PAR[2],PAR[3],PAR[4])-tf$elogit[i])^2}))
	# sum(sapply(1:length(tofit$t),function(i){(exp.cont.x(tofit$t[i],tofit$te[i],tofit$E[i],PAR[1],PAR[2],PAR[3],PAR[4])-tofit$elogit[i])^2})) #slower and gives same result
}
opt.exp.cont.E.x<-function(PAR){
	#PAR=c(xc,r0,alpha,d,sim.E,imm.E,non.E)
	pop<-c("monoeng","bileng","lot","little")
	disfac<-c(1,PAR[5:7])
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(exp.cont.x(tf$t[i],tf$te[i],disfac[grep(tf$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3],PAR[4])-tf$elogit[i])^2}))
	# sum(sapply(1:length(tofit$t),function(i){(exp.cont.x(tofit$t[i],tofit$te[i],disfac[grep(tofit$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3],PAR[4])-tofit$elogit[i])^2})) #slower and gives same result
}


pow.flat<-function(t,te,E,r){
	(1-(t-te+1)^(E*-r))*2+1.5
}
opt.pow.flat<-function(PAR){
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(pow.flat(tf$t[i],tf$te[i],tf$E[i],PAR[1])-tf$elogit[i])^2}))
}
opt.pow.flat.E<-function(PAR){
	#PAR: c(r, sim.E, imm.E, non.E)
	pop<-c("monoeng","bileng","lot","little")
	disfac<-c(1,PAR[2:4])
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(pow.flat(tf$t[i],tf$te[i],disfac[grep(tf$Eng_little[i],pop)],PAR[1])-tf$elogit[i])^2}))
}

pow.disc<-function(t,te=0,E=1,tc=0,r,p){
	val=0
	if(t<=tc & te<=tc){val<-(1-(t-te+1)^(E*-r))}
	if(t>tc & te<=tc){val<-1-((tc-te+1)^(E*-r)+p*((t-te+1)^(-E*r)-(tc-te+1)^(-E*r)))}
	if(t>=te & te>tc){val<-1-p*((t-te+1)^(-E*r))}
	return(val*2+1.5)
}
opt.pow.disc<-function(PAR){
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(pow.disc(tf$t[i],tf$te[i],tf$E[i],PAR[1],PAR[2],PAR[3])-tf$elogit[i])^2}))
}
opt.pow.disc.E<-function(PAR){
	#PAR=c(tc,r,p,sim.E,imm.E,non.E)
	pop<-c("monoeng","bileng","lot","little")
	disfac<-c(1,PAR[4:6])
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(pow.disc(tf$t[i],tf$te[i],disfac[grep(tf$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3])-tf$elogit[i])^2}))
}
pow.disc.x<-function(t,te=0,E=1,xc=0,r,p){
	val=0
	if(t<=te+xc){val<-(1-(t-te+1)^(E*-r))}
	if(t>te+xc){val<-1-((xc+1)^(E*-r)+p*((t-te+1)^(-E*r)-(xc+1)^(-E*r)))}
	return(val*2+1.5)
}
opt.pow.disc.E.x<-function(PAR){
	#PAR=c(xc,r,p,sim.E,imm.E,non.E)
	pop<-c("monoeng","bileng","lot","little")
	disfac<-c(1,PAR[4:6])
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(pow.disc.x(tf$t[i],tf$te[i],disfac[grep(tf$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3])-tf$elogit[i])^2}))
}

pow.cont<-function(t,te=0,E=1,tc=0,r,alpha,d){
	val=0
	if(t<=tc & te<=tc){val<-1-(t-te+1)^(-E*r)}
	if(t>tc & te<=tc){
		val<-(tc-te+1)^(-E*r)
		for (n in (tc+1):t){
			p<-1-1/(1+exp(-alpha*(n-tc-d)))
			# print(p)
			val<-val+p*((n-te+1)^(-E*r)-((n-1)-te+1)^(-E*r))
		}
		val<-1-val
	}
	if(t>=te & te>tc){
		for (n in (te+1):t){
			p<-1-1/(1+exp(-alpha*(n-tc-d)))
			# print(p)
			val<-val+p*(n-te+1)^(-E*r)
		}
		val<-1-val
	}
	return(val*2+1.5)
}
opt.pow.cont<-function(PAR){
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(pow.cont(tf$t[i],tf$te[i],tf$E[i],PAR[1],PAR[2],PAR[3],PAR[4])-tf$elogit[i])^2}))
}
opt.pow.cont.E<-function(PAR){
	pop<-c("monoeng","bileng","lot","little")
	disfac<-c(1,PAR[5:7])
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(pow.cont(tf$t[i],tf$te[i],disfac[grep(tf$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3],PAR[4])-tf$elogit[i])^2}))
}
pow.cont.x<-function(t,te=0,E=1,xc=0,r,alpha,d){
	val=0
	if(t<=te+xc){val<-1-(t-te+1)^(-E*r)}
	if(t>te+xc){
		val<-(xc+1)^(-E*r)
		for (n in (xc+te+1):t){
			p<-1-1/(1+exp(-alpha*(n-(xc+te)-d)))
			# print(p)
			val<-val+p*((n-te+1)^(-E*r)-((n-1)-te+1)^(-E*r))
		}
		val<-1-val
	}
	return(val*2+1.5)
}
opt.pow.cont.E.x<-function(PAR){
	pop<-c("monoeng","bileng","lot","little")
	disfac<-c(1,PAR[5:7])
	sum(sapply(1:length(tf$t),function(i){tf$N[i]*(pow.cont.x(tf$t[i],tf$te[i],disfac[grep(tf$Eng_little[i],pop)],PAR[1],PAR[2],PAR[3],PAR[4])-tf$elogit[i])^2}))
}

# SUPERFLUOUS. We use 1x-Analyses XX continu and discontinu instead
#GetR2
shmccv<-function(n,bptype){
	#split-half monte carlo cross-validation
	#n is the number of times to run cross-validate	
	#SStot is the total sum of squares. used to calculate R^2
	#bptype is the type of breakpoint we're using
	
	tofit.back<-tofit
	tofit.back$test<-0 #test item or not?
	R2<-rep(0,n) #store R^2 results
	SStot<-sum((tofit$elogit-mean(tofit$elogit))^2)
	fitfunc<-switch(bptype,
		"exp.flat" = opt.exp.flat,
		"exp.flat.E" = opt.exp.flat.E,
		"exp.disc" = opt.exp.disc,
		"exp.disc.E" = opt.exp.disc.E,
		"exp.disc.E.x" = opt.exp.disc.E.x,
		"exp.cont" = opt.exp.cont,
		"exp.cont.E" = opt.exp.cont.E,
		"exp.cont.E.x" = opt.exp.cont.E.x)
	guesses<-switch(bptype,
		"exp.flat" = c(.5),
		"exp.flat.E" = c(.5,.5,.5,.5),
		"exp.disc" = c(20,.5,.5),
		"exp.disc.E" = c(20,.5,.5,.5,.5,.5),
		"exp.disc.E.x" = c(20,.5,.5,.5,.5,.5),
		"exp.cont" = c(20,.5,.5,0),
		"exp.cont.E" = c(20,.5,.5,0,.5,.5,.5),
		"exp.cont.E.x" = c(20,.5,.5,0,.5,.5,.5))

	for (i in 1:n){
		# ensuring an even split is too slow
		tofit.back$test<-rbinom(length(tofit.back$test),1,.5)
		assign("tofit",tofit.back[tofit.back$test==0,],envir=.GlobalEnv) #use these for training
		if (bptype=="exp.flat"){
			PAR<-optim(c(.5),upper=1,lower=0,fitfunc,method="Brent")$par
		}else{
			PAR<-optim(guesses,fitfunc,method="Nelder-Mead")$par
		}
		assign("tofit",tofit.back[tofit.back$test==1,],envir=.GlobalEnv) #use these for test
		R2[i]=1-fitfunc(PAR)/SStot
	}
	assign("tofit",tofit.back[,1:(length(colnames(tofit.back))-1)],envir=.GlobalEnv)
	return(R2)
}

# SUPERFLUOUS. WE USE 3-learningcurves 
DEshmccv<-function(n,bptype, itermax){
	#split-half monte carlo cross-validation
	#n is the number of times to run cross-validate	
	#SStot is the total sum of squares. used to calculate R^2
	#bptype is the type of breakpoint we're using
	
	print("starting")
	
	# tofit.back<-tofit
	# tofit.back$test<-0 #test item or not?
	PARs<-cbind(
		rep(0,n),
		rep(0,n),
		rep(0,n),
		rep(0,n)) #store R^2 results

	R2<-rep(0,n) #store R^2 results
	fitfunc<-switch(bptype,
		"exp.flat" = opt.exp.flat,
		"exp.flat.E" = opt.exp.flat.E,
		"exp.disc" = opt.exp.disc,
		"exp.disc.E" = opt.exp.disc.E,
		"exp.disc.E.x" = opt.exp.disc.E.x,
		"exp.cont" = opt.exp.cont,
		"exp.cont.E" = opt.exp.cont.E,
		"exp.cont.E.x" = opt.exp.cont.E.x,
		"pow.flat" = opt.pow.flat,
		"pow.flat.E" = opt.pow.flat.E,
		"pow.disc" = opt.pow.disc,
		"pow.disc.E" = opt.pow.disc.E,
		"pow.disc.E.x" = opt.pow.disc.E.x,
		"pow.cont" = opt.pow.cont,
		"pow.cont.E" = opt.pow.cont.E,
		"pow.cont.E.x" = opt.pow.cont.E.x)
    basefunc<-switch(bptype,
         "exp.flat" = exp.flat,
         "exp.flat.E" = exp.flat,
         "exp.disc" = exp.disc,
         "exp.disc.E" = exp.disc,
         "exp.disc.E.x" = exp.disc.x,
         "exp.cont" = exp.cont,
         "exp.cont.E" = exp.cont,
         "exp.cont.E.x" = exp.cont.x,
         "pow.flat" = pow.flat,
         "pow.flat.E" = pow.flat,
         "pow.disc" = pow.disc,
         "pow.disc.E" = pow.disc,
         "pow.disc.E.x" = pow.disc.x,
         "pow.cont" = pow.cont,
         "pow.cont.E" = pow.cont,
         "pow.cont.E.x" = pow.cont.x)
	lowers<-switch(bptype,
		"exp.flat" = c(0),
		"exp.flat.E" = c(0,0,0,0),
		"exp.disc" = c(1,0,0),
		"exp.disc.E" = c(1,0,0,0,0,0),
		"exp.disc.E.x" = c(1,0,0,0,0,0),
		"exp.cont" = c(1,0,0,-50),
		"exp.cont.E" = c(1,0,0,-50,0,0,0),
		"exp.cont.E.x" = c(1,0,0,-50,0,0,0),
		"pow.flat" = c(0),
		"pow.flat.E" = c(0,0,0,0),
		"pow.disc" = c(1,0,0),
		"pow.disc.E" = c(1,0,0,0,0,0),
		"pow.disc.E.x" = c(1,0,0,0,0,0),
		"pow.cont" = c(1,0,0,-50),
		"pow.cont.E" = c(1,0,0,-50,0,0,0),
		"pow.cont.E.x" = c(1,0,0,-50,0,0,0))
	uppers<-switch(bptype,
		"exp.flat" = c(1),
		"exp.flat.E" = c(1,1,1,1),
		"exp.disc" = c(40,1,1),
		"exp.disc.E" = c(40,1,1,1,1,1),
		"exp.disc.E.x" = c(40,1,1,1,1,1),
		"exp.cont" = c(40,1,1,50),
		"exp.cont.E" = c(40,1,1,50,1,1,1),
		"exp.cont.E.x" = c(40,1,1,50,1,1,1),
		"pow.flat" = c(100),
		"pow.flat.E" = c(100,1,1,1),
		"pow.disc" = c(40,100,100),
		"pow.disc.E" = c(40,100,100,1,1,1),
		"pow.disc.E.x" = c(40,100,100,1,1,1),
		"pow.cont" = c(40,100,1,50),
		"pow.cont.E" = c(40,100,1,50,1,1,1),
		"pow.cont.E.x" = c(40,100,1,50,1,1,1))

	for (i in 1:n){
		# ensuring an even split is too slow
		tofit$test<-rbinom(length(tofit$t),1,.5)
		tf<-binme(tofit[tofit$test==0,])
		# tf<-compress(tofit[tofit$test==0,])
		tf$N=1
		assign("tf",tf,envir=.GlobalEnv) #use these for training
		PAR<-c(DEoptim(fitfunc,lower=lowers,upper=uppers,DEoptim.control(VTR=.01,trace=TRUE,itermax=itermax,parallelType=0,parVar=c("tf","basefunc")))[[1]]$bestmem)
		tf<-binme(tofit[tofit$test==1,])
		tf$N=1
		assign("tf",tf,envir=.GlobalEnv) #use these for training
		SStot<-sum((tf$elogit-mean(tf$elogit))^2)
		R2[i]=1-fitfunc(PAR)/SStot
		PARs[i, ] <- PAR
	}
	# return(R2)
	return(list(R2, PARs))
}

################################################################################################################################################################
### Plotting
############ ALL SUPERFLUOUS. We use "4-Plots learning_curves" instead####################################################################################################################################################

library("grid")
plotfits<-function(type,PAR,Es,logodds=TRUE,name="fig4",toplot){
    ## type = one of several models, see below
    ## PAR = vector of up to 4 parameters (depends on model type)
    ## Es = Value of E for native bilinguals, immigrants, and foreigners, in that order
    ## logodds = use log-ods?
    ## toplot = Pre-binned data to use to create bins for graphs
	colnames(toplot)[4]<-"score"
	toplot$E<-1
	toplot$E[toplot$Eng_little=="bileng"]<-Es[1]
	toplot$E[toplot$Eng_little=="little"]<-Es[3]
	toplot$E[toplot$Eng_little=="lot"]<-Es[2]
	if (type=="exp.flat"){for (i in 1:length(toplot$score)){toplot$score[i]<-exp.flat(toplot$age[i],toplot$te[i],toplot$E[i],PAR)}}
	if (type=="exp.disc"){for (i in 1:length(toplot$score)){toplot$score[i]<-exp.disc(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3])}}
	if (type=="exp.cont"){for (i in 1:length(toplot$score)){toplot$score[i]<-exp.cont(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3],PAR[4])}}
	if (type=="exp.disc.x"){for (i in 1:length(toplot$score)){toplot$score[i]<-exp.disc.x(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3])}}
	if (type=="exp.cont.x"){for (i in 1:length(toplot$score)){toplot$score[i]<-exp.cont.x(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3],PAR[4])}}
	if (type=="pow.flat"){for (i in 1:length(toplot$score)){toplot$score[i]<-pow.flat(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2])}}
	if (type=="pow.disc"){for (i in 1:length(toplot$score)){toplot$score[i]<-pow.disc(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3],PAR[4])}}
	if (type=="pow.cont"){for (i in 1:length(toplot$score)){toplot$score[i]<-pow.cont(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3],PAR[4])}}
	if (type=="pow.disc.x"){for (i in 1:length(toplot$score)){toplot$score[i]<-pow.disc.x(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3])}}
	if (type=="pow.cont.x"){for (i in 1:length(toplot$score)){toplot$score[i]<-pow.cont.x(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3],PAR[4])}}
	toplot$te<-as.factor(toplot$te)
	toplot$fac<-as.factor(paste(toplot$Eng_little,toplot$te,sep=""))
	
	p<-ggplot(data=toplot[toplot$Eng_little!="little",],aes(x=age,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1)+scale_color_manual(values=c("#D55E00","#009E73","#009E73","#009E73","#D55E00","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","red"))
	# p<-p+annotate("text",x=min(toplot$age[toplot$Eng_little=="monoeng"]),y=toplot$score[toplot$Eng_little=="monoeng" & toplot$age==min(toplot$age[toplot$Eng_little=="monoeng"])],label="m",color="black",size=3.5)
	# p<-p+annotate("text",x=min(toplot$age[toplot$Eng_little=="bileng"]),y=toplot$score[toplot$Eng_little=="bileng" & toplot$age==min(toplot$age[toplot$Eng_little=="bileng"])],label="0",color="black",size=3.5)
	# for (i in seq(2,29,3)){
		# jitter<-0
		# if (type=="exp.flat"){
			# numage<-min(toplot$age[toplot$Eng_little=="lot" & toplot$te==i])
		# }
		# p<-p+annotate("text",x=numage,y=toplot$score[toplot$Eng_little=="lot" & toplot$te==i & (toplot$age==numage)],label=as.character(i),color="black",size=3.5)
	# }
	p<-p+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="age")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		p<-p+scale_y_continuous(lim=c(1,4))
	}else{
		p<-p+scale_y_continuous(lim=c(.75,1))
	}
	p<-p+scale_x_continuous(lim=c(0,71)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
	
	p2<-ggplot(data=toplot[toplot$Eng_little=="little",],aes(x=age,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1)+scale_color_manual(values=c("#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00"))
	# for (i in seq(4,30,1)){
		# jitter<-0
		# if (i==5 | i==8){jitter<-11}
		# if (i==9){jitter<-16}
		# if (i==11){jitter<-5}
		# if (i==15){jitter<-4}
		# if (i==22){jitter<-14}
		# if (i==24){jitter<-5}
		# if (i==26){jitter<-11}
		# if (i==29){jitter<-3}
		# p2<-p2+annotate("text",x=max(toplot$age[toplot$Eng_little=="little" & toplot$te==i])-jitter,y=toplot$score[toplot$Eng_little=="little" & toplot$te==i & (toplot$age==max(toplot$age[toplot$Eng_little=="little" & toplot$te==i])-jitter)],label=as.character(i),color="black",size=3.5)
	# }
	p2<-p2+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="age")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		p2<-p2+scale_y_continuous(lim=c(1,4))
	}else{
		p2<-p2+scale_y_continuous(lim=c(.75,1))
	}
	p2<-p2+scale_x_continuous(lim=c(0,71)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

	q<-ggplot(data=toplot[toplot$Eng_little!="little",],aes(x=experience,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1) + scale_color_manual(values=c("#D55E00","#009E73","#009E73","#009E73","#D55E00","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","red"))
	# q<-q+annotate("text",x=70,y=toplot$score[toplot$Eng_little=="monoeng" & toplot$experience==70],label="m",color="black",size=3.5)
	# q<-q+annotate("text",x=70,y=toplot$score[toplot$Eng_little=="bileng" & toplot$experience==70],label="0",color="black",size=3.5)
	# for (i in seq(2,29,3)){
		# jitter<-0
		# if (i==23){jitter<-6}
		# if (i==20){jitter<-4}
		# q<-q+annotate("text",x=max(toplot$experience[toplot$Eng_little=="lot" & toplot$te==i])-jitter,y=toplot$score[toplot$Eng_little=="lot" & toplot$te==i & (toplot$experience==max(toplot$experience[toplot$Eng_little=="lot" & toplot$te==i])-jitter)],label=as.character(i),color="black",size=3.5)
	# }
	q<-q+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="years of experience")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		q<-q+scale_y_continuous(lim=c(1,4))
	}else{
		q<-q+scale_y_continuous(lim=c(.75,1))
	}
	q<-q+scale_x_continuous(lim=c(0,71)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
	
	q2<-ggplot(data=toplot[toplot$Eng_little=="little",],aes(x=experience,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1)+scale_color_manual(values=c("#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00"))
	# for (i in seq(4,30,1)){
		# jitter<-0
		# if (i==5 | i==8){jitter<-11}
		# if (i==9){jitter<-16}
		# if (i==11){jitter<-6}
		# if (i==12){jitter<-20}
		# if (i==22){jitter<-8}
		# if (i==26){jitter<-10}
		# q2<-q2+annotate("text",x=max(toplot$experience[toplot$Eng_little=="little" & toplot$te==i])-jitter,y=toplot$score[toplot$Eng_little=="little" & toplot$te==i & (toplot$experience==max(toplot$experience[toplot$Eng_little=="little" & toplot$te==i])-jitter)],label=as.character(i),color="black",size=3.5)
	# }
	q2<-q2+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="years of experience")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		q2<-q2+scale_y_continuous(lim=c(1,4))
	}else{
		q2<-q2+scale_y_continuous(lim=c(.75,1))
	}
	q2<-q2+scale_x_continuous(lim=c(0,71)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
	
	if (type=="exp.flat"){rate=data.frame(age=seq(1,70,.1),rate=PAR)}
	if (type=="exp.disc" | type=="exp.disc.x"){rate=data.frame(age=seq(1,70,.1),rate=c(rep(PAR[2],round(PAR[1])),rep(PAR[3],70-round(PAR[1]))))}
	if (type=="exp.cont" | type=="exp.cont.x"){
		rate=data.frame(age=seq(1,70,.1),rate=c(rep(PAR[2],691)))
		rate$rate[rate$age>PAR[1]]<-sapply(rate$age[rate$age>PAR[1]],function(x){PAR[2]*(1-1/(1+exp(-PAR[3]*(x-PAR[1]-PAR[4]))))})
		}
	if (type=="pow.flat"){rate=data.frame(age=seq(1,70,1),rate=PAR[2])}
	if (type=="pow.disc" | type=="pow.disc.x"){rate=data.frame(age=seq(1,70,1),rate=c(rep(PAR[2],round(PAR[1])),rep(PAR[3],70-round(PAR[1]))))}
	if (type=="pow.cont" | type=="pow.cont.x"){
		rate=data.frame(age=seq(1,70,1),rate=c(rep(PAR[2],70)))
		rate$rate[rate$age>PAR[1]]<-sapply(rate$age[rate$age>PAR[1]],function(x){PAR[2]*(1-1/(1+exp(-PAR[3]*(x-PAR[1]-PAR[4]))))})
		}
	r<-ggplot(data=rate,aes(x=age,y=rate))+geom_line(size=1.5)
	if (type=="exp.disc.x" | type=="exp.cont.x" | type=="pow.disc.x" | type=="pow.cont.x"){
		r<-r+scale_x_discrete(name="yrs experience",breaks=seq(0,70,10))
	}
    # new version plots alternatives in background
	rate$trajectory<-100
	temp<-rate
	trajs<-list(c(0,.2,.09,.18),c(5,.20,.09,.18),c(25,.20,.09,.18),c(40,.20,.09,10),c(40,.20,.5,50),c(0,.21,.3,15),c(1,.15,.5,50),c(40,.13,.5,50),c(5,.5,.15,50),c(5,.5,.05,-50),c(40,.33,.5,50),c(1,.4,.09,50),c(40,.45,.5,50),c(38,.43,.15,45),c(30,.28,0,-50),c(8,.35,0,-50),c(3,.4,.01,-25),c(20,.47,.2,-20),c(1,.47,.07,50))
	for (i in 1:length(trajs)){
		PAR<-trajs[[i]]
		temp$rate<-c(rep(PAR[2],691))
		temp$rate[temp$age>PAR[1]]<-sapply(temp$age[temp$age>PAR[1]],function(x){PAR[2]*(1-1/(1+exp(-PAR[3]*(x-PAR[1]-PAR[4]))))})
		temp$trajectory<-i
		rate<-rbind(rate,temp)
	}
	rate$trajectory<-as.factor(rate$trajectory)
	r<-ggplot(data=rate,aes(x=age,y=rate,colour=trajectory))+geom_line(size=1.5) + scale_color_manual(values=c(rep("light grey",19),"black"))
	r <- r + theme(axis.text = element_text(color="black",size=16))
	r <- r + xlab("current age") + ylab("rate")
	r <- r + theme(axis.title.y = element_text(size=15))
	r <- r + theme(axis.title.x = element_text(size=15))	
	r <- r + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")

	pdf(file=paste(name,"all",".pdf",sep="_"),w=8,h=8)
	vplayout <- function(x,y) viewport(layout.pos.row=x, layout.pos.col=y)
	pushViewport(viewport(layout=grid.layout(3,2,heights=c(1,1,.5),widths=c(.5,.5))))
	print(p,vp=vplayout(1,1))
	print(q,vp=vplayout(2,1))
	print(p2,vp=vplayout(1,2))
	print(q2,vp=vplayout(2,2))
	print(r,vp=vplayout(3,2))
	dev.off()

	pdf(file=paste(name,"main",".pdf",sep="_"),w=7,h=7)
	vplayout <- function(x,y) viewport(layout.pos.row=x, layout.pos.col=y)
	pushViewport(viewport(layout=grid.layout(2,2,heights=c(1,1),widths=c(.5,.5))))
	print(p,vp=vplayout(1,1))
	print(q,vp=vplayout(2,1))
	print(p2,vp=vplayout(1,2))
	print(q2,vp=vplayout(2,2))
	dev.off()
	
	pdf(file=paste(name,"rate",".pdf",sep="_"),w=5.75,h=2.8)
	print(r)
	dev.off()
}


plotfits2<-function(type,PAR,Es,logodds=TRUE,name="fig4",toplot){
	## only difference from plotfits1 should be that r is plotted with example curves
    ## type = one of several models, see below
    ## PAR = vector of up to 4 parameters (depends on model type)
    ## Es = Value of E for native bilinguals, immigrants, and foreigners, in that order
    ## logodds = use log-ods?
    ## toplot = Pre-binned data to use to create bins for graphs
	colnames(toplot)[4]<-"score"
	toplot$E<-1
	toplot$E[toplot$Eng_little=="bileng"]<-Es[1]
	toplot$E[toplot$Eng_little=="little"]<-Es[3]
	toplot$E[toplot$Eng_little=="lot"]<-Es[2]
	if (type=="exp.flat"){for (i in 1:length(toplot$score)){toplot$score[i]<-exp.flat(toplot$age[i],toplot$te[i],toplot$E[i],PAR)}}
	if (type=="exp.disc"){for (i in 1:length(toplot$score)){toplot$score[i]<-exp.disc(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3])}}
	if (type=="exp.cont"){for (i in 1:length(toplot$score)){toplot$score[i]<-exp.cont(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3],PAR[4])}}
	if (type=="exp.disc.x"){for (i in 1:length(toplot$score)){toplot$score[i]<-exp.disc.x(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3])}}
	if (type=="exp.cont.x"){for (i in 1:length(toplot$score)){toplot$score[i]<-exp.cont.x(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3],PAR[4])}}
	if (type=="pow.flat"){for (i in 1:length(toplot$score)){toplot$score[i]<-pow.flat(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2])}}
	if (type=="pow.disc"){for (i in 1:length(toplot$score)){toplot$score[i]<-pow.disc(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3],PAR[4])}}
	if (type=="pow.cont"){for (i in 1:length(toplot$score)){toplot$score[i]<-pow.cont(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3],PAR[4])}}
	if (type=="pow.disc.x"){for (i in 1:length(toplot$score)){toplot$score[i]<-pow.disc.x(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3])}}
	if (type=="pow.cont.x"){for (i in 1:length(toplot$score)){toplot$score[i]<-pow.cont.x(toplot$age[i],toplot$te[i],toplot$E[i],PAR[1],PAR[2],PAR[3],PAR[4])}}
	toplot$te<-as.factor(toplot$te)
	toplot$fac<-as.factor(paste(toplot$Eng_little,toplot$te,sep=""))
	
	p<-ggplot(data=toplot[toplot$Eng_little!="little",],aes(x=age,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1)+scale_color_manual(values=c("#D55E00","#009E73","#009E73","#009E73","#D55E00","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","red"))
	# p<-p+annotate("text",x=min(toplot$age[toplot$Eng_little=="monoeng"]),y=toplot$score[toplot$Eng_little=="monoeng" & toplot$age==min(toplot$age[toplot$Eng_little=="monoeng"])],label="m",color="black",size=3.5)
	# p<-p+annotate("text",x=min(toplot$age[toplot$Eng_little=="bileng"]),y=toplot$score[toplot$Eng_little=="bileng" & toplot$age==min(toplot$age[toplot$Eng_little=="bileng"])],label="0",color="black",size=3.5)
	# for (i in seq(2,29,3)){
		# jitter<-0
		# if (type=="exp.flat"){
			# numage<-min(toplot$age[toplot$Eng_little=="lot" & toplot$te==i])
		# }
		# p<-p+annotate("text",x=numage,y=toplot$score[toplot$Eng_little=="lot" & toplot$te==i & (toplot$age==numage)],label=as.character(i),color="black",size=3.5)
	# }
	p<-p+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="age")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		p<-p+scale_y_continuous(lim=c(1,4))
	}else{
		p<-p+scale_y_continuous(lim=c(.75,1))
	}
	p<-p+scale_x_continuous(lim=c(0,71)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
	
	p2<-ggplot(data=toplot[toplot$Eng_little=="little",],aes(x=age,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1)+scale_color_manual(values=c("#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00"))
	# for (i in seq(4,30,1)){
		# jitter<-0
		# if (i==5 | i==8){jitter<-11}
		# if (i==9){jitter<-16}
		# if (i==11){jitter<-5}
		# if (i==15){jitter<-4}
		# if (i==22){jitter<-14}
		# if (i==24){jitter<-5}
		# if (i==26){jitter<-11}
		# if (i==29){jitter<-3}
		# p2<-p2+annotate("text",x=max(toplot$age[toplot$Eng_little=="little" & toplot$te==i])-jitter,y=toplot$score[toplot$Eng_little=="little" & toplot$te==i & (toplot$age==max(toplot$age[toplot$Eng_little=="little" & toplot$te==i])-jitter)],label=as.character(i),color="black",size=3.5)
	# }
	p2<-p2+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="age")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		p2<-p2+scale_y_continuous(lim=c(1,4))
	}else{
		p2<-p2+scale_y_continuous(lim=c(.75,1))
	}
	p2<-p2+scale_x_continuous(lim=c(0,71)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

	q<-ggplot(data=toplot[toplot$Eng_little!="little",],aes(x=experience,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1) + scale_color_manual(values=c("#D55E00","#009E73","#009E73","#009E73","#D55E00","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","red"))
	# q<-q+annotate("text",x=70,y=toplot$score[toplot$Eng_little=="monoeng" & toplot$experience==70],label="m",color="black",size=3.5)
	# q<-q+annotate("text",x=70,y=toplot$score[toplot$Eng_little=="bileng" & toplot$experience==70],label="0",color="black",size=3.5)
	# for (i in seq(2,29,3)){
		# jitter<-0
		# if (i==23){jitter<-6}
		# if (i==20){jitter<-4}
		# q<-q+annotate("text",x=max(toplot$experience[toplot$Eng_little=="lot" & toplot$te==i])-jitter,y=toplot$score[toplot$Eng_little=="lot" & toplot$te==i & (toplot$experience==max(toplot$experience[toplot$Eng_little=="lot" & toplot$te==i])-jitter)],label=as.character(i),color="black",size=3.5)
	# }
	q<-q+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="years of experience")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		q<-q+scale_y_continuous(lim=c(1,4))
	}else{
		q<-q+scale_y_continuous(lim=c(.75,1))
	}
	q<-q+scale_x_continuous(lim=c(0,71)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
	
	q2<-ggplot(data=toplot[toplot$Eng_little=="little",],aes(x=experience,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1)+scale_color_manual(values=c("#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00"))
	# for (i in seq(4,30,1)){
		# jitter<-0
		# if (i==5 | i==8){jitter<-11}
		# if (i==9){jitter<-16}
		# if (i==11){jitter<-6}
		# if (i==12){jitter<-20}
		# if (i==22){jitter<-8}
		# if (i==26){jitter<-10}
		# q2<-q2+annotate("text",x=max(toplot$experience[toplot$Eng_little=="little" & toplot$te==i])-jitter,y=toplot$score[toplot$Eng_little=="little" & toplot$te==i & (toplot$experience==max(toplot$experience[toplot$Eng_little=="little" & toplot$te==i])-jitter)],label=as.character(i),color="black",size=3.5)
	# }
	q2<-q2+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="years of experience")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		q2<-q2+scale_y_continuous(lim=c(1,4))
	}else{
		q2<-q2+scale_y_continuous(lim=c(.75,1))
	}
	q2<-q2+scale_x_continuous(lim=c(0,71)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
	
	if (type=="exp.flat"){rate=data.frame(age=seq(1,70,.1),rate=PAR)}
	if (type=="exp.disc" | type=="exp.disc.x"){rate=data.frame(age=seq(1,70,.1),rate=c(rep(PAR[2],round(PAR[1])),rep(PAR[3],70-round(PAR[1]))))}
	if (type=="exp.cont" | type=="exp.cont.x"){
		rate=data.frame(age=seq(1,70,.1),rate=c(rep(PAR[2],691)))
		rate$rate[rate$age>PAR[1]]<-sapply(rate$age[rate$age>PAR[1]],function(x){PAR[2]*(1-1/(1+exp(-PAR[3]*(x-PAR[1]-PAR[4]))))})
		}
	if (type=="pow.flat"){rate=data.frame(age=seq(1,70,1),rate=PAR[2])}
	if (type=="pow.disc" | type=="pow.disc.x"){rate=data.frame(age=seq(1,70,1),rate=c(rep(PAR[2],round(PAR[1])),rep(PAR[3],70-round(PAR[1]))))}
	if (type=="pow.cont" | type=="pow.cont.x"){
		rate=data.frame(age=seq(1,70,1),rate=c(rep(PAR[2],70)))
		rate$rate[rate$age>PAR[1]]<-sapply(rate$age[rate$age>PAR[1]],function(x){PAR[2]*(1-1/(1+exp(-PAR[3]*(x-PAR[1]-PAR[4]))))})
		}
	r<-ggplot(data=rate,aes(x=age,y=rate))+geom_line(size=1.5)
	if (type=="exp.disc.x" | type=="exp.cont.x" | type=="pow.disc.x" | type=="pow.cont.x"){
		r<-r+scale_x_discrete(name="yrs experience",breaks=seq(0,70,10))
	}
    # new version plots alternatives in background
	rate$trajectory<-100
	temp<-rate
	# trajs<-list(c(0,.2,.09,.18),c(5,.20,.09,.18),c(25,.20,.09,.18),c(40,.20,.09,10),c(40,.20,.5,50),c(0,.21,.3,15),c(1,.15,.5,50),c(40,.13,.5,50),c(5,.5,.15,50),c(5,.5,.05,-50),c(40,.33,.5,50),c(1,.4,.09,50),c(40,.45,.5,50),c(38,.43,.15,45),c(30,.28,0,-50),c(8,.35,0,-50),c(3,.4,.01,-25),c(20,.47,.2,-20),c(1,.47,.07,50))
	trajs<-list(c(1,.2,.09,.18),c(25,.20,.09,.18),c(40,.20,.09,10),c(40,.20,.5,50),c(1,.21,.3,15),c(40,.13,.5,50),c(5,.5,.15,50),c(5,.5,.05,-50),c(40,.33,.5,50),c(1,.4,.09,50),c(40,.45,.5,50),c(38,.43,.15,45),c(30,.28,0,50),c(8,.35,0,-50),c(3,.4,.01,-25),c(1,.47,.07,50))
	for (i in 1:length(trajs)){
		PAR<-trajs[[i]]
		temp$rate<-c(rep(PAR[2],691))
		temp$rate[temp$age>PAR[1]]<-sapply(temp$age[temp$age>PAR[1]],function(x){PAR[2]*(1-1/(1+exp(-PAR[3]*(x-PAR[1]-PAR[4]))))})
		temp$trajectory<-i
		rate<-rbind(rate,temp)
	}
	rate$trajectory<-as.factor(rate$trajectory)
	r<-ggplot(data=rate,aes(x=age,y=rate,colour=trajectory))+geom_line(size=1.5) + scale_color_manual(values=c(rep("light grey",(length(trajs))),"black"))
	r <- r + theme(axis.text = element_text(color="black",size=16))
	r <- r + xlab("current age") + ylab("rate")
	r <- r + theme(axis.title.y = element_text(size=15))
	r <- r + theme(axis.title.x = element_text(size=15))	
	r <- r + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")

	pdf(file=paste(name,"all",".pdf",sep="_"),w=8,h=8)
	vplayout <- function(x,y) viewport(layout.pos.row=x, layout.pos.col=y)
	pushViewport(viewport(layout=grid.layout(3,2,heights=c(1,1,.5),widths=c(.5,.5))))
	print(p,vp=vplayout(1,1))
	print(q,vp=vplayout(2,1))
	print(p2,vp=vplayout(1,2))
	print(q2,vp=vplayout(2,2))
	print(r,vp=vplayout(3,2))
	dev.off()

	pdf(file=paste(name,"main",".pdf",sep="_"),w=7,h=7)
	vplayout <- function(x,y) viewport(layout.pos.row=x, layout.pos.col=y)
	pushViewport(viewport(layout=grid.layout(2,2,heights=c(1,1),widths=c(.5,.5))))
	print(p,vp=vplayout(1,1))
	print(q,vp=vplayout(2,1))
	print(p2,vp=vplayout(1,2))
	print(q2,vp=vplayout(2,2))
	dev.off()
	
	pdf(file=paste(name,"rate",".pdf",sep="_"),w=5.75,h=2.8)
	print(r)
	dev.off()
}


plotdat<-function(logodds=TRUE){
	require(doBy)
	require(ggplot2)
	require(grid)
		
	if(logodds){
		load("U:/DATA Hartshorne (2018)/HTP/data_binned")
	}else{
		load("U:/DATA Hartshorne (2018)/HTP/data_binned")
	}
	toplot<-binned
	toplot$te<-as.factor(toplot$te)
	toplot$fac<-as.factor(paste(toplot$Eng_little,toplot$te,sep=""))
	colnames(toplot)[4]<-"score"
	
	p<-ggplot(data=toplot[toplot$Eng_little!="little",],aes(x=age,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1)+scale_color_manual(values=c("#D55E00","#009E73","#009E73","#009E73","#D55E00","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","red"))
	p<-p+annotate("text",x=70,y=toplot$score[toplot$Eng_little=="monoeng" & toplot$age==70],label="m",color="black",size=3.5)
	p<-p+annotate("text",x=70,y=toplot$score[toplot$Eng_little=="bileng" & toplot$age==70],label="0",color="black",size=3.5)
	for (i in seq(2,29,3)){
		jitter<-0
		if (i==5){jitter<-2}
		if (i==23){jitter<-6}
		p<-p+annotate("text",x=max(toplot$age[toplot$Eng_little=="lot" & toplot$te==i])-jitter,y=toplot$score[toplot$Eng_little=="lot" & toplot$te==i & (toplot$age==max(toplot$age[toplot$Eng_little=="lot" & toplot$te==i])-jitter)],label=as.character(i),color="black",size=3.5)
	}
	p<-p+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="age")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		p<-p+scale_y_continuous(lim=c(1,4))
	}else{
		p<-p+scale_y_continuous(lim=c(.75,1))
	}
	p<-p+scale_x_continuous(lim=c(0,71))+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
	
	p2<-ggplot(data=toplot[toplot$Eng_little=="little",],aes(x=age,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1)+scale_color_manual(values=c("#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00"))
	for (i in seq(4,30,1)){
		jitter<-0
		if (i==4){jitter<-5}
		if (i==8){jitter<-11}
		if (i==5){jitter<-9}
		if (i==9){jitter<-16}
		if (i==11){jitter<-8}
		if (i==12){jitter<-5}
		if (i==14){jitter<-1}
		if (i==15){jitter<-4}
		if (i==22){jitter<-14}
		if (i==24){jitter<-5}
		if (i==26){jitter<-11}
		if (i==29){jitter<-3}
		p2<-p2+annotate("text",x=max(toplot$age[toplot$Eng_little=="little" & toplot$te==i])-jitter,y=toplot$score[toplot$Eng_little=="little" & toplot$te==i & (toplot$age==max(toplot$age[toplot$Eng_little=="little" & toplot$te==i])-jitter)],label=as.character(i),color="black",size=3.5)
	}
	p2<-p2+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="age")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		p2<-p2+scale_y_continuous(lim=c(1,4))
	}else{
		p2<-p2+scale_y_continuous(lim=c(.75,1))
	}
	p2<-p2+scale_x_continuous(lim=c(0,71))+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

	q<-ggplot(data=toplot[toplot$Eng_little!="little",],aes(x=experience,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1) + scale_color_manual(values=c("#D55E00","#009E73","#009E73","#009E73","#D55E00","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","red"))
	q<-q+annotate("text",x=70,y=toplot$score[toplot$Eng_little=="monoeng" & toplot$experience==70],label="m",color="black",size=3.5)
	q<-q+annotate("text",x=70,y=toplot$score[toplot$Eng_little=="bileng" & toplot$experience==70],label="0",color="black",size=3.5)
	for (i in seq(2,29,3)){
		jitter<-0
		if (i==23){jitter<-6}
		if (i==20){jitter<-4}
		q<-q+annotate("text",x=max(toplot$experience[toplot$Eng_little=="lot" & toplot$te==i])-jitter,y=toplot$score[toplot$Eng_little=="lot" & toplot$te==i & (toplot$experience==max(toplot$experience[toplot$Eng_little=="lot" & toplot$te==i])-jitter)],label=as.character(i),color="black",size=3.5)
	}
	q<-q+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="years of experience")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		q<-q+scale_y_continuous(lim=c(1,4))
	}else{
		q<-q+scale_y_continuous(lim=c(.75,1))
	}
	q<-q+scale_x_continuous(lim=c(0,71))+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
	
	q2<-ggplot(data=toplot[toplot$Eng_little=="little",],aes(x=experience,y=score))+geom_line(aes(fill=factor(fac),color=factor(fac)),alpha=1)+scale_color_manual(values=c("#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#009E73","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00"))
	for (i in seq(4,30,1)){
		jitter<-0
		if (i==5 | i==8){jitter<-11}
		if (i==9){jitter<-16}
		if (i==11){jitter<-6}
		if (i==12){jitter<-20}
		if (i==22){jitter<-9}
		if (i==24){jitter<-3}
		if (i==26){jitter<-10}
		if (i==28){jitter<-1}
		q2<-q2+annotate("text",x=max(toplot$experience[toplot$Eng_little=="little" & toplot$te==i])-jitter,y=toplot$score[toplot$Eng_little=="little" & toplot$te==i & (toplot$experience==max(toplot$experience[toplot$Eng_little=="little" & toplot$te==i])-jitter)],label=as.character(i),color="black",size=3.5)
	}
	q2<-q2+guides(color=FALSE)+labs(y="accuracy (log-odds)",x="years of experience")+theme(plot.title=element_text(size=11,color="black"),axis.title=element_text(size=14,color="black"),axis.text=element_text(size=11,color="black"))
	if(logodds){
		q2<-q2+scale_y_continuous(lim=c(1,4))
	}else{
		q2<-q2+scale_y_continuous(lim=c(.75,1))
	}
	q2<-q2+scale_x_continuous(lim=c(0,71))+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
	
	pdf(file="learningcurves.pdf",w=7,h=7)
	vplayout <- function(x,y) viewport(layout.pos.row=x, layout.pos.col=y)
	pushViewport(viewport(layout=grid.layout(2,2,heights=c(1,1),widths=c(.5,.5))))
	print(p,vp=vplayout(1,1))
	print(q,vp=vplayout(2,1))
	print(p2,vp=vplayout(1,2))
	print(q2,vp=vplayout(2,2))
	dev.off()
	
	# quartz(w=7,h=3.5)
	# vplayout <- function(x,y) viewport(layout.pos.row=x, layout.pos.col=y)
	# pushViewport(viewport(layout=grid.layout(1,2,heights=c(1,1),widths=c(.5,.5))))
	# print(p,vp=vplayout(1,1))
	# print(p2,vp=vplayout(1,2))
	# quartz(w=7,h=3.5)
	# vplayout <- function(x,y) viewport(layout.pos.row=x, layout.pos.col=y)
	# pushViewport(viewport(layout=grid.layout(1,2,heights=c(1,1),widths=c(.5,.5))))
	# print(q,vp=vplayout(1,1))
	# print(q2,vp=vplayout(1,2))
}

