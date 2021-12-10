library("DEoptim")
library("tidyverse")
source("helper_functions.R")

dat <- read_csv("../data/full.csv")
dat <- dat[,c("id", "age","elogit","Eng_little","Eng_start")]

dat$groups <- as.factor(dat$Eng_little)
dat$groups <- fct_recode(dat$groups, "monolinguals" = "monoeng",
                         "bilinguals" = "bileng",
                         "immersion" = "lot",
                         "non-immersion" = "little")

dat <- dat[!is.na(dat$groups), ]

#Remove people after cognitive decline, and clean out some weird non-immersion learners
dat <- dat[dat$age <= 70, ]
dat <- dat[dat$groups != "non-immersion" | dat$Eng_start >=4, ]
dat$exp<-dat$age-dat$Eng_start
colnames(dat)<-c("id","age","ability","Eng_little","te","groups","exp")

######Binning
bins <- function(dat,w,c){
  monos<-cbind(smooth2(dat[dat$groups=="monolinguals",c("exp","ability")],w,c),te=0)
  colnames(monos)[4]<-"ability"
  monos$exp<-monos$age-monos$te
  monos$groups<-"monolinguals"

  bis<-cbind(smooth2(dat[dat$groups=="bilinguals",c("exp","ability")],w,c),te=0)
  colnames(bis)[4]<-"ability"
  bis$exp<-bis$age-bis$te
  bis$groups<-"bilinguals"

  #non-immersion, year-by-year
  for (y in 4:30){
    assign(paste("binned_f",y,sep=""),cbind(smooth2(dat[dat$groups=="non-immersion" & dat$te==y,c("exp","ability")],w,c),te=y))
  }
  nimms<-rbind(binned_f4,binned_f5,binned_f6,binned_f7,binned_f8,binned_f9,binned_f10,binned_f11,binned_f12,binned_f13,binned_f14,binned_f15,binned_f16,binned_f17,binned_f18,binned_f19,binned_f20,binned_f21,binned_f22,binned_f23,binned_f24,binned_f25,binned_f26,binned_f27,binned_f28,binned_f29,binned_f30)
  colnames(nimms)[4]<-"ability"
  nimms$exp<-nimms$age
  nimms$age<-nimms$exp+nimms$te
  nimms$groups<-"non-immersion"
  
  #immersion, 3-year bins
  jump<-3
  for (y in seq(1,30,jump)){
    assign(paste("binned_i",y,sep=""),cbind(smooth2(dat[dat$groups=="immersion" & dat$te>=y & dat$te<(y+jump), c("exp","ability")],w,c),te=y+(jump-1)/2))
  }
  imms1<-rbind(binned_i1,binned_i4,binned_i7)
  colnames(imms1)[4]<-"ability"
  imms1$exp<-imms1$age
  imms1$age<-imms1$exp+imms1$te
  imms1$groups<-"immersion"
  imms2<-rbind(binned_i10,binned_i13,binned_i16,binned_i19,binned_i22,binned_i25,binned_i28)
  colnames(imms2)[4]<-"ability"
  imms2$exp<-imms2$age
  imms2$age<-imms2$exp+imms2$te
  imms2$groups<-"immersion"
  
  binned<-rbind(monos,bis,imms1,imms2,nimms)
  colnames(binned)[4]<-"ability"
  return(list(monos,bis,imms1,imms2,nimms,binned))
}

smoothed<-bins(dat,2,10)
binned.nosmooth<-bins(dat,0,1)

monos<-binned.nosmooth[[1]][,c("exp","ability","te","age","groups")]
colnames(monos)[2]<-"bability"
monos<-merge(smoothed[[1]],monos)

bis<-binned.nosmooth[[2]][,c("exp","ability","te","age","groups")]
colnames(bis)[2]<-"bability"
bis<-merge(smoothed[[2]],bis)

imms1<-binned.nosmooth[[3]][,c("exp","ability","te","age","groups")]
colnames(imms1)[2]<-"bability"
imms1<-merge(smoothed[[3]],imms1)

imms2<-binned.nosmooth[[4]][,c("exp","ability","te","age","groups")]
colnames(imms2)[2]<-"bability"
imms2<-merge(smoothed[[4]],imms2)

nimms<-binned.nosmooth[[5]][,c("exp","ability","te","age","groups")]
colnames(nimms)[2]<-"bability"
nimms<-merge(smoothed[[5]],nimms)

binned<-binned.nosmooth[[6]][,c("exp","ability","te","age","groups")]
colnames(binned)[2]<-"bability"
binned<-merge(smoothed[[6]],binned)

output_piecewise <- data.frame(r=NA,a1=NA,a2=NA,d1=NA,d2=NA,Eb=NA,Ei=NA,En=NA)

#-------------------------------------------Fit the actual model-------------------------------------------
set.seed(1234) #for reproducibility

tofit <- binned
#fit to all data, using the old restrictions on range
result_piecewise <- DEoptim(loss_piecewise_elogit, lower = c(0,0,0,-40,-40,0,0,0,2,1.5), upper = c(1,50,2,50,50,2,2,2,2,1.5),
                            control= list(strategy=6, c=.05, p=.4, NP = 500, itermax = 5000, parallelType=1, reltol=.0000001, steptol=500, parVar=list("tofit","loss_curve_piecewise")))
fit_pars_piecewise_classic <- result_piecewise[[1]]$bestmem

#fit to all data, using the revised old restrictions on range
result_piecewise <- DEoptim(loss_piecewise_elogit, lower = c(0,0,0,-40,-40,0,0,0,(max(binned$ability)-min(binned$ability)),min(binned$ability)), upper = c(1,50,2,50,50,2,2,2,(max(binned$ability)-min(binned$ability)),min(binned$ability)),
                            control= list(strategy=6, c=.05, p=.4, NP = 500, itermax = 5000, parallelType=1, reltol=.0000001, steptol=500, parVar=list("tofit","loss_curve_piecewise")))
fit_pars_piecewise_classic2 <- result_piecewise[[1]]$bestmem

#fit to all data, using full range of possible scores (assuming chance is the floor)
##BTW this one fits horribly. Not too surprising, given it wants to start at 0
result_piecewise <- DEoptim(loss_piecewise_elogit, lower = c(0,0,0,-40,-40,0,0,0,log(95.5/.5),0), upper = c(1,50,2,50,50,2,2,2,log(95.5/.5),0),
                            control= list(strategy=6, c=.05, p=.4, NP = 500, itermax = 5000, parallelType=1, reltol=.0000001, steptol=500, parVar=list("tofit","loss_curve_piecewise")))
fit_pars_piecewise_wholerange <- result_piecewise[[1]]$bestmem

#fit to all data, allowing score range to be fit  (assuming chance is the floor)
tofit<-binned
result_piecewise <- DEoptim(loss_piecewise_elogit, lower = c(0,0,0,-50,-50,0,0,0,1,0), upper = c(1,50,2,50,50,2,2,2,log(95.5/.5),log(95.5/.5)),
                            control= list(strategy=6, c=.05, p=.4, NP = 500, itermax = 5000, parallelType=1, reltol=.0000001, steptol=500, parVar=list("tofit","loss_curve_piecewise")))
fit_pars_piecewise_fitrange <- result_piecewise[[1]]$bestmem

#fit to monolinguals, using full range of possible scores (assuming chance is the floor)
tofit<-binned[binned$groups=="monolinguals",]
result_piecewise <- DEoptim(loss_piecewise_elogit, lower = c(0,0,0,-50,-50,0,0,0,(max(binned$ability)-min(binned$ability)),min(binned$ability)), upper = c(1,50,2,50,50,0,0,0,(max(binned$ability)-min(binned$ability)),min(binned$ability)),
                            control= list(strategy=6, c=.05, p=.4, NP = 500, itermax = 5000, parallelType=1, reltol=.0000001, steptol=500, parVar=list("tofit","loss_curve_piecewise")))
fit_pars_piecewise_monos_classic2<- result_piecewise[[1]]$bestmem
#additional fit with truncated monolingual data
tofit<-binned[binned$groups=="monolinguals" & binned$age<=35,]
result_piecewise <- DEoptim(loss_piecewise_elogit, lower = c(0,0,0,-50,-50,0,0,0,(max(tofit$ability)-min(tofit$ability)),min(tofit$ability)), upper = c(1,50,2,50,50,0,0,0,(max(tofit$ability)-min(tofit$ability)),min(tofit$ability)),
                            control= list(strategy=6, c=.05, p=.4, NP = 500, itermax = 5000, parallelType=1, reltol=.0000001, steptol=500, parVar=list("tofit","loss_curve_piecewise")))
fit_pars_piecewise_monos_classic2_short<- result_piecewise[[1]]$bestmem

#fit to bilinguals, using full range of possible scores (assuming chance is the floor)
tofit<-binned[binned$groups=="bilinguals",]
result_piecewise <- DEoptim(loss_piecewise_elogit, lower = c(0,0,0,-50,-50,1,0,0,(max(binned$ability)-min(binned$ability)),min(binned$ability)), upper = c(1,50,2,50,50,1,0,0,(max(binned$ability)-min(binned$ability)),min(binned$ability)),
                            control= list(strategy=6, c=.05, p=.4, NP = 500, itermax = 5000, parallelType=1, reltol=.0000001, steptol=500, parVar=list("tofit","loss_curve_piecewise")))
fit_pars_piecewise_bis_classic2<- result_piecewise[[1]]$bestmem

#fit to imms1, using full range of possible scores (assuming chance is the floor)
tofit<-binned[binned$groups=="immersion" & binned$te<10,]
result_piecewise <- DEoptim(loss_piecewise_elogit, lower = c(0,0,0,-40,-40,0,1,0,(max(binned$ability)-min(binned$ability)),min(binned$ability)), upper = c(1,50,2,50,50,0,1,0,(max(binned$ability)-min(binned$ability)),min(binned$ability)),
                            control= list(strategy=6, c=.05, p=.4, NP = 500, itermax = 5000, parallelType=1, reltol=.0000001, steptol=500, parVar=list("tofit","loss_curve_piecewise")))
fit_pars_piecewise_imms1_classic2<- result_piecewise[[1]]$bestmem

#fit to imms2, using full range of possible scores (assuming chance is the floor)
tofit<-binned[binned$groups=="immersion" & binned$te>9,]
result_piecewise <- DEoptim(loss_piecewise_elogit, lower = c(0,0,0,-50,-50,0,1,0,(max(binned$ability)-min(binned$ability)),min(binned$ability)), upper = c(1,50,2,50,50,0,1,0,(max(binned$ability)-min(binned$ability)),min(binned$ability)),
                            control= list(strategy=6, c=.05, p=.4, NP = 500, itermax = 5000, parallelType=1, reltol=.0000001, steptol=500, parVar=list("tofit","loss_curve_piecewise")))
fit_pars_piecewise_imms2_classic2<- result_piecewise[[1]]$bestmem

#fit to nimms, using full range of possible scores (assuming chance is the floor)
tofit<-binned[binned$groups=="non-immersion",]
result_piecewise <- DEoptim(loss_piecewise_elogit, lower = c(0,0,0,-50,-50,0,0,1,(max(binned$ability)-min(binned$ability)),min(binned$ability)), upper = c(1,50,2,50,50,0,0,1,(max(binned$ability)-min(binned$ability)),min(binned$ability)),
                            control= list(strategy=6, c=.05, p=.4, NP = 500, itermax = 5000, parallelType=1, reltol=.0000001, steptol=500, parVar=list("tofit","loss_curve_piecewise")))
fit_pars_piecewise_nimms_classic2<- result_piecewise[[1]]$bestmem

plotTwoGroups.CH<-function(somedat, simdata, pars1, pars2){
  simdata$exp<-simdata$age-simdata$te
  for (i in 1:length(simdata$elogit1)){simdata$elogit1[i]<-(1-exp(loss_curve_piecewise(pars1[1],pars1[2],pars1[3],pars1[4],pars1[5],pars1[6],pars1[7],pars1[8],simdata$te[i],simdata$age[i],simdata$groups[i])))*pars1[9]+pars1[10]}
  for (i in 1:length(simdata$elogit2)){simdata$elogit2[i]<-(1-exp(loss_curve_piecewise(pars2[1],pars2[2],pars2[3],pars2[4],pars2[5],pars2[6],pars2[7],pars2[8],simdata$te[i],simdata$age[i],simdata$groups[i])))*pars2[9]+pars2[10]}
  p <- ggplot(simdata, aes(x=age, y=elogit1, group=te, fill=te)) + 
    geom_line(aes(), lwd=1.25) + 
    geom_line(aes(y=elogit2, color='red', ), lwd=1.25, linetype="dashed") + 
    theme_bw() + 
    geom_smooth(method="loess", data=somedat, aes(y=bability, x=age, group=te, fill=0)) + 
    theme(legend.position = "none")

  calcr = function(pars,t,group){
    r = pars[1]
    a1 = pars[2]
    a2 = pars[3]
    d1 = pars[4]
    d2 = pars[5]
    Eb = pars[6]
    Ei = pars[7]
    En = pars[8]
    E <- ifelse(group == "non-immersion",
                En,
                ifelse(group == "immersion",
                       Ei,
                       ifelse(group == "bilinguals",
                              Eb,
                              1)))
    ta <- (a1 * d1 - a2 * d2)/(a1 - a2) # Point of intersection between the two sigmoids
    out <- ifelse(t <= ta,
                  E * r * (1 - 1/(1 + exp(-1*a1*(t-d1)))),
                  E * r * (1 - 1/(1 + exp(-1*a2*(t-d2))))
            )
    return(out)
  }
  x = seq(0, 70, .1)
  y = unlist(lapply(x, function(a) {calcr(pars1, a, somedat$groups[1])}))
  tempdat<-data.frame(x=x, y=y, y2 = unlist(lapply(x, function(a) {calcr(pars2, a, somedat$groups[1])})))
  r <- ggplot(tempdat, aes(x=x, y=y)) + geom_line(lwd=1.25) + xlab("age") + ylab("learning rate")
  r <- r + geom_line(aes(y=y2, color='red'), lwd=1.25, linetype='dashed') + 
    theme_bw() + ylab("learning rate") + 
    theme(text = element_text(size=20)) + theme(legend.position = "none")
    
  return(list(r,p))
}

#plot monos
data<-data.frame(age = seq(1, 70, .25), te=0, elogit1=0, elogit2=0, groups="monolinguals")
plotTwoGroups.CH(binned[binned$groups=="monolinguals",], data, fit_pars_piecewise_fitrange, fit_pars_piecewise_classic2) -> res
res[[1]]
res[[2]]+ylim(1.5,3.75)
plotTwoGroups.CH(binned[binned$groups=="monolinguals",], data, fit_pars_piecewise_classic2, fit_pars_piecewise_monos_classic2) -> res
res[[1]]
res[[2]]+ylim(1.5,3.75)

#plot bis
data<-data.frame(age = seq(1, 70, .25), te=0, elogit1=0, elogit2=0, groups="bilinguals")
plotTwoGroups.CH(binned[binned$groups=="bilinguals",], data, fit_pars_piecewise_fitrange, fit_pars_piecewise_classic2) -> res
res[[1]]
res[[2]]+ylim(1.5,3.75)
plotTwoGroups.CH(binned[binned$groups=="bilinguals",], data, fit_pars_piecewise_classic2, fit_pars_piecewise_bis_classic2) -> res
res[[1]]
res[[2]]+ylim(1.5,3.75)

#plot imm1
data<-rbind(data.frame(age = seq(1, 70, .25), te=2, elogit1=0, elogit2=0, groups="immersion"),
            data.frame(age = seq(1, 70, .25), te=5, elogit1=0, elogit2=0, groups="immersion"),
            data.frame(age = seq(1, 70, .25), te=8, elogit1=0, elogit2=0, groups="immersion"))
data<-data[data$age>=data$te,]
plotTwoGroups.CH(binned[binned$groups=="immersion" & binned$te<10,], data, fit_pars_piecewise_fitrange, fit_pars_piecewise_classic2) -> res
res[[1]]
res[[2]]+ylim(.9,3.75)
plotTwoGroups.CH(binned[binned$groups=="immersion" & binned$te<10,], data, fit_pars_piecewise_classic2, fit_pars_piecewise_imms1_classic2) -> res
res[[1]]
res[[2]]+ylim(.9,3.75)

#plot imm2
data<-rbind(data.frame(age = seq(1, 70, .25), te=11, elogit1=0, elogit2=0, groups="immersion"),
            data.frame(age = seq(1, 70, .25), te=14, elogit1=0, elogit2=0, groups="immersion"),
            data.frame(age = seq(1, 70, .25), te=17, elogit1=0, elogit2=0, groups="immersion"),
            data.frame(age = seq(1, 70, .25), te=20, elogit1=0, elogit2=0, groups="immersion"),
            data.frame(age = seq(1, 70, .25), te=23, elogit1=0, elogit2=0, groups="immersion"),
            data.frame(age = seq(1, 70, .25), te=26, elogit1=0, elogit2=0, groups="immersion"),
            data.frame(age = seq(1, 70, .25), te=29, elogit1=0, elogit2=0, groups="immersion"))
data<-data[data$age>=data$te,]
plotTwoGroups.CH(binned[binned$groups=="immersion" & binned$te>9,], data, fit_pars_piecewise_classic, fit_pars_piecewise_classic2) -> res
res[[1]]
res[[2]]+ylim(.9,3.5)
plotTwoGroups.CH(binned[binned$groups=="immersion" & binned$te>9,], data, fit_pars_piecewise_classic2, fit_pars_piecewise_imms2_classic2) -> res
res[[1]]
res[[2]]+ylim(.9,3.5)

#plot nimms
data<-rbind(data.frame(age = seq(1, 70, .25), te=4, elogit1=0, elogit2=0, groups="non-immersion"),
            data.frame(age = seq(1, 70, .25), te=8, elogit1=0, elogit2=0, groups="non-immersion"),
            data.frame(age = seq(1, 70, .25), te=12, elogit1=0, elogit2=0, groups="non-immersion"),
            data.frame(age = seq(1, 70, .25), te=16, elogit1=0, elogit2=0, groups="non-immersion"),
            data.frame(age = seq(1, 70, .25), te=20, elogit1=0, elogit2=0, groups="non-immersion"),
            data.frame(age = seq(1, 70, .25), te=24, elogit1=0, elogit2=0, groups="non-immersion"),
            data.frame(age = seq(1, 70, .25), te=28, elogit1=0, elogit2=0, groups="non-immersion"))
data<-data[data$age>=data$te,]
plotTwoGroups.CH(binned[binned$groups=="non-immersion" & (binned$te==4 | binned$te==8 | binned$te==12 | binned$te==16 | binned$te==20 | binned$te==24 | binned$te==28),], data, fit_pars_piecewise_classic, fit_pars_piecewise_classic2) -> res
res[[1]]
res[[2]]+ylim(.9,3.5)
plotTwoGroups.CH(binned[binned$groups=="non-immersion" & (binned$te==4 | binned$te==8 | binned$te==12 | binned$te==16 | binned$te==20 | binned$te==24 | binned$te==28),], data, fit_pars_piecewise_classic2, fit_pars_piecewise_nimms_classic2) -> res
res[[1]]
res[[2]]+ylim(.9,3.5)

binned.newdat<-binned
save(binned.newdat,fit_pars_piecewise_classic, 
     fit_pars_piecewise_classic2, 
     fit_pars_piecewise_wholerange, 
     fit_pars_piecewise_fitrange, 
     fit_pars_piecewise_nimms_classic2, 
     fit_pars_piecewise_imms2_classic2, 
     fit_pars_piecewise_imms1_classic2, 
     fit_pars_piecewise_bis_classic2, 
     fit_pars_piecewise_monos_classic2, 
     fit_pars_piecewise_monos_classic2_short,
     file="fits")
