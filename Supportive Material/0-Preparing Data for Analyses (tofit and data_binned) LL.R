# This file is a modified version of the "analyze_learning_curves.R" script, 
# we've received from Tianhu Chen and Joshua Hartshorne
# "0-Preparing Data for Analyses (tofit and data_binned).R"

# learningcurves_models.R is a source script we've got from Tianhu Chen and Joshua Hartshorne.
# 2-aggregate_functions is a modified version of the script learningcurves_models.R

source("2-aggregate_functions LL.R")


# data.csv is the HTP data file downloaded from: https://osf.io/pyb8s/
correct <-read.csv("data.csv") # N = 669,498

# some exclusions used throughout
binningcorrect<-correct[correct$age<=70,]
correct<-correct[is.na(correct$Eng_little)==FALSE,]
correct<-correct[correct$Eng_start>=4 | correct$Eng_little!="little",] #N  = 662,228
correct$exp<-correct$age-correct$Eng_start

############  BINNING AND SMOOTHING ########
binned_m<-cbind(smooth2(correct[correct$Eng_little=="monoeng",c("exp","elogit")],2,10),te=0,Eng_little="monoeng") #note --  smoothed because is done in HTP paper
# Compare HTP (2018, p.268:Fig. 4A, "m" [smoothed] with 2018, p4: Fig S3C, "m" [non-smoothed] 
# binned_m<-cbind(smooth2(correct[correct$Eng_little=="monoeng",c("exp","elogit")],0,10),te=0,Eng_little="monoeng") #note -- not smoothed because didn't seem necessary
binned_s<-cbind(smooth2(correct[correct$Eng_little=="bileng",c("exp","elogit")],2,10),te=0,Eng_little="bileng")

#non-immersion, year-by-year
binned<-binned_s
for (y in 4:30){
	assign(paste("binned_f",y,sep=""),cbind(smooth2(correct[correct$Eng_little=="little" & correct$Eng_start==y,c("exp","elogit")],2,10),te=y,Eng_little="little"))
}
binned<-rbind(binned_s,binned_m,binned_f4,binned_f5,binned_f6,binned_f7,binned_f8,binned_f9,binned_f10,binned_f11,binned_f12,binned_f13,binned_f14,binned_f15,binned_f16,binned_f17,binned_f18,binned_f19,binned_f20,binned_f21,binned_f22,binned_f23,binned_f24,binned_f25,binned_f26,binned_f27,binned_f28,binned_f29,binned_f30)

#immersion, 3-year bins
jump<-3
for (y in seq(1,30,jump)){
	assign(paste("binned_i",y,sep=""),cbind(smooth2(correct[correct$Eng_little=="lot" & correct$Eng_start>=y & correct$Eng_start<(y+jump),c("exp","elogit")],2,10),te=y+(jump-1)/2,Eng_little="lot"))
}
binned<-rbind(binned,binned_i1,binned_i4,binned_i7,binned_i10,binned_i13,binned_i16,binned_i19,binned_i22,binned_i25,binned_i28)
colnames(binned)[1]<-"experience"
binned$age<-binned$experience+binned$te

save(binned,file="data_binned.Rda") # N = 1791 AGGREGATED DATA FOR FURTHER ANALYSES

tofit<-correct[,c("Eng_little","Eng_start","age","elogit")]
tofit$e<-tofit$age-tofit$Eng_start
tofit$E<-1 #temporary. will be replaced lower down.
tofit<-tofit[tofit$Eng_start>3 | tofit$Eng_little!="little",] #get rid of the weird, young non-immersion learners
tofit<-tofit[tofit$age<=70,] #get them before cognitive decline
tofit<-tofit[tofit$Eng_little!="",]
colnames(tofit)<-c("Eng_little","te","t","elogit","e","E") 

save(tofit,file="tofit.Rda")  # N = 548,857 DATA FOR FURTHER ANALYSES

# tofit.Rda and data_binned.Rda will be used as input for all remaining analyses