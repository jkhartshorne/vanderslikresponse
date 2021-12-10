# BAR CHART FOR IMMERSION LEARNERS


correct <-read.csv("data.csv") # N = 669,498
# some exclusions used throughout
binningcorrect<-correct[correct$age<=70,]
correct<-correct[is.na(correct$Eng_little)==FALSE,]
correct<-correct[correct$Eng_start>=4 | correct$Eng_little!="little",] #N  = 662,228

# Is primelangs wether (1) or not (0) solely English?
correct$primedich <- ifelse(correct$primelangs == "English", 1,
                     ifelse(correct$primelangs == ", English", 1,
                     ifelse(correct$primelangs != "English", 0,99)))
summary(correct$primedich) # mean = .4635
correct$exp<-correct$age-correct$Eng_start

library(plyr)
BAR_LOT <- subset(correct, Eng_little == "lot")
summary(BAR_LOT$primedich)
#  Min.    1st Qu. Median  Mean   3rd Qu.   Max. 
#  0.000   0.000   1.000   0.641   1.000   1.000
summary(BAR_LOT$age)

BAR_LOT <- subset(BAR_LOT, BAR_LOT$age >= 7 & BAR_LOT$age <= 70)
BAR_LOT <- subset(BAR_LOT, BAR_LOT$Eng_start <= 30) # N = 14290
BAR_LOT$Eng_start_bin3 <- ifelse(BAR_LOT$Eng_start <= 3, 2,
                            ifelse(BAR_LOT$Eng_start >= 4 & BAR_LOT$Eng_start <=6, 5,
                            ifelse(BAR_LOT$Eng_start >= 7 & BAR_LOT$Eng_start <=9, 8,
                            ifelse(BAR_LOT$Eng_start >= 10 & BAR_LOT$Eng_start <=12, 11,
                            ifelse(BAR_LOT$Eng_start >= 13 & BAR_LOT$Eng_start <=15, 14,
                            ifelse(BAR_LOT$Eng_start >= 16 & BAR_LOT$Eng_start <=18, 17,
                            ifelse(BAR_LOT$Eng_start >= 19 & BAR_LOT$Eng_start <=21, 20,
                            ifelse(BAR_LOT$Eng_start >= 22 & BAR_LOT$Eng_start <=24, 23,
                            ifelse(BAR_LOT$Eng_start >= 25 & BAR_LOT$Eng_start <=27, 26,                              
                            ifelse(BAR_LOT$Eng_start >= 28 & BAR_LOT$Eng_start <=30, 29,99))))))))))

summary(BAR_LOT$Eng_start_bin3)
LOT_sum <- ddply(BAR_LOT, c("Eng_start_bin3"), summarise,
                 N    = length(primedich),
                 mean = mean(primedich),
                 sd   = sd(primedich),
                 se   = sd / sqrt(N),
                 CI   = 1.96*se)

# Relationship between AoE and English as sole medium of communication
pdf(file=paste("name","Bar Chart",".pdf",sep="_"),w=8,h=8)
ggplot(LOT_sum, aes(x=Eng_start_bin3, y=mean)) + 
  geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI), width=.1) +
  geom_line() + labs(x="Age of Exposure (3-year bins)/(CI 95%)", y = "English as sole medium ofcommunication") +
  geom_point()
dev.off()
