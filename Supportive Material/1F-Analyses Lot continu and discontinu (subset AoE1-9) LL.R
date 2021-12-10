# Authors: Job Schepens & Frans van der Slik
# Script for: 
# van der Slik, F., Schepens, J., Bongaerts, T, .van Hout, R. 
# Revisiting Hartshorne, Tenenbaum, and Pinker (2018): 
# Critical Period Claim Revisited: Re-analysis of Hartshorne, Tenenbaum, and Pinker (2018) 
# Suggests Steady Decline and Learner-Type Differences.
# Language learning 

# Original script based on: 
# Hartshorne, J. K., Tenenbaum, J. B., & Pinker, S. (2018). 
# A critical period for second language acquisition: Evidence from 2/3 million English speakers. 
# Cognition, 177, 263277. https://doi.org/10.1016/j.cognition.2018.04.007
#


# This file fits the Continuous and Discontinuous models for early immersion learners 
# SUBSET AoE < 10#######
ability_metric <- "elogit" 

library("DEoptim")
library("tidyverse")


source("2-aggregate_functions LL.R")
data_old <- get(load("tofit.Rda", verbose = T)) 
data_old <- subset(data_old, data_old$t <=70)


old_file <- get(load("data_binned.Rda", verbose = T)) #N = 1791
old_file <- subset(old_file, old_file$age <= 70) 
old_file <- subset(old_file, old_file$Eng_little == "lot")
old_file <- subset(old_file, old_file$te <= 9) # Only for checking numbers of this subset. # N = 181
## CHECK the numbers LOT and AoE <= 9 ####
sum(old_file$Nr) # N = 11594

binned_old<-binme_old(data_old, elogit = T, old_file) # N = 181
binned_old <- subset(binned_old, binned_old$Eng_little == "lot")
binned_old <- subset(binned_old, binned_old$te <= 9)  ## SUBSET 1 - 9 AoE #####

save(binned,file="data_binned.Rda")

table(binned_old$Eng_little)
names(binned_old)
table(binned_old$te[binned_old$groups == "immersion"])

#   2  5  8 
#  61 61 59 



# # flat refers to the HTP model

output_flat_y <- data.frame(r=NA,a=NA,d=NA,Ei=NA,tc=NA)

#Fit the C and D models
assign("tofit", binned_old, envir=.GlobalEnv) # tofit n = 181

#####Continuous Model (tc=1)########################################################.
source("3i-learningcurves_functions LOT CONT LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_y <- loss_flat_elogit_y}


result_flat_y_c <- DEoptim(loss_fn_flat_y, lower = c(0,0,-50,1,1), upper = c(1,1,50,1,1),
        control = list(NP = 140, itermax = 2500))
fit_pars_flat_y_c <- result_flat_y_c[[1]]$bestmem
fit_pars_flat_y_c
# Continuous model######################################
#    par1     par2     par3     par4     par5 
#  0.1480801  1.0000000 26.1046663  1.0000000  1.0000000   tc =1
# Naggr = 181; Nindividual = 11594

# Explained variances and AIC's
# CONTINUOUS
R2cont <- 1 - (loss_fn_flat_y(fit_pars_flat_y_c) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2cont # R2 = 0.6471697 for  continuous (tc=1)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 9.364738
SSEcont <- loss_fn_flat_y(fit_pars_flat_y_c)
SSEcont #<- 9.325177
n <- 181
k <- 5
AICcont1 = n * log(SSEcont/n) + 2*k
AICcont1 #<- -715.366


# DISCONTINUOUS
source("3i-learningcurves_functions LOT DISC LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_y <- loss_flat_elogit_y}

result_flat_y_d <- DEoptim(loss_fn_flat_y, lower = c(0,0,-50,1,1), upper = c(1,1,50,1,40),
                         control = list(NP = 140, itermax = 2500))
fit_pars_flat_y_d <- result_flat_y_d[[1]]$bestmem
fit_pars_flat_y_d
#   par1 (r)    par2 (a)     par3 (d)   par4 (Ei)   par5 (tc) #flat discontinu # note: alpha and delta are not stable
#   00.1480103   0.9282192 -38.9774090   1.0000000  27.076529  (tc = 27.08)    # r, tc, AIC, and R^2 are stable
# Naggr = 181; Nindividual = 11594

R2disc <- 1 - (loss_fn_flat_y(fit_pars_flat_y_d) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2disc # R2 = 0.6475486  for discontinuous 
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 9.325177
SSEdisc <- loss_fn_flat_y(fit_pars_flat_y_d)
SSEdisc #<- 3.300522
n <- 181
k <- 7

AICdisc1 = n * log(SSEdisc/n) + 2*k
AICdisc1  #<- -711.5605

PROB1 <- exp((AICdisc1 - AICcont1)/2)
PROB1 #<- 6.704426
PROB11 <- exp((AICcont1 - AICdisc1)/2)
PROB11 #< 0.1491522

# The continuous model is 6.704426 times as likely to minimize information loss than the D model
# In reverse, the D model is 0.1491552 times as likely, etc.

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

