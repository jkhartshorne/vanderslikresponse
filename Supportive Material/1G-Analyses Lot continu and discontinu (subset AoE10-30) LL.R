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


# This file fits the Continuous and Discontinuous models for later immersion learners 
# SUBSET AoE > 9#######
ability_metric <- "elogit" 

library("DEoptim")
library("tidyverse")


source("2-aggregate_functions LL.R")
data_old <- get(load("tofit.Rda", verbose = T))
data_old <- subset(data_old, data_old$t <=70)


old_file <- get(load("data_binned.Rda", verbose = T)) #N = 1791
old_file <- subset(old_file, old_file$age <= 70) 
old_file <- subset(old_file, old_file$Eng_little == "lot")
#old_file <- subset(old_file, old_file$te >= 9) # Only for checking numbers of this subset. Keep it inactive for analyses!
## CHECK the numbers LOT and AoE >= 9 ####
sum(old_file$Nr) # N = 2505

binned_old<-binme_old(data_old, elogit = T, old_file) # N = 217
binned_old <- subset(binned_old, binned_old$Eng_little == "lot")
binned_old <- subset(binned_old, binned_old$te >= 9)  ## SUBSET 10 - 30 AoE #####

save(binned,file="data_binned.Rda")

table(binned_old$Eng_little)
names(binned_old)
table(binned_old$te[binned_old$groups == "immersion"])

# 11 14 17 20 23 26 29 ## This is the final distribution of later immersion learners
# 48 35 28 27 25 26 28 



# # flat refers to the HTP model

output_flat_y <- data.frame(r=NA,a=NA,d=NA,Ei=NA,tc=NA)

#Fit the C and D models
assign("tofit", binned_old, envir=.GlobalEnv) # tofit n = 217

#####Continuous Model (tc=1)########################################################.
source("3i-learningcurves_functions LOT CONT LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_y <- loss_flat_elogit_y}


result_flat_y_c <- DEoptim(loss_fn_flat_y, lower = c(0,0,-50,1,1), upper = c(1,1,50,1,1), 
        control = list(NP = 140, itermax = 2500))
fit_pars_flat_y_c <- result_flat_y_c[[1]]$bestmem
fit_pars_flat_y_c
# Continu model######################################
#       par1        par2        par3        par4        par5 
#  0.09362415  0.16990149 36.15647855  1.00000000  1.00000000   tc =1
# Naggr = 217; Nindividual = 2505

# Explained variances and AIC's
# CONTINU
R2cont <- 1 - (loss_fn_flat_y(fit_pars_flat_y_c) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2cont # R2 = 0.9035865 for  continuous (tc=1)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 46.5022
SSEcont <- loss_fn_flat_y(fit_pars_flat_y_c)
SSEcont #<- 4.483438
n <- 217
k <- 5
AICcont1 = n * log(SSEcont/n) + 2*k
AICcont1 #<- -831.8531


# DISCONTINU
source("3i-learningcurves_functions LOT DISC LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_y <- loss_flat_elogit_y}

result_flat_y_d <- DEoptim(loss_fn_flat_y, lower = c(0,0,-50,1,1), upper = c(1,1,50,1,40), 
                         control = list(NP = 140, itermax = 2500))
fit_pars_flat_y_d <- result_flat_y_d[[1]]$bestmem
fit_pars_flat_y_d
#   par1 (r)    par2 (a)     par3 (d)   par4 (Ei)   par5 (tc) #flat discontinu
#    0.11950457  0.08341141 10.44047711  1.00000000 19.00000000 (tc = 19.00)
# Naggr = 217; Nindividual = 2505

R2disc <- 1 - (loss_fn_flat_y(fit_pars_flat_y_d) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2disc # R2 = 0.9246971  for discontinuous (tc=19.00)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 46.5022
SSEdisc <- loss_fn_flat_y(fit_pars_flat_y_d)
SSEdisc #<- 3.501753
n <- 217
k <- 7

AICdisc1 = n * log(SSEdisc/n) + 2*k
AICdisc1  #<- -881.4795

PROB1 <- exp((AICdisc1 - AICcont1)/2)
PROB1 #<- 1.673974e-11
PROB11 <- exp((AICcont1 - AICdisc1)/2)
PROB11 #< 59738093569

# The continuous model is 1.673974e-11 times as likely to minimize information loss than the D model
# In reverse, the D model is 59738093569 times as likely, etc.

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

