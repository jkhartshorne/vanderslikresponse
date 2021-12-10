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

# This file fits the Continuous and Discontinuous models for bilinguals only 

ability_metric <- "elogit" 

library("DEoptim")
library("tidyverse")

source("2-aggregate_functions LL.R")
data_old <- get(load("tofit.Rda", verbose = T))
data_old <- subset(data_old, data_old$t <=70)

old_file <- get(load("data_binned.Rda", verbose = T)) #N = 1791
old_file <- subset(old_file, old_file$age <= 70) # N = 1700
old_file <- subset(old_file, old_file$Eng_little == "bileng") #  N = 64
## CHECK the numbers BILENG####
sum(old_file$Nr) # N = 30347

binned_old<-binme_old(data_old, elogit = T, old_file)
binned_old <- subset(binned_old, binned_old$Eng_little == "bileng")
save(binned,file="data_binned.Rda")

names(data_old)
names(binned_old)
table(data_old$Eng_little)
summary(binned_old)
table(binned_old$Eng_little)
summary(as.factor(binned_old$groups))
table(binned_old$te[binned_old$groups == "bileng"])
table(binned_old$age)

#  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 
#  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
# 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 
#  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1

# flat refers to the HTP model


output_flat_bi <- data.frame(r=NA,a=NA,d=NA,Eb=NA,tc=NA)

#Fit the C and D models
assign("tofit", binned_old, envir=.GlobalEnv) # tofit n = 64

#############################################################.
source("3B-learningcurves_functions BILENG CONT LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_bi <- loss_flat_elogit_bi}


# Fit the C and D models
# CONTINUOUS
result_flat_bi <- DEoptim(loss_fn_flat_bi, lower = c(0,0,-50, 1, 0), upper = c(1,1,50,1,0),
        control = list(NP = 140, itermax = 2500))
fit_pars_flat_bi_c <- result_flat_bi[[1]]$bestmem
fit_pars_flat_bi_c
#CONTINUOUS 
# par1       par2       par3       par4       par5 
#  0.1047465  1.0000000 38.8287613  1.0000000  0.0000000

# Explained variances and AIC's
R2cont <- 1 - (loss_fn_flat_bi(fit_pars_flat_bi_c) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2cont # R2 = 0.9169284 for  continuous (tc=0)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 4.601897
SSEcont <- loss_fn_flat_bi(fit_pars_flat_bi_c)
SSEcont #<- 0.3822871
n <- 64
k <- 5

AICcont1 = n * log(SSEcont/n) + 2*k
AICcont1 #<- -317.7099

source("3B-learningcurves_functions BILENG DISC LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_bi <- loss_flat_elogit_bi}

# DISCONTINUOUS
result_flat_bi <- DEoptim(loss_fn_flat_bi, lower = c(0,0,-50, 1, 0), upper = c(1,1,50,1,40), 
                          control = list(NP = 140, itermax = 2500))
fit_pars_flat_bi_d <- result_flat_bi[[1]]$bestmem
fit_pars_flat_bi_d

#        par1        par2        par3        par4        par5 
#     0.104761937  1.000000000 -0.004115544  1.000000000 38.000000000 
## DISCONTINU
# 


R2disc <- 1 - (loss_fn_flat_bi(fit_pars_flat_bi_d) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2disc # R2 = .9169529  for discontinuous (tc=39.38)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 4.601897
SSEdisc <- loss_fn_flat_bi(fit_pars_flat_bi_d)
SSEdisc #<- 0.382174
n <- 64
k <- 7
#AICdisc = n * log(SStot - SSEdisc/n) + 2*k
#AICdisc #<- 111.8431
AICdisc1 = n * log(SSEdisc/n) + 2*k
AICdisc1 #<- -313.7288

#PROB <- exp((AICcont - AICdisc)/2)
#PROB # 0.135334
PROB1 <- exp((AICcont1 - AICdisc1)/2)
PROB1 # 0.1366227
PROB11 <- exp((AICdisc1 - AICcont1)/2)
PROB11 # 7.319428 

# D is .14 times more likely to minimize information loss than the C model. 




