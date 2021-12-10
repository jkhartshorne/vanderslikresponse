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


# This file fits the Continuous and Discontinuous models for monolinguals only 

ability_metric <- "elogit" 

library("DEoptim")
library("tidyverse")

source("2-aggregate_functions LL.R")
data_old <- get(load("tofit.Rda", verbose = T)) 
data_old <- subset(data_old, data_old$t <=70) # N = 548857
sum(data_old$Nr)
old_file <- get(load("data_binned.Rda", verbose = T)) #N = 1791
old_file <- subset(old_file, old_file$age <= 70) 
old_file <- subset(old_file, old_file$Eng_little == "monoeng") #  N = 64
## CHECK the numbers MONOENG####
sum(old_file$Nr) # N = 244,840

binned_old<-binme_old(data_old, elogit = T, old_file)
binned_old <- subset(binned_old, binned_old$Eng_little == "monoeng")
save(binned,file="data_binned.Rda")

names(data_old)
names(binned_old)
table(data_old$Eng_little)
summary(binned_old)
table(binned_old$Eng_little)
summary(as.factor(binned_old$groups))
table(binned_old$age)

#  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 
#  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
# 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 
#  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1

output_flat_mo <- data.frame(r=NA,a=NA,d=NA,tc=NA)


#Fit the C and D models
assign("tofit", binned_old, envir=.GlobalEnv) # tofit n = 64

source("3M-learningcurves_functions MONO CONT LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_mo <- loss_flat_elogit_mo}

#CONTINUOUS
result_flat_mo <- DEoptim(loss_fn_flat_mo, lower = c(0,0,-50,0), upper = c(1,1,50,0), 
        control = list(NP = 140, itermax = 2500))
fit_pars_flat_mo <- result_flat_mo[[1]]$bestmem
fit_pars_flat_mo
#     par1        par2        par3        par4 
#   0.1460809  1.0000000 50.0000000  0.0000000

R2cont <- 1 - (loss_fn_flat_mo(fit_pars_flat_mo) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2cont # R2 = 0.7310579for  continuous (tc=0)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 3.3146
SSEcont <- loss_fn_flat_mo(fit_pars_flat_mo)
SSEcont #<- 0.8914356
n <- 64
k <- 4
AICcont1 = n * log(SSEcont/n) + 2*k
AICcont1 #<- -265.5235


####DISCONTINUOUS MODEL ####
source("3M-learningcurves_functions MONO DISC LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_mo <- loss_flat_elogit_mo}

#DISCONTINUOUS
result_flat_mo <- DEoptim(loss_fn_flat_mo, lower = c(0,0,-50,0), upper = c(1,1,50,40), 
                          control = list(NP = 140, itermax = 2500))
fit_pars_flat_mo <- result_flat_mo[[1]]$bestmem
fit_pars_flat_mo
# par1       par2       par3       par4 
#  0.1457527  0.9999134 49.9998467 39.9983148 


# DISCONTINUOUS
R2disc <- 1 - (loss_fn_flat_mo(fit_pars_flat_mo) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2disc # R2 = .7322205 for discontinuous (tc=40)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 3.3146
SSEdisc <- loss_fn_flat_mo(fit_pars_flat_mo)
SSEdisc #<- 0.8875821
n <- 64
k <- 6
AICdisc1 = n * log(SSEdisc/n) + 2*k
AICdisc1 #<- -261.8008


PROB1 <- exp((AICcont1 - AICdisc1)/2)
PROB1 # 0.1554597
PROB11 <- exp((AICdisc1 - AICcont1)/2)
PROB11 # 6.432534

# D is .16 times more likely than C.
