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

# This file fits the Continuous and Discontinuous models for non-immersion learners 

ability_metric <- "elogit" 

library("DEoptim")
library("tidyverse")


source("2-aggregate_functions LL.R")
data_old <- get(load("tofit.Rda", verbose = T)) 

old_file <- get(load("data_binned.Rda", verbose = T)) #N = 1791
old_file <- subset(old_file, old_file$age <= 70) # N = 1700
old_file <- subset(old_file, old_file$Eng_little == "little") # N = 1172
## CHECK the numbers LITTLE####
sum(old_file$Nr) # N = 258,002

binned_old<-binme_old(data_old, elogit = T, old_file) # N = 1,170
binned_old <- subset(binned_old, binned_old$Eng_little == "little")
save(binned,file="data_binned.Rda")

table(binned_old$Eng_little)
names(binned_old)
table(binned_old$te[binned_old$groups == "non-immersion"])

#   4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 
#  55 59 58 62 58 60 60 59 58 57 56 55 54 45 50 36 49 27 29 29 26 29 20 20 20  4 35
 
#Fit the C and D models
assign("tofit", binned_old, envir=.GlobalEnv) # tofit n = 1170

output_flat_lit <- data.frame(r=NA,a=NA,d=NA,En=NA,tc=NA)




#####Continuous Model (tc=1)########################################################..
source("3L-learningcurves_functions LITTLE CONT LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_lit <- loss_flat_elogit_lit}

result_flat_lit <- DEoptim(loss_fn_flat_lit, lower = c(0,0,-50,1,1), upper = c(1,1,50,1,1),
        control = list(NP = 140, itermax = 2500))
fit_pars_flat_lit_c <- result_flat_lit[[1]]$bestmem
fit_pars_flat_lit_c
#        par1        par2        par3        par4        par5 
#   0.04228068  0.31855645 27.75153499  1.00000000  1.00000000
# Naggr = 1.170, Nindividual = 258.002

# Explained variances and AIC's
# CONTINU
R2cont <- 1 - (loss_fn_flat_lit(fit_pars_flat_lit_c) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2cont # R2 = 0.7370674 for  continuous (tc=1)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 178.266
SSEcont <- loss_fn_flat_lit(fit_pars_flat_lit_c)
SSEcont #<- 46.871957
n <- 1170
k <- 5
AICcont1 = n * log(SSEcont/n) + 2*k
AICcont1 # -3754.287

# Discontinuous model
source("3L-learningcurves_functions LITTLE DISC LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_lit <- loss_flat_elogit_lit}

result_flat_lit <- DEoptim(loss_fn_flat_lit, lower = c(0,0,-50,1,1), upper = c(1,1,50,1,40), 
                           control = list(NP = 140, itermax = 2500))
fit_pars_flat_lit_d <- result_flat_lit[[1]]$bestmem
fit_pars_flat_lit_d
########   Discontinu    ########
#   par1 (r)      par2 (a)  par3 (d)   par4 (En)   par5 (tc) 
#  0.06198456  0.10352185 -3.33808955  1.00000000 18.62513171  
# Naggr = 1.170, Nindividual = 258.002

# DISCONTINU
R2disc <- 1 - (loss_fn_flat_lit(fit_pars_flat_lit_d) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2disc # R2 = .8448878  for discontinuous (tc=18.63)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 178.266
SSEdisc <- loss_fn_flat_lit(fit_pars_flat_lit_d)
SSEdisc #<- 27.65124
n <- 1170
k <- 7
AICdisc1 = n * log(SSEdisc/n) + 2*k
AICdisc1 #< -4367.753

PROB1 <- exp((AICdisc1 - AICcont1)/2)
PROB1 # 6.131106e-134

PROB <- exp((AICcont1 - AICdisc1)/2)
PROB # 1.631027e+133


# The continuous model is 6.131106e-134 times as likely to minimize information loss than the D model
# In reverse, the D model is 1.631027e+133 times as likely, etc.
