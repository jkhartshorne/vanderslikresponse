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


# This file fits the Continuous and Discontinuous models for ALL groups simultaneously 
# No subset


ability_metric <- "elogit" 

library("DEoptim")
library("tidyverse")


source("2-aggregate_functions LL.R")
data_old <- get(load("tofit.RDa", verbose = T))
data_old <- subset(data_old, data_old$t <=70)

old_file <- get(load("data_binned.Rda", verbose = T)) #N = 1791
old_file <- subset(old_file, old_file$age <= 70) # N = 1700
sum(old_file$Nr) # N = 547,288 (was 547,250. A difference of 38)
## CHECK the numbers PER GROUP####
old_bileng <- subset(old_file, old_file$Eng_little == "bileng") 
sum(old_bileng$Nr) # N = 30,347 (Added up with 14,099 (lot) = 44,446, should be 44,412)

old_mono <- subset(old_file, old_file$Eng_little == "monoeng") 
sum(old_mono$Nr) # N = 244,840
old_non <- subset(old_file, old_file$Eng_little == "little") 
sum(old_non$Nr) # N = 258,002 (was 257,998) 
old_imm <- subset(old_file, old_file$Eng_little == "lot") 
sum(old_imm$Nr) # N = 14,099 
binned_old<-binme_old(data_old, elogit = T, old_file) # N = 1.696 aggregated data points
save(binned_old,file="binned_old.Rda")

names(binned_old)
table(binned_old$Eng_little)
#  bileng  little   lot    monoeng 
#    64      1170   398      64



# flat refers to the reported HTP model
output_flat <- data.frame(r=NA,a=NA,d=NA,Eb=NA,Ei=NA,En=NA,tc=NA)

#Fit the C and D models
assign("tofit", binned_old, envir=.GlobalEnv) # tofit n = 1696

#####Continuous Model (tc=1)########################################################.
source("3A-learningcurves_functions ALL CONT LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat <- loss_flat_elogit}
#CONTINUOUS # Run time: about 6 minutes
### NP = 140 and itermax = 2500 (in HTP: NP = 70, itermax = 500)
result_flat_c <- DEoptim(loss_fn_flat, lower = c(0,0,-50,0,0,0,1), upper = c(1,1,50,1,1,1,1),
                       control = list(NP = 140, itermax = 2500))
fit_pars_flat_c <- result_flat_c[[1]]$bestmem
fit_pars_flat_c

#      par1       par2       par3       par4       par5       par6       par7 
#    0.1556507  0.2410229 30.7985948  0.7012873  0.8007958  0.2362337  1.0000000
#   Naggr = 1696, Nindiv = 547.288, R2 = .87

# Explained variances and AIC's
# 
R2cont <- 1 - (loss_fn_flat(fit_pars_flat_c) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2cont # R2 = 0.8738215 for continuous (tc=1)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 550.3572
SSEcont <- loss_fn_flat(fit_pars_flat_c)
SSEcont #<- 69.44326
n <- 1696
k <- 7
AICcont1 = n * log(SSEcont/n) + 2*k
AICcont1 #<- -5405.598


#DISCONTINUOUS
source("3A-learningcurves_functions ALL DISC LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat <- loss_flat_elogit}

# Run time: 6 minutes
result_flat_d <- DEoptim(loss_fn_flat, lower = c(0,0,-50,0,0,0,0), upper = c(1,1,50,1,1,1,40),
                       control = list(NP = 140, itermax = 2500))
fit_pars_flat_d <- result_flat_d[[1]]$bestmem
fit_pars_flat_d

#        par1        par2        par3        par4        par5        par6        par7 
#       par1        par2        par3        par4        par5        par6        par7 
#    0.19121257  0.08883256 -0.43680519  0.63381128  1.00000000  0.29231550 18.00000000
#   Naggr = 1696, Nindiv = 547.288, R2 = .92

# Explained variances and AIC's

R2disc <- 1 - (loss_fn_flat(fit_pars_flat_d) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2disc # R2 = 0.9187988  for discontinuous (tc=18.0000)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 550.3572
SSEdisc <- loss_fn_flat(fit_pars_flat_d)
SSEdisc #<- 44.77841
n <- 1696
k <- 9 # 7 + 2 df
AICdisc1 = n * log(SSEdisc/n) + 2*k
AICdisc1 #<- -6149.141


PROB1 <- exp((AICdisc1 - AICcont1)/2)
PROB1 # 3.481753e-162
PROB11 <- exp((AICcont1 - AICdisc1)/2)
PROB11 # 2.872116e+161

# The discontinuous model is 2.872116e+161 times as likely to minimize information loss
# In reverse, the C model is nearly null times as likely, etc.


# convergence plot for the discontinuous model
par(mar=rep(2,4))
plot(result_flat_d, plot.type = "bestvalit", type='b')

