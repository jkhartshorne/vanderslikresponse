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

# This file fits the Continuous and Discontinuous models for immersion learners

ability_metric <- "elogit" 

library("DEoptim")
library("tidyverse")


source("2-aggregate_functions LL.R")
data_old <- get(load("tofit.RDa", verbose = T)) # 
data_old <- subset(data_old, data_old$t <=70)

old_file <- get(load("data_binned.Rda", verbose = T)) #N = 1791
old_file <- subset(old_file, old_file$age <= 70) 
old_file <- subset(old_file, old_file$Eng_little == "lot")
## CHECK the numbers LOT####
sum(old_file$Nr) # N = 14.099

binned_old<-binme_old(data_old, elogit = T, old_file) # N = 398
binned_old <- subset(binned_old, binned_old$Eng_little == "lot")
save(binned,file="data_binned.Rda")

table(binned_old$Eng_little)
names(binned_old)
table(binned_old$te[binned_old$groups == "immersion"])

#  2  5  8 11 14 17 20 23 26 29 ## This is the final distribution of immersion learners
# 61 61 59 48 35 28 27 25 26 28 



# # flat refers to the HTP model

output_flat_y <- data.frame(r=NA,a=NA,d=NA,Ei=NA,tc=NA)

#Fit the C and D models
assign("tofit", binned_old, envir=.GlobalEnv) # tofit n = 398

#####Continuous Model (tc=1)########################################################.
source("3i-learningcurves_functions LOT CONT LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_y <- loss_flat_elogit_y}


result_flat_y_c <- DEoptim(loss_fn_flat_y, lower = c(0,0,-50,1,1), upper = c(1,1,50,1,1),
        control = list(NP = 140, itermax = 2500))
fit_pars_flat_y_c <- result_flat_y_c[[1]]$bestmem
fit_pars_flat_y_c
# Continuous model######################################
#      par1       par2       par3       par4       par5 
#   0.1128999  0.1681332 34.2457261  1.0000000  1.0000000  tc =1
# Nagg = 398; Nindividual = 14.099

# Explained variances and AIC's
# CONTINUOUS MODEL
R2cont <- 1 - (loss_fn_flat_y(fit_pars_flat_y_c) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2cont # R2 = 0.8880097 for  continuous (tc=1)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 96.05193
SSEcont <- loss_fn_flat_y(fit_pars_flat_y_c)
SSEcont #<- 10.78658
n <- 398
k <- 5
AICcont1 = n * log(SSEcont/n) + 2*k
AICcont1 #<- 1427.141


# DISCONTINUOUS MODEL
source("3i-learningcurves_functions LOT DISC LL.R")
if (ability_metric == "elogit"){
  loss_fn_flat_y <- loss_flat_elogit_y}

result_flat_y_d <- DEoptim(loss_fn_flat_y, lower = c(0,0,-50,1,1), upper = c(1,1,50,1,40), 
                         control = list(NP = 140, itermax = 2500))
fit_pars_flat_y_d <- result_flat_y_d[[1]]$bestmem
fit_pars_flat_y_d
#   par1 (r)    par2 (a)     par3 (d)   par4 (Ei)   par5 (tc) #flat discontinuous
#   0.15144333  0.08209622  8.91784783  1.00000000 15.59296694  (tc = 15.59)
# Nagg = 398; Nindividual = 14.099

R2disc <- 1 - (loss_fn_flat_y(fit_pars_flat_y_d) / (sum((tofit$elogit-mean(tofit$elogit))^2))) #R^2 value
R2disc # R2 = 0.9220457  for discontinuous (tc=15.59)
SStot <- (sum((tofit$elogit-mean(tofit$elogit))^2))
SStot #<- 96.05193
SSEdisc <- loss_fn_flat_y(fit_pars_flat_y_d)
SSEdisc #<- 7.487661
n <- 398
k <- 7

AICdisc1 = n * log(SSEdisc/n) + 2*k
AICdisc1  #<- -1567.332

PROB1 <- exp((AICdisc1 - AICcont1)/2)
PROB1 #<- 3.613196e-31
PROB11 <- exp((AICcont1 - AICdisc1)/2)
PROB11 #< 2.767633e+30

# The continuous model is 3.613196e-31 times as likely to minimize information loss than the D model
# In reverse, the D model is 2.767633e+30 times as likely, etc.

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

