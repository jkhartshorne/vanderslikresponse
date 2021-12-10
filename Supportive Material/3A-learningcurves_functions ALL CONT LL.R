# This file is a modified version of the "helper-functions.R" script 
# that can be downloaded from https://osf.io/vab8j/

# We do not make use of the IRT and the piecewise sigmoid functions included in this script

# ----------------------- Model Fitting ----------------------------
# General note: we use flat to denote the original, HTP model from 2018


# The original sigmoid curve used in HTP 2018
# This function calculates the area under the curve
loss_curve_flat <- function(r,a,d,Eb,Ei,En,tc,te,t,group){
  E <- ifelse(group == "non-immersion",
              En,
              ifelse(group == "immersion",
                     Ei,
                     ifelse(group == "bilinguals",
                            Eb,
                            1))) # Monolinguals have immersion factor of 1
#  ifelse(t <= tc, ## INACTIVE RESULTS IN CONTINUOUS
#         -E * r * (t - te), # IDEM
         ifelse(t > tc, 
                -E * r * ((tc - te) + (t - tc) + (1/a * log((1 + exp(-a * d))/(1 + exp(a * (t - tc - d)))))), 
                -E * r * ((t - te) + (1/a * log((1 + exp(a * (te - tc -d)))/(1 + exp(a * (t - tc - d)))))))
}

# We use MSE for the error
loss_flat_elogit <-function(par){
  sum(((1-exp(loss_curve_flat(r = par[1], a = par[2], d = par[3], Eb = par[4], Ei = par[5], En = par[6], tc = par[7],
                             te = tofit$te, t = tofit$age, group = tofit$groups)))*2 + 1.5 - tofit$ability)^2)
}
# IRRELEVANT: IRT AND PIECEWISE ARE NOT OF CONCERN HERE
loss_flat_irt <-function(par){
  sum(((1-exp(loss_curve_flat(r = par[1], a = par[2],d = par[3],Eb = par[4], Ei = par[5], En = par[6], tc = par[7],
                              te = tofit$te, t = tofit$age, group = tofit$groups)))*2 - 1.5 - tofit$ability)^2)
}

# The piecewise sigmoidal form of the language learning curve
loss_curve_piecewise <- function(r,a1,a2,d1,d2,Eb,Ei,En,te,t,group){
  E <- ifelse(group == "non-immersion",
              En,
              ifelse(group == "immersion",
                     Ei,
                     ifelse(group == "bilinguals",
                            Eb,
                            1)))
  ta <- (a1 * d1 - a2 * d2)/(a1 - a2) # Point of intersection between the two sigmoids
  out <- ifelse(t <= ta,
         -E * r * ((t - te) + (1 / a1) * log((1 + exp(a1 * (te - d1))) / (1 + exp(a1 * (t - d1))))),
         ifelse(te < ta,
                -E * r * ((t - te) + (1 / a1) * log((1 + exp(a1 * (te - d1))) / (1 + exp(a1 * (ta - d1)))) + 
                        (1 / a2) * log((1 + exp(a2 * (ta - d2))) / (1 + exp(a2 * (t - d2))))),
                -E * r * ((t - te) + (1 / a2) * log((1 + exp(a2 * (te - d2))) / (1 + exp(a2 * (t - d2)))))
         )
  )
  return(out)
}


loss_piecewise_elogit <- function(par){
  sum(((1 - exp(loss_curve_piecewise(r = par[1], a1 = par[2], a2 = par[3], d1 = par[4],
                                     d2 = par[5], Eb = par[6], Ei = par[7], En = par[8], te = tofit$te, 
                                     t = tofit$age, group = tofit$groups)))*2 + 1.5 - tofit$ability)^2)
}

loss_piecewise_irt <- function(par){
  sum(((1 - exp(loss_curve_piecewise(r = par[1], a1 = par[2], a2 = par[3], d1 = par[4],
                                     d2 = par[5], Eb = par[6], Ei = par[7], En = par[8],
                                     te = tofit$te, t = tofit$age, group = tofit$groups)))*2- 1.5 - tofit$ability)^2)
}

# Loss function with lower horizontal asymptote - see supplementary materials of paper
loss_curve_piecewise_asymptote <- function(r,a1,a2,d1,d2,e,Eb,Ei,En,te,t,group){
  E <- ifelse(group == "non-immersion",
              En,
              ifelse(group == "immersion",
                     Ei,
                     ifelse(group == "bilinguals",
                            Eb,
                            1)))
  ta <- (a1*d1- a2*d2)/(a1-a2)
  ifelse(t <= ta,
         -E*(r*((t-te) + 1/a1 * log((1+exp(a1*(te-d1)))/(1+exp(a1*(t-d1))))) + e*(t-te)),
         ifelse(te < ta,
                -E*(r*((t - te) + (1/a1)*log((1+exp(a1*(te-d1)))/(1+exp(a1*(ta-d1))))+ 
                        (1/a2*log((1+exp(a2*(ta-d2)))/(1+exp(a2*(t-d2)))))) + e*(t-te)),
                -E*(r*((t-te) + 1/a2*log((1+exp(a2*(te-d2)))/(1+exp(a2*(t-d2))))) + e*(t-te))
         )
  )
}

loss_piecewise_asymptote <- function(par){
  sum(((1 - exp(loss_curve_piecewise_asymptote(r=par[1],a1=par[2],a2=par[3],d1=par[4],
                                     d2 = par[5],e=par[6],Eb=par[9],Ei=par[8],En=par[8],
                                     te=tofit$te,t=tofit$age,group=tofit$groups)))*2 + 1.5 - tofit$ability)^2)
}

#-------------------------------------------Plotting-----------------------------------------------------
# Evaluate the piecewise flat (HTP Model) language learning curve at a given age, conditioned on a set of parameters
piecewise_1 <- function(par,t){
  # par is a vector containing r, a, d, tc
  ifelse(t <= par[4],
         par[1],
         par[1] - par[1]/(1+exp(-par[2]*(t-par[3]-par[4]))))
}

# Evaluate the piecewise sigmoid learning curve at a given age, conditioned on a set of parameters
piecewise_2 <- function(par,t){
  # par is a vector containing r,a1,a2,d1,d2
  ta <- (par[2]*par[4]- par[3]*par[5])/(par[2] - par[3])
  ifelse(t <= ta,
         par[1]*(1 - 1/(1+exp(-par[2]*(t-par[4])))),
         par[1]*(1-1/(1+exp(-par[3]*(t-par[5])))))
}

# Plot the ultimate attainment curves
predicted_curve_piecewise <- function(r,a1,a2,d1,d2,E,te,t){
  ta <- (a1*d1- a2*d2)/(a1-a2)
  ifelse(t <= ta,
         -E*r*((t-te)+1/a1*log((1+exp(a1*(te-d1)))/(1+exp(a1*(t-d1))))),
         ifelse(te < ta,
                -E*r*((t - te) + (1/a1)*log((1+exp(a1*(te-d1)))/(1+exp(a1*(ta-d1))))+ 
                        (1/a2*log((1+exp(a2*(ta-d2)))/(1+exp(a2*(t-d2)))))),
                -E*r*((t-te) + 1/a2*log((1+exp(a2*(te-d2)))/(1+exp(a2*(t-d2)))))
         )
  )
}

prediction_piecewise <- function(r,a1,a2,d1,d2,E,te,t){
  (1 - exp(predicted_curve_piecewise(r,a1,a2,d1,d2,E,te,t)))*2 + 1.5
}

prediction_piecewise_irt <- function(r,a1,a2,d1,d2,E,te,t){
  (1 - exp(predicted_curve_piecewise(r,a1,a2,d1,d2,E,te,t)))*2 - 1.5
}

predicted_curve_flat <- function(r,a,d,E,tc,te,t){
  ifelse(t <= tc,
         -E*r*(t-te),
         ifelse(t > tc & te <= tc,
                -E*r*((tc-te) + (t-tc) + (1/a*log((1+exp(-a*d))/(1+exp(a*(t-tc-d)))))),
                -E*r*((t-te) + (1/a*log((1+exp(a*(te-tc-d)))/(1+exp(a*(t-tc-d))))))))
}

prediction_flat <- function(r,a,d,E,tc,te,t){
  (1- exp(predicted_curve_flat(r,a,d,E,tc,te,t)))*2 + 1.5
}

prediction_flat_irt <- function(r,a,d,E,tc,te,t){
  (1- exp(predicted_curve_flat(r,a,d,E,tc,te,t)))*2 - 1.5
}