
# -----------------------------------------------------------
# | Model selection in MLD Model fitted on NESDA data
# |   May 17, 2016
# -----------------------------------------------------------

## load dependency library
# require(geepack)
# require(Formula)


## load library
library(mldm)

## load data
data("NESDA")


## ------------------------------------------------------------
## Model 1: distress/fear model, independence + ALL predictors
##  AICq_1 = 12,396.42
##  BICq_1 = 12,522.12
## ------------------------------------------------------------
Z1 <- matrix(c(1,1,1,0,0,0,0,0,1,1),5,2,byrow = FALSE)

mf1 <- Outcome | Outcome ~ EDU + GEN + AGE + N + E + O + A + C | EDU + GEN + AGE + N + E + O + A + C 
mF1 <- Formula(mf1)

out1 <- mldm.fit(formula=mF1, index = Index, resp.dim.ind = Z1, 
                 data = NESDA, id = pident, Rusr = "independence", scale=TRUE)
out1

AICq_1 <- out1$deviance + 2 * out1$npar; AICq_1
BICq_1 <- out1$deviance + log(length(unique(NESDA$pident))) * out1$npar; BICq_1
out1$npar

## -----------------------------------------------------------------
## Model 2: depression/anxiety model, independence + ALL predictors
##  AICq_2 = 12,398.08
##  BICq_2 = 12,523.78
## ----------------------------------------------------------------
Z2 <- matrix(c(1,1,0,0,0,0,0,1,1,1),5,2,byrow = FALSE)

mf2 <- Outcome | Outcome ~ EDU + GEN + AGE + N + E + O + A + C | EDU + GEN + AGE + N + E + O + A + C 
mF2 <- Formula(mf2)

out2 <- mldm.fit(formula=mF2, index = Index, resp.dim.ind = Z2, 
                 data = NESDA, id = pident, Rusr = "independence", scale=TRUE)
out2

AICq_2 <- out2$deviance + 2 * out2$npar; AICq_2
BICq_2 <- out2$deviance + log(length(unique(NESDA$pident))) * out2$npar; BICq_2
out2$npar

## -----------------------------------------------------------------
## Model 3: unidimentional model, independence + ALL predictors
##  AICq_3 = 12,418.87
##  BICq_3 = 12,496.68
## ----------------------------------------------------------------
Z3 <- matrix(1,5,1,byrow = FALSE)

mf3 <- Outcome ~ EDU + GEN + AGE + N + E + O + A + C
mF3 <- Formula(mf3)

out3 <- mldm.fit(formula=mF3, index = Index, resp.dim.ind = Z3, 
                 data = NESDA, id = pident, Rusr = "independence", scale=TRUE)
out3

AICq_3 <- out3$deviance + 2 * out3$npar; AICq_3
BICq_2 <- out3$deviance + log(length(unique(NESDA$pident))) * out3$npar; BICq_2
out3$npar


## ------------------------------------------------------------------------------ ##
## --- 2D vs 1D: distress/fear model is selected with AIC_q=12,396.42.            ##
## ------------------------------------------------------------------------------ ##


## ---------------------------------------------------------------------------
## Model 1a: distress/fear model, independence + ALL predictors
##  BICq_1a = 12,522.12
##  AICq_1a = 12,396.42 
## ---------------------------------------------------------------------------
Z1a <- matrix(c(1,1,1,0,0,0,0,0,1,1),5,2,byrow = FALSE)

mf1a <- Outcome | Outcome ~ EDU + GEN + AGE + N + E + O + A + C | EDU + GEN + AGE + N + E + O + A + C
mF1a <- Formula(mf1a)

out1a <- mldm.fit(formula=mF1a, index = Index, resp.dim.ind = Z1a, 
                  data = NESDA, id = pident, Rusr = "independence", scale=TRUE)
out1a

BICq_1a <- out1a$deviance + log(length(unique(NESDA$pident))) * out1a$npar; BICq_1a
AICq_1a <- out1a$deviance + 2 * out1a$npar; AICq_1a
out1a$npar

## ---------------------------------------------------------------------------
## Model 1b: distress/fear model, independence + (EDU,GEN,AGE,N,E) predictors
##  BICq_1b = 12,486.46
##  AICq_1b = 12,396.68
## ---------------------------------------------------------------------------
Z1b <- matrix(c(1,1,1,0,0,0,0,0,1,1),5,2,byrow = FALSE)

mf1b <- Outcome | Outcome ~ EDU + GEN + AGE + N + E | EDU + GEN + AGE + N + E
mF1b <- Formula(mf1b)

out1b <- mldm.fit(formula=mF1b, index = Index, resp.dim.ind = Z1b, 
                  data = NESDA, id = pident, Rusr = "independence", scale=TRUE)
out1b

BICq_1b <- out1b$deviance + log(length(unique(NESDA$pident))) * out1b$npar; BICq_1b
AICq_1b <- out1b$deviance + 2 * out1b$npar; AICq_1b
out1b$npar


## ---------------------------------------------------------------------
## Model 1c: distress/fear model, independence + demographic predictors
##  BICq_1c = 14,855.25
##  AICq_1c = 14,789.41
## ---------------------------------------------------------------------
Z1c <- matrix(c(1,1,1,0,0,0,0,0,1,1),5,2,byrow = FALSE)

mf1c <- Outcome | Outcome ~ EDU + GEN + AGE | EDU + GEN + AGE
mF1c <- Formula(mf1c)

out1c <- mldm.fit(formula=mF1c, index = Index, resp.dim.ind = Z1c, 
                  data = NESDA, id = pident, Rusr = "independence", scale=TRUE)
out1c

BICq_1c <- out1c$deviance + log(length(unique(NESDA$pident))) * out1c$npar; BICq_1c
AICq_1c <- out1c$deviance + 2 * out1c$npar; AICq_1c
out1c$npar


## --------------------------------------------------------------------
## Table 3. Estimates of the final 2-dimensional MLD model, namely d/f
## --------------------------------------------------------------------
# Z1b <- matrix(c(1,1,1,0,0,0,0,0,1,1),5,2,byrow = FALSE)
# 
# mf1b <- Outcome | Outcome ~ EDU + GEN + AGE + N + E | EDU + GEN + AGE + N + E
# mF1b <- Formula(mf1b)
# 
# out1b <- mldm.fit(formula=mF1b, index = Index, resp.dim.ind = Z1b, 
#                   data = NESDA, id = pident, Rusr = "independence", scale=TRUE)
# 
# summary(out1b)

## based on standardized predictors
Z1a <- matrix(c(1,1,1,0,0,0,0,0,1,1),5,2,byrow = FALSE)

mf1a <- Outcome | Outcome ~ EDU + GEN + AGE + N + E + O + A + C | EDU + GEN + AGE + N + E + O + A + C
mF1a <- Formula(mf1a)

## standardized model
out1a <- mldm.fit(formula=mF1a, index = Index, resp.dim.ind = Z1a, 
                  data = NESDA, id = pident, Rusr = "independence", scale=TRUE)

summary(out1a)


## unstandardized model
out1a_unstand<- mldm.fit(formula=mF1a, index = Index, resp.dim.ind = Z1a, 
                         data = NESDA, id = pident, Rusr = "independence", scale=FALSE, center = FALSE)

summary(out1a_unstand)



## bootstrap results
out1a_boot_unstand <- mldm.fit(formula=mF1a, index = Index, resp.dim.ind = Z1a, 
                               data = NESDA, id = pident, Rusr = "independence", 
                               scale=FALSE, center = FALSE,
                               bootstrap = 1000)

summary(out1a_boot_unstand, bootstrap=TRUE)









## -----------------------------------------------------------------
## Appendix 1. The final model which was selected in old manuscript
## -----------------------------------------------------------------

Z_old <- matrix(c(1,1,0,0,0,0,0,1,1,1),5,2,byrow = FALSE)

mf_old <- Outcome | Outcome ~ EDU + GEN + AGE + N + E | EDU + GEN + AGE + N + E
mF_old <- Formula(mf_old)
# 
## independence
out_indep <- mldm.fit(formula=mF_old, index = Index, resp.dim.ind = Z_old, 
                      data = NESDA, id = pident, Rusr = "independence", scale=TRUE)

## exchangeable
out_exch <- mldm.fit(formula=mF_old, index = Index, resp.dim.ind = Z_old, 
                     data = NESDA, id = pident, Rusr = "exchangeable", scale=TRUE)

## compare candidate models
QIC.mldm(out_exch, out_indep)

