library(trust)
library(mvtnorm)
library(Matrix)
library(parallel)
library(foreach)
library(doParallel)
library(iterators)
library(glmm)
## Loading required package: trust
## Loading required package: mvtnorm
## Loading required package: Matrix
data(salamander)
names(salamander)
## [1] "Mate" "Cross" "Female" "Male"
head(salamander)

summary(salamander)

set.seed(1234)
ptm<-proc.time()
sal <- glmm(Mate ~ 0 + Cross, random = list(~ 0 + Female,
                                            ~ 0 + Male), varcomps.names = c("F", "M"), data = salamander,
            family.glmm = bernoulli.glmm, m = 10^4, debug = TRUE)
proc.time() - ptm

summary(sal)
