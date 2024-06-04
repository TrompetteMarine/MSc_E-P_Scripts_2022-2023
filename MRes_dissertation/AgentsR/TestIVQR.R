library(IVQR)
library(quantreg)
library(MASS)
library(Formula)
data(CH04)
CH04$icat <- factor(CH04$icat)
CH04$ecat <- factor(CH04$ecat)

                    
model <- net_tfa ~ p401 | e401 | icat+ecat+a1+a2+a3+a4+marr+fsize+twoearn+db+pira+hown

taus <- 0.5
grid <- seq(0,25000,125)
qrMethod <- 'br'


fit_median <- ivqr(formula = iqr_model, taus=taus, data=CH04, grid=grid, qrMethod='br')


data(ivqr_eg)
fit <- ivqr(y ~ d | z | x, 0.5, grid = seq(-2,2,0.2), data = ivqr_eg) # median
fit <- ivqr(y ~ d | z | x, 0.25, grid = seq(-2,2,0.2), data = ivqr_eg) # the first quartile
fit <- ivqr(y ~ d | z | x, seq(0.1,0.9,0.1), grid = seq(-2,2,0.2), data = ivqr_eg) # a process
plot(fit) # plot the coefficients of the endogenous variable along with 95% CI