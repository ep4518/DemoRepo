#set working directory
setwd("")


library("parallel")
library("zoo")
library("xts")
library("ggplot2")
library("strucchange")
library("urca")
library("lmtest")
library("forecast")
library ("aTSA")
library("rugarch")
library("rmgarch")
library ("vars")

data <- readRDS(data, file = "datadaily.rds")
SMPL3 <- data['2022/2023-06']
SMPL2 <- data['2020/2021']
SMPL1 <- data['2016/2019']

plot(index(data),data$EUA, type="l")
plot(index(data),data$NGas, type="l")
plot(index(data),data$BRT, type="l")

plot(index(SMPL3),SMPL3$EUA, type="l")
plot(index(SMPL3),SMPL3$NGas, type="l") 
plot(index(SMPL3),SMPL3$BRT, type="l")

plot(index(SMPL2),SMPL2$EUA, type="l")
plot(index(SMPL2),SMPL2$NGas, type="l")
plot(index(SMPL2),SMPL2$BRT, type="l")

plot(index(SMPL1),SMPL1$EUA, type="l")
plot(index(SMPL1),SMPL1$NGas, type="l")
plot(index(SMPL1),SMPL1$BRT, type="l")

data <- SMPL1

# rugarch.test3a()
##############################################################
# Fitting ARMA as the mean model 
##############################################################
theme_set(theme_classic())
acf(data$rEUA, lag.max = "20")
pacf(data$rEUA, lag.max = "20")
arma_rEUA<- arima(data$rEUA, c(2,0,1), method= "ML") 
auto_rEUA <- auto.arima(data$rEUA, stepwise = FALSE, method= "ML", ic=c("aic"))
arma_rEUA
auto_rEUA
lrtest(arma_rEUA, auto_rEUA)
plot.ts(auto_rEUA$residuals) 
checkresiduals(auto_rEUA)
##ARCH LM test: does the arma models have ARCH effect in the residuals?
arch.test(arima(data$rEUA, c(0,0,2), method= "ML"))
# "we reject the null and so there is arch effect and ARMA is not the best model
# for this data"
##############################################################
# Fitting ARMA as the mean model 
##############################################################

acf(data$rNGas, lag.max = "20")
pacf(data$rNGas, lag.max = "20")
arma_rNGas<- arima(data$rNGas, c(5,0,4), method= "ML") 
auto_rNGas <- auto.arima(data$rNGas, stepwise = FALSE, method= "ML", ic=c("aic"))
auto_rNGas
arma_rNGas
lrtest (arma_rNGas, auto_rNGas)
plot.ts (auto_rNGas$residuals) 
checkresiduals (auto_rNGas)
##ARCH LM test: does the arma models have ARCH effect in the residuals?
arch.test(arima(data$rNGas, c(0,0,2), method= "ML"))
# "we reject the null and so there is arch effect and ARMA is not the best model
# for this data"
##############################################################
# Fitting ARMA as the mean model 
##############################################################

acf(data$rBRT, lag.max = "20")
pacf(data$rBRT, lag.max = "20")
arma_rBRT<- arima(data$rBRT, c(2,0,0), method= "ML") 
auto_rBRT <- auto.arima(data$rBRT, stepwise = FALSE, method= "ML", ic=c("aic"))
auto_rBRT
arma_rBRT
lrtest (arma_rBRT, auto_rBRT)
plot.ts (auto_rBRT$residuals) 
checkresiduals (auto_rBRT)
##ARCH LM test: does the arma models have ARCH effect in the residuals?
arch.test(arima(data$rBRT, c(0,0,2), method= "ML"))


auto_rEUA$arma; auto_rNGas$arma; auto_rBRT$arma

###############################################################
# Task 1: Fitting GARCH(1,1) models for NGas price returns and S&P500 returns
##############################################################
# garch_spec_1 = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), submodel = "GARCH"), 
#                           mean.model = list(armaOrder = c(2,2), include.mean = TRUE), distribution.model = "std")
# garch_spec_2 <- ugarchspec(variance.model=list(model="iGARCH", garchOrder= c(1,1)),
#                            mean.model= list(armaOrder=c(4,5)), distribution.model = "std")
# garch_spec_3 <- ugarchspec(variance.model=list(model="apARCH", garchOrder= c(1,1)),
#                            mean.model= list(armaOrder=c(0,0)),distribution.model = "sstd")
# setfixed(garch_spec_3) <- list(delta = 1, beta1 = 0.923476)
# garch_spec_1 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder= c(1,1)),
#                            mean.model= list(armaOrder=c(0,0)), distribution.model = "sstd")
# garch_spec_2 <- ugarchspec(variance.model=list(model="iGARCH", garchOrder= c(1,1)),
#                            mean.model= list(armaOrder=c(0,2)), distribution.model = "std")
# garch_spec_3 <- ugarchspec(variance.model=list(model="apARCH", garchOrder= c(1,1)),
#                            mean.model= list(armaOrder=c(0,2)),distribution.model = "sstd")
# garch_spec_1 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder= c(1,1)),
#                            mean.model= list(armaOrder=c(1,0)), distribution.model = "std")
# garch_spec_2 <- ugarchspec(variance.model=list(model="apARCH", garchOrder= c(1,1)),
#                            mean.model= list(armaOrder=c(2,2)), distribution.model = "std")
# setfixed(garch_spec_2) <- list(delta = 1)
# garch_spec_3 <- ugarchspec(variance.model=list(model="apARCH", garchOrder= c(1,1)),
                           # mean.model= list(armaOrder=c(3,2)),distribution.model = "std")
# setfixed(garch_spec_3) <- list(delta = 1)
# Estimate GARCH (1,1) models and save the estimates. Note that we
# are using the same mean model specification as in Lab 6, following the
# ARMA analysis in Lab 3. Identify the mean and the variance equations and interpret the estimation results. Do you think the stability
# condition of volatility is satisfied? How do you know?

garch_spec_1 <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), submodel = "GARCH"), 
                           mean.model = list(armaOrder = c(2,2), include.mean = TRUE), distribution.model = "std")
garch_spec_2 <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)), 
                           mean.model = list(armaOrder = c(4,5), include.mean = TRUE), distribution.model = "std")
garch_spec_3 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), 
                           mean.model = list(armaOrder = c(0,0), include.mean = TRUE), distribution.model = "sstd")
garch_spec_1 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1,1)), 
                                  mean.model = list(armaOrder = c(0,0), include.mean = TRUE), distribution.model = "sstd")
garch_spec_2 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), 
                           mean.model = list(armaOrder = c(0,2), include.mean = TRUE), distribution.model = "std")
garch_spec_3 <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), submodel = "TGARCH"), 
                           mean.model = list(armaOrder = c(0,2), include.mean = TRUE), distribution.model = "sstd")
garch_spec_1 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), 
                           mean.model = list(armaOrder = c(1,0), include.mean = TRUE), distribution.model = "std")
garch_spec_2 <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), submodel = "TGARCH"), 
                           mean.model = list(armaOrder = c(2,2), include.mean = TRUE), distribution.model = "std")
garch_spec_3 <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), submodel = "TGARCH"), 
                           mean.model = list(armaOrder = c(3,2), include.mean = TRUE), distribution.model = "std")

garch_rEUA <- ugarchfit(garch_spec_1,data=data$rEUA)
garch_rNGas <- ugarchfit(garch_spec_2,data=data$rNGas)
garch_rBRT <- ugarchfit(garch_spec_3,data=data$rBRT)
# names(garch_rNGas@fit)

garch_rEUA
# plot(garch_rNGas)
theme_set(theme_classic())

plot(garch_rEUA, which="all")
plot(garch_rNGas, which="all")
plot(garch_rBRT, which = "all")

ht_rEUA <- garch_rEUA@fit$var
ht_rNGas <- garch_rNGas@fit$var
ht_rBRT <- garch_rBRT@fit$var



plot(ht_rEUA, type="l", ylab = "EUA Returns Volatility")
plot(ht_rNGas, type="l", ylab = "TTF Returns Volatility")
plot(ht_rBRT, type="l", ylab = "BRT Returns Volatility")
# solver.control = list(tol =1e-7)
# , solver='hybrid'
# par(mfrow=c(3,3))
# plot(ht_rEUA1, type="l", ylab = "EUA Returns Volatility")
# plot(ht_rNGas1, type="l", ylab = "TTF Returns Volatility", ylim=c(0,0.05))
# plot(ht_rBRT1, type="l", ylab = "BRT Returns Volatility")
# plot(ht_rEUA2, type="l", ylab = "EUA Returns Volatility", ylim=c(0,0.0025))
# plot(ht_rNGas2, type="l", ylab = "TTF Returns Volatility", ylim=c(0,0.02))
# plot(ht_rBRT2, type="l", ylab = "BRT Returns Volatility", ylim=c(0,0.003))
# plot(ht_rEUA3, type="l", ylab = "EUA Returns Volatility", ylim=c(0,0.005))
# plot(ht_rNGas3, type="l", ylab = "TTF Returns Volatility", ylim=c(0,0.05))
# plot(ht_rBRT3, type="l", ylab = "BRT Returns Volatility")

ADF_htEUA <- ur.df(ht_rEUA, type="trend", selectlags="AIC")
summary(ADF_htEUA)
ADF_htNGas <- ur.df(ht_rNGas, type="trend", selectlags="AIC")
summary(ADF_htNGas)
ADF_htBRT <- ur.df(ht_rBRT, type="trend", selectlags="AIC")
summary(ADF_htBRT)

# Estimate the VAR model of the two volatility estimates, test using Granger
# causality test, interpret using impulse response function, and check for model
# diagnostics. Comment on the hedging capability of S&P500 returns for NGas
# price returns.
# VAR/VECM with univariate GARCH variance doesn’t use the non-linear combinations

var_data <- cbind(ht_rEUA, ht_rNGas)
VARselect(var_data, lag.max=30, type = "both")
VAR_est <- VAR(y = var_data,lag.max=30, ic="AIC")
#Granger Causality test
causality(VAR_est, cause="ht_rEUA")$Granger
causality(VAR_est, cause="ht_rNGas")$Granger
#The impulse response function (IRF)
irf <- irf(VAR_est, impulse="ht_rEUA", response=c("ht_rNGas"))
summary(VAR_est)$varresult$ht_rEUA$sigma
plot(irf)
irf <- irf(VAR_est, impulse="ht_rNGas", response=c("ht_rEUA"))
summary(VAR_est)$varresult$ht_rNGas$sigma
plot(irf)
#Diagnostics
serial.test(VAR_est,lags.pt = 3, type = "BG")
normality.test(VAR_est, multivariate.only = TRUE)
VAR_estStabil <- stability(VAR_est)
plot(VAR_estStabil, type="l")
arch.test(VAR_est,lags.multi=1, multivariate.only=TRUE)

# Estimate the VAR model of the two volatility estimates, test using Granger
# causality test, interpret using impulse response function, and check for model
# diagnostics. Comment on the hedging capability of S&P500 returns for NGas
# price returns.

var_data <- cbind(ht_rEUA, ht_rBRT)
VARselect(var_data, lag.max=30, type = "both")
VAR_est <- VAR(y = var_data,lag.max=30, ic="AIC")
#Granger Causality test
causality(VAR_est, cause="ht_rEUA")$Granger
causality(VAR_est, cause="ht_rBRT")$Granger
#The impulse response function (IRF)
irf1 <- irf(VAR_est, impulse="ht_rEUA", response=c("ht_rBRT"))
summary(VAR_est)$varresult$ht_rEUA$sigma
plot(irf1, ylab = "BRT Returns Volatility", main = "")
# Orthogonal Impulse Response from EUA Returns Volatility
irf2 <- irf(VAR_est, impulse="ht_rBRT", response=c("ht_rEUA"))
summary(VAR_est)$varresult$ht_rBRT$sigma
plot(irf2, ylab = "EUA Returns Volatility", main = "")
#Diagnostics
serial.test(VAR_est,lags.pt = 3, type = "BG")
normality.test(VAR_est, multivariate.only = TRUE)
VAR_estStabil <- stability(VAR_est)
plot(VAR_estStabil, type="l")
detach("package:aTSA", unload = TRUE)
arch.test(VAR_est,lags.multi=1, multivariate.only=TRUE)

###############################################################
# Task 2: Dynamic Conditional Correlations (DCC) GARCH model
###############################################################
# Let us now estimate a system of GARCH model with the two variables and
# estimate their dynamic conditional correlation. Let us use the three lag
# structure of the mean model from above and a no constant model, as we are
# using the returns variables. For R, we use the package developed by Galanos
# (2019).
auto_rEUA$arma; auto_rNGas$arma; auto_rBRT$arma
# uspec.n = multispec(c(ugarchspec(mean.model = list(armaOrder = c(1,0))),
#                       ugarchspec(mean.model = list(armaOrder = c(0,2))),
#                       ugarchspec(mean.model = list(armaOrder = c(2,0)))))
# uspec.n = multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(3,0)))))
data<- cbind(data$rEUA, data$rNGas, data$rBRT)
uspec.n = multispec(c(garch_spec_1,garch_spec_2,garch_spec_3))
multf = multifit(uspec.n, data)
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), model = c("DCC") ,distribution = 'mvt')
fit1 = dccfit(spec1, data = data, fit.control = list(eval.se = TRUE), fit = multf)
fit1

plot(fit1, which = 4)
# plot(fit1, which=4, ask=F, main="Canada")

# Test of non-constant correlation based on Engle III and Sheppard (2001):
DCCtest(data, garchOrder = c(1,1), n.lags = 100, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)
#forecast
# plot(dccforecast(fit1, n.ahead=10))

# Get the model based time varying covariance (arrays) and correlation matrices
cov1 = rcov(fit1) # extracts the covariance matrix within sample
cor1 = rcor(fit1) # extracts the correlation matrix within sample
# dim(cor1) #dimension of dynamic conditional correlation

cor1[,,dim(cor1)[2]]
corr_rEUA_rNGas=cor1[2,1,] # row 2, column 1, observations between 1100 to 1227
corr_rEUA_rNGas<- as.xts(corr_rEUA_rNGas)
plot(corr_rEUA_rNGas, type = "l", grid=FALSE)
summary(corr_rEUA_rNGas)
sd(corr_rEUA_rNGas)

corr_rEUA_rBRT=cor1[3,1,] # row 2, column 1, observations between 1100 to 1227
corr_rEUA_rBRT<- as.xts(corr_rEUA_rBRT)
plot(corr_rEUA_rBRT, type="l", minor.ticks= FALSE)
summary(corr_rEUA_rBRT)
sd(corr_rEUA_rBRT)
# plot(cov1[2,1,], type='l')

par(mfrow=c(1,2))
plot(as.xts(cor1[2,1,]), main = "")
plot(as.xts(cor1[3,1,]), main = "")



