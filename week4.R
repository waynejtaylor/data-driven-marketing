library(tidyverse)
library(glmnet)
library(xgboost)

## Load data and prep
load('data/IowaHousing.Rdata')

y = as.matrix(house %>% select(logSalePrice))
x = as.matrix(house %>% select(-logSalePrice))

n = nrow(y)
set.seed(1);ind = sample(1:n,n*.75)

y.train = y[ind]
x.train = x[ind,]

y.test = y[-ind]
x.test = x[-ind,]

## Distribution of sale prices
ggplot(house,aes(x=logSalePrice)) + geom_histogram() + theme_bw(15) +
  xlab("log(Sale Price)") + ylab("") + ggtitle("Distribution of Sale Price")

## First try ridge regression
#set alpha = 0 for ridge
ridge = glmnet(x.train,y.train,alpha=0)
plot(ridge)

## Use cross-validation to select $\lambda$
ridge.cv = cv.glmnet(x.train,y.train,alpha=0)
s = ridge.cv$lambda.min #"best"
plot(ridge.cv);abline(v = log(s),col="red")
coef(ridge,s=s)

## How good is the prediction?
ridge_yhat = predict(ridge,newx=x.test,s=s)
#MSE
sqrt(mean((exp(ridge_yhat)-exp(y.test))^2))

#compare with OLS
ols = lm(y.train~x.train)
betahat = ols$coefficients
betahat[is.na(betahat)] = 0
ols_yhat = cbind(1,x.test) %*% betahat
sqrt(mean((exp(ols_yhat)-exp(y.test))^2))

## What about the variance?
var(ridge_yhat)
var(ols_yhat)

## Now try Lasso
#set alpha = 1 for lasso
lasso = glmnet(x.train,y.train,alpha=1)
plot(lasso)

## Use cross-validation to select $\lambda$
lasso.cv = cv.glmnet(x.train,y.train,alpha=1)
s = lasso.cv$lambda.min #"best"
plot(lasso.cv);abline(v = log(s),col="red")
coef(lasso,s=s)

## How good is the prediction?
lasso_yhat = predict(lasso,newx=x.test,s=s)
#MSE
sqrt(mean((exp(lasso_yhat)-exp(y.test))^2))



#Elastic net
enOut = data.frame(method = 'elasticnet',par = seq(0,1,.1),RMSE = NA)
enOut

for (a in 1:nrow(enOut)){
  
  alpha_a = enOut$par[a]
  en.cv   = cv.glmnet(x.train,y.train,alpha=alpha_a)
  s       = en.cv$lambda.min #"best"
  en_out   = glmnet(x.train,y.train,alpha=alpha_a)
  yhat_en  = predict(en_out,newx=x.test,s=s)
  RMSE     = sqrt(mean((y.test - yhat_en)^2))
  enOut$RMSE[a] = RMSE
  
  print(a)
}

ggplot(enOut,aes(par,RMSE)) + geom_line()


#XGBoost
#https://xgboost.readthedocs.io/en/stable/R-package/xgboostPresentation.html
xgbOut = data.frame(method = 'xgb', par = seq(0,1,.1),RMSE = NA)
xgbOut

for (e in 1:nrow(xgbOut)){
  eta_e    = xgbOut$par[e]
  xgb_e  = xgboost(data = x.train,label = y.train, nrounds = 20, eta = eta_e)
  yhat_xgb = predict(xgb_e,x.test)
  RMSE    = sqrt(mean((y.test - yhat_xgb)^2))
  xgbOut$RMSE[e] = RMSE
  print(e)
}

ggplot(xgbOut,aes(par,RMSE)) + geom_line()

#OLS
ols = lm(y.train~x.train)
betahat = ols$coefficients
betahat[is.na(betahat)] = 0
ols_yhat = cbind(1,x.test) %*% betahat
sqrt(mean((ols_yhat-y.test)^2))

#Plot all of them
outALL = bind_rows(
  olsOut = data.frame(method='ols',par = seq(0,1,.1),RMSE = sqrt(mean((ols_yhat-y.test)^2))),
  enOut,xgbOut
)

ggplot(outALL,aes(par,RMSE,color=method)) + 
  geom_line() +
  coord_cartesian(ylim=c(0,1))

#XGboost is worse, elasticnet is BARELY better than OLS