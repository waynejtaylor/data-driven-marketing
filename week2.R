library(tidyverse)
library(kableExtra)
library(broom)
showlm = function(obj) tidy(obj) %>% select(1,2,3,5) %>% mutate(across(-1, ~round(., 3)))
## See it in R...
  
var_t = 5 #variance outcomes
var_c = 2
n_t = 100 #number of observations
n_c = 100
tau = 13  #treatment estimate
a = .05   #significance level
z = abs(qnorm(a/2)) #1.96

var_tau = var_t/n_t + var_c/n_c
var_tau; tau - z*sqrt(var_tau); tau + z*sqrt(var_tau)

## Double $n$

n_t = 100*2
n_c = 100*2

var_tau = var_t/n_t + var_c/n_c
var_tau; tau - z*sqrt(var_tau); tau + z*sqrt(var_tau)

## Or half the variance in $Y$

var_t = 5/2 #halved
var_c = 2/2
n_t = 100   #reset
n_c = 100

var_tau = var_t/n_t + var_c/n_c
var_tau; tau - z*sqrt(var_tau); tau + z*sqrt(var_tau)

## Given a fixed $n$, better to have an even split

getTau  = function(p = .5,n = 1000) {
  var_t/(p*n) + var_c/((1-p)*n)}
pLevels = seq(0.01,.99,.01)
splitN = data.frame(pLevels,var = getTau(pLevels))

ggplot(splitN,aes(pLevels,var)) + 
  geom_line() + 
  theme_minimal()

## General rule of thumb

## Or in R

library(pwr)
pwr.t.test(d = (46-40)/18,
           sig.level = .05,
           power = .8)

## More realistic

h = ES.h(.02,.0202) ##Cohen's d for proportions 
pwr.p.test(h = h,
           sig.level = 0.05,    
           power = 0.8,         
           alternative = "two.sided")

## Toy example
pop     = 10000
mobile  = rep(1:0,c(pop*.9,pop*.1))
d = sample(0:1,pop,TRUE)
treatment_effect = 3
sales =  10 + treatment_effect*d + 12*mobile + 
  rnorm(pop,0,4)
pop_df = data.frame(sales,d,mobile)
#regular sample
n = 300
set.seed(1);reg_df = pop_df %>% sample_n(n)
prop.table(table(reg_df$mobile))

## Stratified Sample

n_mobile  = n*.9
n_desktop = n*.1

ss_df = bind_rows(
  pop_df %>% filter(mobile == 1) %>%
    sample_n(n_mobile),
  pop_df %>% filter(mobile == 0) %>%
    sample_n(n_desktop))

prop.table(table(ss_df$mobile))

## Compare ATE variance...

showlm(lm(sales ~ d,reg_df))
showlm(lm(sales ~ d,ss_df))

## Booking.com Example

load('data/booking.rdata')
str(booking)

## Booking ATE

y1       = booking$bookings
treated  = booking$treated

showlm(lm(y1~treated))

## Booking CUPED

y0 = booking$bookings_pre
lm_booking = lm(y1 ~ y0)
theta      = coef(lm_booking)[2]
y1_cuped   = y1 - theta*(y0 - mean(y0))
showlm(lm(y1_cuped ~ treated))

## Why not just add $y0$ like CATE?

showlm(lm(y1 ~ treated + y0))

## Repeat 1,000 times on simulated data

n = 1000
nSim = 1000
estD = data.frame(ate=rep(NA,nSim),cuped=NA,lm=NA)
te = 5
for(i in 1:nSim){
  y0 = rnorm(n,100,sd=20)
  d  = sample(c(0, 1),n,replace = TRUE)
  y1 = 10 + y0 + te*d + rnorm(n,0,20)
  theta = cov(y0,y1)/var(y0)
  y1_cuped = y1 - theta*(y0 - mean(y0))
  
  estD[i,'ate']   = coef(lm(y1 ~ d))[2]
  estD[i,'cuped'] = coef(lm(y1_cuped ~ d))[2]
  estD[i,'lm']    = coef(lm(y1 ~ d + y0))[2]
}

## Simlar means and variances...

sapply(estD, function(x) c(mean = mean(x), 
                           variance = var(x)))

## CUPED is *very* similar to CATE

estD_long = estD %>% pivot_longer(everything())

ggplot(estD_long,aes(value,fill=name,color=name)) + 
  theme_minimal() +
  geom_vline(xintercept = te,linetype = 'dashed') + 
  geom_histogram(alpha = .2,position='identity')

## Data

load('data/casino.rdata')
str(casino)

## ATE

showlm(lm(spend ~ treatment,casino))

## CATE

showlm(lm(spend ~ treatment + 
            age + income + loyalty,casino))

## Match with PSM

library(MatchIt)
ps_match = matchit(treatment ~ 
                     age + income + loyalty,casino)


## What happened?

ps_match

## Check matching...

summary(ps_match)

## Check propensity scores

plot(ps_match, type = "jitter", interactive = FALSE)

## Check densities

plot(ps_match, type = "density", 
     interactive = FALSE,
     which.xs = ~income + loyalty)

## Get CATE

casino_md = match.data(ps_match)
showlm(lm(spend ~ treatment + age + income + loyalty,
          casino_md))

## Casino ATE with IPW

response_model = glm(treatment ~ age + income + loyalty,
                     casino,family='binomial')
casino$e = predict(response_model,type='response')
casino = casino %>% 
  mutate(ipw = treatment/e + (1-treatment)/(1-e))

## Used IPW as weights in `lm`

showlm(lm(spend ~ treatment, weights = ipw,casino))

## Checking overlap assumption

ggplot(casino,aes(e,fill=factor(treatment))) + 
  theme_minimal() +
  geom_density(alpha = .5) + 
  xlab('Propensity Score') + 
  scale_fill_discrete(name = 'Treatment') +
  theme(legend.position = 'bottom')

## AIPW In R
    
outcome_model = lm(spend ~ treatment + 
                     age + income + loyalty,casino)
u1 = predict(outcome_model,casino %>% mutate(treatment = 1))
u0 = predict(outcome_model,casino %>% mutate(treatment = 0))
y = casino$spend
e = casino$e
d = casino$treatment
mean(u1 - u0 + d/e*(y - u1) - (1-d)/(1-e)*(y - u0))

## Or use `grf`

library(grf)
X  = casino[,c('age','income','loyalty')]
cf = causal_forest(X,y,d)
average_treatment_effect(cf)


## DoubleML steps (don't worry, a package will do all this)
  
n = nrow(casino)
Y = casino$spend
D = casino$treatment
X = as.matrix(casino[,c('age','income','loyalty')])

lm(Y ~ D + X)
lm(lm(Y ~ X)$residuals ~ lm(D ~ X)$residuals)

#Now we can use ML for both steps to "residual out" the treatment effect
#To do this properly, we need a train/test split
ind = sample(1:n,n/2)
Y1 = Y[ind]
D1 = D[ind]
X1 = X[ind,]

Y2 = Y[-ind]
D2 = D[-ind]
X2 = X[-ind,]

#Estimate pr(D | X) and E[Y | X]:
library(randomForest)
e_Y1 = randomForest(X1,Y1)
e_Y2 = randomForest(X2,Y2)
e_D1 = randomForest(X1,D1)
e_D2 = randomForest(X2,D2)

#Out of sample predictions
Y1_hat = predict(e_Y2,X1) #E[Y1 | X1]
Y2_hat = predict(e_Y1,X2)
D1_hat = predict(e_D2,X1)
D2_hat = predict(e_D1,X2)

#Residuals
r_Y1 = Y1 - Y1_hat
r_Y2 = Y2 - Y2_hat
r_D1 = D1 - D1_hat
r_D2 = D2 - D2_hat

theta1 = coef(lm(r_Y1 ~ r_D1))[2]
theta2 = coef(lm(r_Y2 ~ r_D2))[2]
mean(theta1,theta2)


## Using DoubleML Package

library(DoubleML)
library(mlr3)
library(mlr3learners)
mlr_learners

## Step 1: Set up data
   
dml_data = DoubleMLData$new(casino,
                               y_col = "spend",
                               d_cols = "treatment",
                               x_cols = c("age","income","loyalty"))
#or
y = casino$spend
X = as.matrix(casino %>% select(age,income,loyalty))
d = casino$treatment
dml_data = double_ml_data_from_matrix(X=X, y=y, d=d)
   
## Step 2: Set learners
   
learner = lrn("regr.ranger", 
             num.trees=500, 
             max.depth=5, 
             min.node.size=2)
ml_l = learner$clone() #clone for independent models
ml_m = learner$clone() 

## Step 3: Estimate

obj_dml_plr = DoubleMLPLR$new(dml_data, 
                             ml_l = ml_l, 
                             ml_m = ml_m)
obj_dml_plr$fit()

## Step 4: Show results

print(obj_dml_plr)

   
   