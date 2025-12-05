library(tidyverse)
library(broom)
library(pwr)
library(binom)
library(kableExtra)

## Doordash Data

load('data/doordash.rdata')
str(doordash)

ate = doordash %>%
  group_by(treatment) %>%
  summarise(seconds_bar = mean(seconds))
ate

## Another way...

with(doordash,
     mean(seconds[treatment == 1]) - 
     mean(seconds[treatment == 0]))

## Use a t-test

t.test(seconds ~ treatment,doordash)

## Or use regression...

r1 = lm(seconds ~ treatment,doordash)
tidy(r1) %>% select(1,2,5)
confint(r1)


## DoorDash CATE

r2=lm(seconds ~ treatment*mobile,doordash)
summary(r2)
tidy(r2) %>% select(1,2,5)

## Confidence intervals

confint(r2)

## DoorDash CATE

r3=lm(seconds ~ treatment*mobile + age + mthsActive,
      doordash)
coef(r3)

## More precise CIs

confint(r3)

## Simple regression tree in DoorDash

library(grf)
X = doordash %>% select(mobile,age,mthsActive)
Y = doordash$seconds

rf   = regression_forest(X, Y, num.trees = 1)
tree = get_tree(rf, 1)
plot(tree)

## Random forests

rf50 = regression_forest(X, Y, num.trees = 50)
plot(get_tree(rf50, 1))
plot(get_tree(rf50, 50))

## Random forest predictions

plot(X$age, rf50$predictions, col=factor(X$mobile))

## Causal forest for DoorDash

W     = doordash$treatment
W.hat = .5 #randomly assigned, equally sized groups
cf = causal_forest(X, Y, W, W.hat=0.5, seed=1) 

## Causal forest CATEs

doordash$CATE = cf$predictions
str(doordash)

## Heterogeneity in predicted CATES

hist(cf$predictions)

## Causal forest versus random forest

plot(rf$predictions, cf$predictions)

## Causal forest CATES by desktop/mobile

boxplot(CATE~mobile,doordash)
#or stripchart(CATE~mobile,doordash,vertical=TRUE)

## Causal forest CATEs versus age

ggplot(doordash,aes(mthsActive,CATE,color=factor(mobile))) + 
  theme_minimal(15) +
  theme(legend.position = 'bottom') + 
  xlab('Months with DoorDash') + 
  scale_color_discrete(name='Desktop (0) or Mobile (1)') +
  geom_point()

## Optimal treatement given $X$?

library(policytree)
#opportunity cost of not treating = -tau
rewards = cbind(control=-get_scores(cf), 
                treatment=get_scores(cf))
tree = policy_tree(X, rewards, min.node.size = 1)
plot(tree, leaf.labels=c("Control", "Treatment"))

## Which predictors are the most important? 

best_linear_projection(cf, X)

## Causal forest average treatment effect

average_treatment_effect(cf)


## Power Calculations in R
pwr.t.test(d = 0.2,      #difference in means
    sig.level = 0.05,    #Type I error (alpha)
    power = 0.8,         #Power (1 - pr(Type II))
    type = "two.sample", #one/two/paired
    alternative = "two.sided")

## Smaller Effect $\rightarrow$ Larger N

pwr.t.test(d = 0.01,     #difference in means
    sig.level = 0.05,    #Type I error (alpha)
    power = 0.8,         #Power (1 - pr(Type II))
    type = "two.sample", #one/two/paired
    alternative = "two.sided")

## Data
load('data/amazon.rdata')
head(amazon)

## ATE for proportions

ate_p = amazon %>%
  group_by(treatment) %>%
  summarise(conversion_bar = mean(conversion))
ate_p

## Another way...

with(amazon,
     mean(conversion[treatment == 1]) - 
     mean(conversion[treatment == 0]))

## Confidence Intervals for Proportions

n = nrow(amazon)
binom.confint(sum(subset(amazon,treatment == 1)$conversion),
              n = n,method='prop.test')
binom.confint(sum(subset(amazon,treatment == 0)$conversion),
              n = n,method='prop.test')

## Use a prop-test

propData = amazon %>% group_by(treatment) %>%
  summarise(conversions = sum(conversion),
            n = n())
prop.test(propData$conversions,propData$n)

## Controlling for confounders

cate_p = glm(conversion ~ treatment*prime + 
  monthlySpend + mobile,amazon,family='binomial')
coef(cate_p)
tidy(cate_p) %>% select(1,2,5)

## Confidence intervals

confint(cate_p)

## Predict probabilities

cate_p_df = expand.grid(
  treatment = 0:1,
  mobile = 0:1,
  monthlySpend = round(mean(amazon$monthlySpend),2),
  prime = 0:1)
lo_hat = predict(cate_p, newdata = cate_p_df, se.fit = TRUE)
lower_lo = lo_hat$fit - 1.96 * lo_hat$se.fit
upper_lo = lo_hat$fit + 1.96 * lo_hat$se.fit

## Compare results

cbind(cate_p_df,lb = round(plogis(lower_lo),3),ub = round(plogis(upper_lo),3))

## *Bootstrap SEs on the probabilities

nBootstraps = 100
n = nrow(amazon)
bootstrapOut = data.frame(control = rep(NA,nBootstraps),
                          treatment = rep(NA,nBootstraps))

for(b in 1:nBootstraps){
  df_b = amazon[sample(1:n,n,replace=TRUE),]
  temp = glm(conversion ~ treatment,family='binomial',data = df_b)
  bootstrapOut[b,] = predict(temp,data.frame(treatment = 0:1),type='response')
}

## Now take quantiles

head(bootstrapOut)
apply(bootstrapOut,2,quantile)

## Or get quantiles this way...

bootstrapOut %>%
  reframe(across(c(control,treatment),
                   ~quantile(.x,c(.025,.975))))

## *Another option for bootstrapping...

singleBootstrap = function() {
  df_b = amazon[sample(1:n,n,replace=TRUE),]
  temp = glm(conversion ~ treatment,family='binomial',data = df_b)
  predict(temp,data.frame(treatment = 0:1),type='response')
}

#control | treated
bootOut = t(replicate(10,singleBootstrap()))
apply(bootOut,2,quantile)

## Causal forest 

W = amazon$treatment
Y = amazon$conversion
X = amazon %>% select(-c(treatment,conversion))
cf_amzn = causal_forest(X, Y, W, W.hat = 0.5, seed=1) 
amazon$CATE = cf_amzn$predictions

## Causal forest ATE

average_treatment_effect(cf_amzn)

## Heterogeneity in predicted CATES

hist(cf_amzn$predictions)

## Which predictors are the most important? 

best_linear_projection(cf_amzn, X)

## Causal forest CATES versus mobile users

boxplot(CATE~prime,amazon)

## Causal forest CATEs versus Prime
ggplot(amazon,aes(monthlySpend,CATE,color=factor(prime))) + 
  theme_minimal(15) +
  theme(legend.position = 'bottom') + 
  geom_point() +
  scale_x_continuous(trans = 'log')

## Which treatement given $X$?
rewards = cbind(control=-get_scores(cf_amzn), 
                treatment=get_scores(cf_amzn))
tree = policy_tree(X, rewards, min.node.size = 1)
plot(tree, leaf.labels=c("Control", "Treatment"))