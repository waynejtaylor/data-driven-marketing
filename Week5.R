library(tidyverse)

## Ground truth data setup

set.seed(3)
n = 1000
#mobile and male used later for contextual bandit
mobile   = sample(0:1,n,TRUE)
male     = sample(0:1,n,TRUE)
true_ctr_A = 0.10 - .04*mobile - .02*male
true_ctr_B = 0.04 + .04*mobile + .06*male
clicks_a = rbinom(n, 1, true_ctr_A)
clicks_b = rbinom(n, 1, true_ctr_B)

## Plain vanilla A/B test

ig_ab = data.frame(mab='A/B',trial=1:n,group=NA,
                   click=NA,totalClicks=0)
for(t in 1:n){
  arm_t   = sample(c('A','B'), 1)
  click_t = ifelse(arm_t == 'A',clicks_a[t],clicks_b[t])
  ig_ab$group[t] = arm_t
  ig_ab$click[t] = click_t
  if(t == 1) {
    ig_ab$totalClicks[t] = click_t 
  } else {
    ig_ab$totalClicks[t] = ig_ab$totalClicks[t - 1] + 
      click_t 
  }
}

## A/B results
ig_ab %>%
  group_by(group) %>%
  summarise(click_bar = mean(click))

## A/B cumulative clicks
ggplot(ig_ab,aes(trial,totalClicks)) + 
  theme_minimal() +
  geom_line()

## Epsilon-Greedy code

ig_eg   = data.frame(mab='EG',trial=1:n,group=NA,click=NA,totalClicks=0)
epsilon = .1 #explore with this probability
for (t in 1:n) {
  
  arm_t = sample(c('A','B'), 1)
  
  #Exploit with pr(1-epsilon)
  if(runif(1) > epsilon) {
    ctr_a = mean(subset(ig_eg,group=='A' & trial < t)$click)
    ctr_b = mean(subset(ig_eg,group=='B' & trial < t)$click)
    if(!is.na(ctr_a) & !is.na(ctr_b)){ #only exploit with enough data
      arm_t = ifelse(ctr_b > ctr_a, 'B','A') #defaults to worse true option  
    }
  }
  
  click_t = ifelse(arm_t == 'A',clicks_a[t],clicks_b[t])

  ig_eg$group[t] = arm_t
  ig_eg$click[t] = click_t
  if(t == 1) {
    ig_eg$totalClicks[t] = click_t 
  } else {
    ig_eg$totalClicks[t] = ig_eg$totalClicks[t - 1] + click_t 
  }
}

## Epsilon-Greedy results

ig_eg %>%
  group_by(group) %>%
  summarise(click_bar = mean(click))

## Epsilon-Greedy cumulative clicks

ggplot(ig_eg,aes(trial,totalClicks)) + 
  theme_minimal() +
  geom_line()

## Epsilon-Greedy vs. A/B

ig = bind_rows(ig_ab,ig_eg)
ggplot(ig,aes(trial,totalClicks,color=mab)) + 
  theme_minimal() + 
  geom_line()


## Updating posterior beliefs

p = seq(0,1,.01)
beta_dist_p = data.frame(p = p, d = dbeta(p, 1, 1), type = 'n = 0')
beta_dist_1 = data.frame(p = p, d = dbeta(p, 2, 18), type = 'n = 20')
beta_dist_2 = data.frame(p = p, d = dbeta(p, 30, 270), type = 'n = 300')
beta_dist_all   = bind_rows(beta_dist_p, 
                            beta_dist_1,
                            beta_dist_2)

ggplot(beta_dist_all, aes(x = p, y = d, color = type)) +
  geom_line() +
  geom_vline(xintercept = .1,linetype='dashed') +
  labs(x="Estimated Probability",y = "Density",color = "") +
  theme_minimal()


## Thompson Sampling code

ig_ts   = data.frame(mab='TS',trial=1:n,group=NA,click=NA,totalClicks=0)
for (t in 1:n) {
  
  clicks_a_t  = subset(ig_ts,group == 'A' & trial < t)$click
  clicks_b_t  = subset(ig_ts,group == 'B' & trial < t)$click
  sample_A    = rbeta(1,sum(clicks_a_t == 1)+1,sum(clicks_a_t == 0)+1)
  sample_B    = rbeta(1,sum(clicks_b_t == 1)+1,sum(clicks_b_t == 0)+1)
  arm_t       = ifelse(sample_B  >  sample_A,'B','A')
  
  click_t = ifelse(arm_t == 'A',clicks_a[t],clicks_b[t])

  ig_ts$group[t] = arm_t
  ig_ts$click[t] = click_t
  
  if(t == 1) {
    ig_ts$totalClicks[t] = click_t 
  } else {
    ig_ts$totalClicks[t] = ig_ts$totalClicks[t - 1] + click_t 
  }
}

## Thompson Sampling results

ig_ts %>%
  group_by(group) %>%
  summarise(click_bar = mean(click))

## Thompson Sampling cumulative clicks

ggplot(ig_ts,aes(trial,totalClicks)) + 
  theme_minimal() +
  geom_line()

## Comparisons so far...

ig = bind_rows(ig_ab,ig_eg,ig_ts)
ggplot(ig,aes(trial,totalClicks,color=mab)) + 
  theme_minimal() + 
  geom_line()

## Contextual Bandit code

ig_cb   = data.frame(mab='CB',trial=1:n,group=NA,click=NA,totalClicks=0)
for (t in 1:n) {
  
    #context 
    ind_a = ig_cb$group == 'A' & ig_cb$trial < t
    ind_b = ig_cb$group == 'B' & ig_cb$trial < t
    
    clicks_a_t = ig_cb$click[ind_a]
    clicks_b_t = ig_cb$click[ind_b]
    
    context_a = cbind(mobile[ind_a],male[ind_a])
    context_b = cbind(mobile[ind_b],male[ind_b])
    
    #No error catching
    #beta_a = coef(lm(clicks_a_t ~ context_a))
    #beta_b = coef(lm(clicks_b_t ~ context_b))
    
    #https://adv-r.hadley.nz/conditions.html?q=catch#handling-conditions
    beta_a = beta_b = NULL
    tryCatch({
      #E[click|context]
      beta_a = coef(lm(clicks_a_t ~ context_a))
      beta_b = coef(lm(clicks_b_t ~ context_b))
    }, error = function(e) {
    })
    
    arm_t   = sample(c('A','B'), 1)
    
    anyError = is.null(beta_a) | is.null(beta_b) | any(is.na(beta_a)) | any(is.na(beta_b))
    if(!anyError) {
      
      context_t = c(1,mobile[t],male[t]) 
  
      phat_a = context_t%*%beta_a
      phat_b = context_t%*%beta_b
    
      arm_t   = ifelse(phat_b  >  phat_a,'B','A')  
    }
      
  click_t = ifelse(arm_t == 'A',clicks_a[t],clicks_b[t])

  ig_cb$group[t] = arm_t
  ig_cb$click[t] = click_t
  if(t == 1) {
    ig_cb$totalClicks[t] = click_t 
  } else {
    ig_cb$totalClicks[t] = ig_cb$totalClicks[t - 1] + click_t 
  }
  
}

## Contextual Bandit results

ig_cb %>%
  group_by(group) %>%
  summarise(click_bar = mean(click))

## Contextual Bandit cumulative clicks

ggplot(ig_cb,aes(trial,totalClicks)) +
  theme_minimal() +
  geom_line()

## Compare all

ig = bind_rows(ig_ab,ig_eg,ig_ts,ig_cb)
ggplot(ig,aes(trial,totalClicks,color=mab)) + 
  theme_minimal() + 
  geom_line()