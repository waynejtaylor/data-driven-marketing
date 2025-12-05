library(tidyverse)
library(MASS)

load('data/simpleRegression.rdata')
head(myData)

## Average sales by country
myData %>% 
  group_by(usa) %>%
  summarise(avgSales = mean(sales))

## Responsiveness to price
ggplot(myData,aes(price,sales,color=factor(usa),group=factor(usa))) + 
  geom_point()+ stat_smooth(method='lm',color='black',se = FALSE) + 
  theme_bw(15) +
  scale_color_discrete(name = 'USA')

## Basic and lame amateur approach
data_mex = subset(myData,usa == 0)
out_mex  = lm(sales ~ price,data_mex)
round(coef(out_mex),2)

data_usa = subset(myData,usa == 1)
out_usa  = lm(sales ~ price,data_usa)
round(coef(out_usa),2)

## Professional approach
out = lm(sales ~ price * usa,myData)
round(coef(out),2)



#SIMPLE SEGMENTATION ---------------------------
load('data/amazon_segmentation.rdata')

#two files
head(amazon_demo)
head(amazon_transaction)

#Customer summary
summary(amazon_demo)

regionCnt = amazon_demo %>% 
  group_by(region) %>% 
  summarise(n = n())
regionCnt

#My first pie chart in ggplot (kind of ugly, but it works)
ggplot(regionCnt, aes(x='',y=n, fill=region)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()

#transaction summary
#by person
amazon_transaction %>% 
  group_by(id) %>%
  summarise(purchases = n(),
            avgAmount = mean(amount),
            sdAmount = sd(amount))

#by product category
amazon_transaction %>% 
  group_by(category) %>%
  summarise(purchases = n(),
            avgAmount = mean(amount),
            sdAmount = sd(amount))

#by date
amazon_transaction %>% 
  group_by(date) %>%
  summarise(purchases = n(),
            avgAmount = mean(amount),
            sdAmount = sd(amount))

#use a function!
mySummary = function(groupVar) {
  amazon_transaction %>% 
    group_by({{groupVar}}) %>% #notice the {{}} around the variable name, see ?dplyr_data_masking
    summarise(purchases = n(),
              avgAmount = mean(amount),
              sdAmount = sd(amount))
}

mySummary(id)
mySummary(category)
mySummary(date)

#Join demographic and transaction data
amazon = inner_join(amazon_transaction,amazon_demo,by='id')

#setting up groups:
catDist = prop.table(table(amazon$category)) #do we really need all these categories?
catDist
hist(amazon$hhSize) #do we expect the relationship to be linear?
hist(amazon$hhInc)  #what segment can we create here?

lowCat = names(catDist[catDist< .1])
lowCat

amazon = amazon %>%
  mutate(categoryReduced = if_else(category %in% lowCat,'Other',category),
         incomeCat = if_else(hhInc > median(hhInc),'High','Low'),
         hhSizeCat = case_when(
           hhSize == 1 ~ 'Single',
           hhSize == 2 ~ 'Pair',
           TRUE ~ 'Family')) %>%
  #here I define segments of interest
  mutate(segment = case_when(
    incomeCat == 'High' & hhSizeCat == 'Family' ~ 'High Income Family',
    incomeCat == 'Low' & hhSizeCat == 'Single' ~ 'Low Income Singles',
    TRUE ~ 'Other'
  ))

#granular
summary(lm(amount ~ category + hhInc + hhSize + region,amazon))

summary(lm(amount ~ categoryReduced + incomeCat + hhSizeCat + region,amazon))

#using pre-defined segments
summary(lm(amount ~ segment + region,amazon))

## The data

load('data/homedepot.rdata')
str(homedepot)

## Simple Plot

ggplot(homedepot, aes(spend,daysBetween)) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(labels = scales::dollar) +
  xlab('Avg. Spend (M)') +
  ylab('Avg. Days Between Trips (F)')

## Basic K-means in R

#recenter the variables
hd_scaled = scale(homedepot)
kfit = kmeans(hd_scaled,centers = 2)

#total variance
kfit$tot.withinss
#sum((hd_scaled - kfit$centers[kfit$cluster,])^2)

#size of each cluster
kfit$size

## Selecting the number of clusters: use a scree plot

screedf = data.frame(k = 1:10,tot.withinss = NA)
for(k in 1:nrow(screedf)){
  screedf$tot.withinss[k] = 
    kmeans(hd_scaled,centers = k)$tot.withinss
}
ggOut = ggplot(screedf,aes(k,tot.withinss)) +
  geom_line() + theme_bw(15) + geom_point() +
  scale_x_continuous(breaks = 1:10) +
  xlab("Number of Clusters") + 
  ylab("Total Within Group Sum of Squares")
ggOut

## Looks like 3 clusters

kfit = kmeans(hd_scaled,centers = 3)
homedepot$cluster = kfit$cluster

## Visualize

ggplot(homedepot, aes(spend,daysBetween,color=factor(cluster))) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(labels = scales::dollar) +
  xlab('Avg. Spend (M)') +
  ylab('Avg. Days Between Trips (F)') + 
  scale_color_discrete(name = 'Cluster') + 
  theme(legend.position = 'bottom')

## Describe the clusters and label...

homedepot %>%
  group_by(cluster) %>% 
  summarise(across(everything(),~round(mean(.),2)))


## Back to The Home Depot

library("mclust")
mcfit = Mclust(hd_scaled)

## Mixture classification

plot(mcfit, what = "classification")

## Mixture uncertainty

plot(mcfit, what = "uncertainty")

head(mcfit)

#parameters of the multivariate normal densities, and probabilities for each cluster
mcfit$parameters
mcfit$z



## *Simulating mixture densities

K = 2
mu    = c(4,10)
sigma = c(1,.5)
p     = c(.8,.2)

#observations to simulate
n = 1000
z = sample(1:K,n,TRUE,p)

X = rep(NA,n)
for(k in 1:K){
  X[z == k] = rnorm(sum(z==k),mu[k],sigma[k])  
}

## *Density plot of $X$

plot(density(X),main="2 Component Mixture Density")

## *Simulating multivariate mixture densities

K   = 2 #number of components
dim = 2 #dimension of X
parms = list()
parms[[1]] = list(mu = c(4,8),
            Sigma=matrix(c(1,.9,.9,3),ncol=dim))
parms[[2]] = list(mu = c(10,20),
            Sigma=matrix(c(.5,-.3,-.3,2),ncol=dim))
p     = c(.8,.2)

#observations to simulate
n = 1000
z = sample(1:K,n,TRUE,p)

X = matrix(NA,n,dim)
for(k in 1:K){
  X[z == k,] = mvrnorm(sum(z==k),parms[[k]]$mu,
                      parms[[k]]$Sigma)  
}

## *Density plot of $X_1$

plot(density(X[,1]),main="Marginal Density of Multivariate Mixture Density")

## *Density plot of $X_2$

plot(density(X[,2]),main="Marginal Density of Multivariate Mixture Density")

## *Joint density

# Calculate kernel density estimate
bivn.kde = kde2d(X[,1], X[,2], n = 100) 

# Contour plot overlayed on heat map image of results
image(bivn.kde,col = terrain.colors(100),
      xlab = expression(X[1]),
      ylab = expression(X[2]))       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package

## *Another view

library(plot3D)
X.kde = kde2d(X[,1], X[,2], n = 100)   # from MASS package
persp(X.kde$z, theta = 40, phi = 40, col = "dodgerblue", border = NA, shade = 0.2,
      xlab="X1",ylab="X2",
      zlab = "Density")


## In R

library(dbscan)
dbscan_out = dbscan(hd_scaled, eps = .5, minPts = 10)
homedepot$dbs1 = dbscan_out$cluster

## `eps = .5` and `minPts = 10`

ggplot(homedepot, aes(spend,daysBetween,color=factor(dbs1))) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(labels = scales::dollar) +
  xlab('Avg. Spend (M)') +
  ylab('Avg. Days Between Trips (F)') + 
  scale_color_discrete(name = 'DBSCAN Cluster') + 
  theme(legend.position = 'bottom')

## `eps = .2` and `minPts = 10`

dbscan_out = dbscan(hd_scaled, eps = .2, minPts = 10)
homedepot$dbs2 = dbscan_out$cluster
ggplot(homedepot, aes(spend,daysBetween,color=factor(dbs2))) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(labels = scales::dollar) +
  xlab('Avg. Spend (M)') +
  ylab('Avg. Days Between Trips (F)') + 
  scale_color_discrete(name = 'DBSCAN Cluster') + 
  theme(legend.position = 'bottom')




#CLUSTER ANALYSIS ---------------------------
load('data/everlane.rdata')

#check out the data!
head(everlane)

#Calculate frequency and average spend by customer
everlaneC = everlane %>%
  group_by(id,age,female) %>%
  summarise(minDate = min(transdate),
            maxDate = max(transdate),
            purchases = n(),
            avgSpend  = mean(spend)) %>%
  mutate(days = as.numeric(maxDate - minDate)) %>%
  mutate(freq = days/purchases) %>% 
  dplyr::select(age,female,purchases,avgSpend,freq) %>%
  ungroup() %>%
  dplyr::select(-id)

everlaneS = everlaneC %>% 
  mutate(across(everything(),scale))

#K-means clustering
K = 10
scree = data.frame(k = factor(1:K),totwithinss = NA) 
for (k in 1:K) scree$totwithinss[k] = kmeans(everlaneS,centers = k)$tot.withinss

#No need for fancy plots if only you see it
#ggplot(scree,aes(k,totwithinss)) + geom_line() had to Google this
ggplot(scree,aes(k,totwithinss,group=1)) + geom_line()

#Notice difference from unscaled
screeUS = data.frame(k = factor(1:K),totwithinss = NA) 
for (k in 1:K) screeUS$totwithinss[k] = kmeans(everlaneC,centers = k)$tot.withinss
ggplot(screeUS,aes(k,totwithinss,group=1)) + geom_line()

#Best to use scaled, unless all variables are on same scale already
everlaneS

#Let's compare the three clusters
set.seed(1);kmeansOut = kmeans(everlaneS,centers = 3)

#What does this mean?
everlaneC$cluster = kmeansOut$cluster
everlaneC %>%
  group_by(cluster) %>% 
  summarise(n = n(),
            across(everything(),mean))

#Cluster 1: Middle aged females, loyal with high purchases
#Cluster 2: Older females, more frequent shoppers
#Cluster 3: Middle aged males, few purchases with the firm, highest spend but lowest frequency

#Why not try a mixture
library("mclust")
#mcfitEverlane = Mclust(everlaneS) #notice error: https://stackoverflow.com/questions/29750345/r-mclust-getting-svd-error-infinite-or-missing-value
mcfitEverlane = Mclust(everlaneS[,-2]) #remove "female" (only binary variable)

plot(mcfitEverlane, what = "classification")
plot(mcfitEverlane, what = "uncertainty")

everlaneC$KMMcluster  = mcfitEverlane$classification
everlaneC$uncertainty = mcfitEverlane$uncertainty

#summary by KMM cluster and uncertainty flag (less than 25% uncertainty)
everlaneC %>%
  mutate(certainFlag = uncertainty < .25) %>%
  group_by(certainFlag,KMMcluster) %>%
  summarise(n = n(),across(everything(),mean))


#AGGLOMERATIVE CLUSTERING ------------------
data(USArrests)
head(USArrests)
USArrestsScaled = sapply(USArrests,scale)
distout   = dist(USArrestsScaled)
hclustout = hclust(distout,method="ward.D")
plot(hclustout,labels = row.names(USArrests))
rect.hclust(hclustout, k=4,border = 2:5)
cutree(hclustout,k=3)




