library(tidyverse)

# MOVIE COLLABORATIVE FILTERING -----------------------

## User-based CF: find similar users, predict the missing rating

rating = matrix(c(5,4,NA,1,4,5,1,2,1,1,3,4,3,2,4,3,3,5,1,2),5,4,byrow = TRUE)
rating

## Find the similarity

#the target user cannot be compared on item 3
#consider only items 1, 2, and 4
simMat  = rating[,-3]
simVec1 = rep(NA,4) #sim b/t user 1 and other 4
for(i in 2:5){
  simVec1[i-1] =
    sum((simMat[1,] - mean(simMat[1,]))*
          (simMat[i,]-mean(simMat[i,])))/
    (sqrt(sum((simMat[1,] - mean(simMat[1,]))^2))*
       sqrt(sum((simMat[i,] - mean(simMat[i,]))^2)))
}
simVec1

## Item-based CF Example

rating

## Item-Item correlation matrix

round(cor(rating[-1,]),2)

## Example: MovieLense dataset

library(recommenderlab)
data("MovieLense")

## Ratings from first 5 users on first 6 movies

image(MovieLense[1:5,1:6])

## Number of ratings per **user**

hist(rowCounts(MovieLense),main = "Ratings/User")

## Number of ratings per **movie**

hist(colCounts(MovieLense),main = "Ratings/Movie")

## Estimate the recommender

rec  = Recommender(MovieLense,method = "UBCF")
names(getModel(rec))

## Use the model

#top 5 movies for first user
i1top5 = predict(rec,MovieLense[1,],n=5)
as(i1top5,"list")[[1]]

#This user has not rated any of these movies
MovieLense[1,match(as(i1top5,"list")[[1]],
                   colnames(MovieLense))]


## Adstock in R

spend = c(10,12,20,12,18,25,15,10)

get_adstock = function(x,alpha = .5,L = 1) {
  n = length(x)
  adstock    = numeric(n)
  adstock[1] = spend[1]
  a = alpha^(0:L)
  
  for (t in 2:n) {
    w  = a[1:min(t,L+1)]
    adstock[t] = sum(w*x[t:max(t-L,1)])/sum(w)
  }
  
  out = data.frame(time=1:n,alpha=alpha,adstock)
  return(out)
}

## Adstock visual

ad_gg = bind_rows(
  get_adstock(spend,.1),
  get_adstock(spend,.5),
  get_adstock(spend,.9))

ggplot(ad_gg,aes(time,adstock,color=factor(alpha))) + 
  theme_minimal() +
  scale_color_discrete(name='Alpha') +
  xlab('Time') + ylab('Adstock') +
  geom_line() + 
  theme(legend.position = 'bottom')



## Hill Function in R
  
hill  = function(x, ec, slope) {
  data.frame(spend=x,ec,slope,
             hill=1/(1+(x/ec)^(-slope)))
}
spend = 1:100

hill_gg = bind_rows(
  hill(spend,25,.2),
  hill(spend,50,.6),
  hill(spend,50,1.5)) %>%
  mutate(label = paste0('EC: ',ec,' Slope: ',slope))

## Hill Function Visual

ggplot(hill_gg,aes(spend,hill,color=label)) + 
  theme_minimal() +
  scale_color_discrete(name='') +
  scale_x_continuous(name='Spend',labels = scales::dollar) + 
  scale_y_continuous(name='Saturation',labels = scales::percent) + 
  geom_hline(yintercept = .5, linetype = 'dashed') +
  geom_line() + 
  theme(legend.position = 'bottom')
