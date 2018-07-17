#7/15/18 
#Estimating Aces using Bayesian Statistics 
tennis_wins=subset(tennis,best_of==3,select=c("winner_name","w_ace","w_SvGms","best_of"))
tennis_wins$ace_rate=tennis_wins$w_ace/tennis_wins$w_SvGms
tennis_wins1=subset(tennis_wins,ace_rate>0) #ace rate based on number of first serves in 

#step 1:estimate a prior from the data
tennis_wins1 %>%
  ggplot(aes(x=ace_rate))+
  geom_density(color="red")+
  geom_rug(color="red")+
  theme_bw()+
  labs(x="ace rate",y="desnity")

#aside: filter players sample 
filter_aces=tennis_wins1 %>%
  filter(w_ace>1)

m=MASS::fitdistr(tennis_wins1$ace_rate,dbeta,
                 start=list(shape1=1,shape2=3))

alpha0=m$estimate[1]
alpha0
beta0=m$estimate[2]
beta0 

#step2: usa this beta distribution as a prior for each individual estimate 
ace_rate_update=tennis_wins1 %>%
  mutate(ace_rate_est=(w_ace+alpha0)/(w_SvGms+alpha0+beta0))

#highest ace rate estimates 
ace_rates_high=ddply(ace_rate_update, .(winner_name), plyr::summarize,  ace_rate_est=mean(ace_rate_est), ace_rate_actual=mean(ace_rate))

ggplot(ace_rates_high,aes(x=ace_rate_est,y=ace_rate_actual))+geom_point(color="orange")+
  xlab("Ace Rate Estimate")+ylab("Actual Ace Rate")


# beta-binomial distribution
# gives more consideration for players with more serves  
library(VGAM)
#negative log likelihood of data given alpha, beta parameters 
ll=function(alpha,beta){
  -sum(dbetabinom.ab(tennis_wins1$w_ace,tennis_wins1$w_SvGms,alpha,beta,log=TRUE))
}

m=mle(ll,start=list(alpha=1,beta=10),method = "L-BFGS-B")







