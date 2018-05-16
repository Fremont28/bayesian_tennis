#5/11/18 bayesian statistics 2016-early 2018 matches

#import the tennis match data
prague=read.csv("tennis_matches.csv")
#subset data 
prague1=subset(prague,select=c("tourney_name","surface","winner_ht",
                               "winner_rank","winner_seed","winner_age",
                               "loser_ht",
                               "loser_rank","loser_seed","loser_age"))
#did the higher rank win?
prague1$win<-ifelse(prague1$winner_rank<prague1$loser_rank,1,0)
prague1$win=as.numeric(prague1$win)
count(prague1,win) #higher rank wins 0.6632

#interval estimate of a population mean with unknown variance
quantile1=list(p=0.5,x=0.465) #we believe that mean of the prior is 0.6632
quantile2=list(p=0.99,x=0.55)
quantile3=list(p=0.01,x=0.40)

#use the findBeta to bsuca the most appropriate prior
findBeta <- function(quantile1,quantile2,quantile3)
{
  # find the quantiles specified by quantile1 and quantile2 and quantile3
  quantile1_p <- quantile1[[1]]; quantile1_q <- quantile1[[2]]
  quantile2_p <- quantile2[[1]]; quantile2_q <- quantile2[[2]]
  quantile3_p <- quantile3[[1]]; quantile3_q <- quantile3[[2]]
  
  # find the beta prior using quantile1 and quantile2
  priorA <- beta.select(quantile1,quantile2)
  priorA_a <- priorA[1]; priorA_b <- priorA[2]
  
  # find the beta prior using quantile1 and quantile3
  priorB <- beta.select(quantile1,quantile3)
  priorB_a <- priorB[1]; priorB_b <- priorB[2]
  
  # find the best possible beta prior
  diff_a <- abs(priorA_a - priorB_a); diff_b <- abs(priorB_b - priorB_b)
  step_a <- diff_a / 100; step_b <- diff_b / 100
  if (priorA_a < priorB_a) { start_a <- priorA_a; end_a <- priorB_a }
  else                     { start_a <- priorB_a; end_a <- priorA_a }
  if (priorA_b < priorB_b) { start_b <- priorA_b; end_b <- priorB_b }
  else                     { start_b <- priorB_b; end_b <- priorA_b }
  steps_a <- seq(from=start_a, to=end_a, length.out=1000)
  steps_b <- seq(from=start_b, to=end_b, length.out=1000)
  max_error <- 10000000000000000000
  best_a <- 0; best_b <- 0
  for (a in steps_a)
  {
    for (b in steps_b)
    {
      # priorC is beta(a,b)
      # find the quantile1_q, quantile2_q, quantile3_q quantiles of priorC:
      priorC_q1 <- qbeta(c(quantile1_p), a, b)
      priorC_q2 <- qbeta(c(quantile2_p), a, b)
      priorC_q3 <- qbeta(c(quantile3_p), a, b)
      priorC_error <- abs(priorC_q1-quantile1_q) +
        abs(priorC_q2-quantile2_q) +
        abs(priorC_q3-quantile3_q)
      if (priorC_error < max_error)
      {
        max_error <- priorC_error; best_a <- a; best_b <- b
      }
    }
  }
  print(paste("The best beta prior has a=",best_a,"b=",best_b))
}

library("LearnBayes")
findBeta(quantile1,quantile2,quantile3)
curve(dbeta(x,112.585,124.63599))


#2. calculating "LH" function for a proportion
calcLH_prop=function(successes,total){
  curve(dbinom(successes,total,x)) #plot the likelihood
}

calcLH_prop(1203,2588)

#calc the posterior distn for a proportion
calcPosteriorForProportion <- function(successes, total, a, b)
{
  # Adapted from triplot() in the LearnBayes package
  # Plot the prior, likelihood and posterior:
  likelihood_a = successes + 1; likelihood_b = total - successes + 1
  posterior_a = a + successes;  posterior_b = b + total - successes
  theta = seq(0.005, 0.995, length = 500)
  prior = dbeta(theta, a, b)
  likelihood = dbeta(theta, likelihood_a, likelihood_b)
  posterior  = dbeta(theta, posterior_a, posterior_b)
  m = max(c(prior, likelihood, posterior))
  plot(theta, posterior, type = "l", ylab = "Density", lty = 2, lwd = 3,
       main = paste("beta(", a, ",", b, ") prior, B(", total, ",", successes, ") data,",
                    "beta(", posterior_a, ",", posterior_b, ") posterior"), ylim = c(0, m), col = "red")
  lines(theta, likelihood, lty = 1, lwd = 3, col = "blue")
  lines(theta, prior, lty = 3, lwd = 3, col = "green")
  legend(x=0.8,y=m, c("Prior", "Likelihood", "Posterior"), lty = c(3, 1, 2),
         lwd = c(3, 3, 3), col = c("green", "blue", "red"))
  # Print out summary statistics for the prior, likelihood and posterior:
  calcBetaMode <- function(aa, bb) { BetaMode <- (aa - 1)/(aa + bb - 2); return(BetaMode); }
  calcBetaMean <- function(aa, bb) { BetaMean <- (aa)/(aa + bb); return(BetaMean); }
  calcBetaSd   <- function(aa, bb) { BetaSd <- sqrt((aa * bb)/(((aa + bb)^2) * (aa + bb + 1))); return(BetaSd); }
  prior_mode      <- calcBetaMode(a, b)
  likelihood_mode <- calcBetaMode(likelihood_a, likelihood_b)
  posterior_mode  <- calcBetaMode(posterior_a, posterior_b)
  prior_mean      <- calcBetaMean(a, b)
  likelihood_mean <- calcBetaMean(likelihood_a, likelihood_b)
  posterior_mean  <- calcBetaMean(posterior_a, posterior_b)
  prior_sd        <- calcBetaSd(a, b)
  likelihood_sd   <- calcBetaSd(likelihood_a, likelihood_b)
  posterior_sd    <- calcBetaSd(posterior_a, posterior_b)
  print(paste("mode for prior=",prior_mode,", for likelihood=",likelihood_mode,", for posterior=",posterior_mode))
  print(paste("mean for prior=",prior_mean,", for likelihood=",likelihood_mean,", for posterior=",posterior_mean))
  print(paste("sd for prior=",prior_sd,", for likelihood=",likelihood_sd,", for posterior=",posterior_sd))
  
}

calcPosteriorForProportion(30,54,112.639,124.68885)

#sample of 2016 winners
matches_16=prague[grep("2016",prague$tourney_id),]
matches_17=prague[grep("2017",prague$tourney_id),]
matches_16_17=rbind(matches_16,matches_17)
matches_16_17$win=ifelse(matches_16_17$winner_rank<matches_16_17$loser_rank,1,0)
count(matches_16_17,win) #higher rank wins 0.663
#based on height?
matches_16_17$age_plus=ifelse(matches_16_17$winner_age<matches_16_17$loser_age,1,0)
count(matches_16_17,age_plus) #46.4% 
summary(matches_16_17)
#based on clay and age
clay=subset(matches_16_17,surface=="Clay") #2610 matches? 
clay$clay_plus<-ifelse(clay$winner_age< clay$loser_age,1,0)
count(clay,clay_plus) #46.5% (2588)

matches_18=prague[grep("2018",prague$tourney_id),]
matches_18$win=ifelse(matches_18$winner_rank<matches_18$loser_rank,1,0)
count(matches_18,win) #higher rank wins 0.663

#based on height
matches_18$age_plus=ifelse(matches_18$winner_age<matches_18$loser_age,1,0)
count(matches_18,age_plus) #47.0% 
describe(matches_18$loser_age)

clay1=subset(matches_18,surface=="Clay") #54 matches
clay1$clay_plus<-ifelse(clay1$winner_age<clay1$loser_age,1,0)
count(clay1,clay_plus) #55.6% (1=30,0-24,54)

