#rm(list=ls())
## Libraries
require("plyr")
require("Rcpp")
require("MuMIn")
library("data.table")
library("snowfall")

### Set global options to prevent fitting models to different datasets
options(na.action = "na.fail")

## Model selection - AICc
m.AICc <- function(modelos,data,n){
  LL <- sapply(modelos,logLik)
  k <- sapply(lapply(modelos,logLik),attr,"df")
  AIC <- -2*LL+2*k
  AICc <- AIC+((2*k*(k+1))/(n-k-1))
  d.AICc <- AICc-min(AICc)
  w.AICc <- (exp(-0.5*d.AICc))/sum(exp(-0.5*d.AICc))
  data<-data
  data.frame(n.par=k,log.lik=LL,AICc=AICc,AIC=AIC,delta.AICc=d.AICc,w.AICc=w.AICc,name=data,header=T,row.names=names(LL))[order(d.AICc),]     
}

## Sample 1 comparison per study
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

## Run models via bootstrap
#top_ranked<-list()
#null_model_AICc<-list()
#null_model_position<-list()
#coef<-list()
#r2<-list()
#ni <- 10000## number of iterations
E.fct <- function(x, data=data){
  #for (i in 1:ni){
  
  ## Sample 1 data per study
  data4<-ddply(data,.(study),randomRows,1)
  
  ### First fit full model ### EDIT THESE FULL MODELS                                     ###### EDIT FULL MODELS
  m1 <- glm(RR.total.yield ~ biome+Y+control.c+legumes.c+crops.c+nutri.c+supple.c+tree.c+legumes.t+crops.t+tree.t+nutri.t+suppe.t+rotation+gender+type+stock.rate.control, data=data4, gaussian("identity"))
  lm1 <- lm(RR.total.yield ~ biome+Y+control.c+legumes.c+crops.c+nutri.c+supple.c+tree.c+legumes.t+crops.t+tree.t+nutri.t+suppe.t+rotation+gender+type+stock.rate.control, data=data4)
  
  #m1 <- glm(treat.total.yield ~ biome+Y+legumes+crops+nutri+supple+tree+rotation+gender+goal, data=data4, gaussian("identity"))
  #lm1 <- lm(treat.total.yield ~ biome+Y+legumes+crops+nutri+supple+tree+rotation+gender+goal, data=data4)
  
  ### Then get and fit all possible models with no more than three variables
  cat("\n Running GLM an")
  models <- dredge(m1, rank = "AICc", trace = TRUE)
  cat("\n Dredge done \n")
  models1 <- get.models(models, subset=TRUE)
  cat("\n Get models done \n")
  ranks <- m.AICc(models1, as.numeric(names(models1[1])), length(data4$RR))
  
  ### Repeat for linear models
  cat("\n Running LM \n")
  l.models <- dredge(lm1, rank = "AICc", m.lim=c(0,3), trace = TRUE)
  cat("\n Dredge done \n")
  l.models1 <- get.models(l.models, subset=TRUE)
  cat("\n Get models done \n")
  
  ### Get the top-ranked model from this iteration 
  top_ranked<-ranks[1,]
  write.table(top_ranked, paste0("./TopRanked_", x,".txt"))
  #null_model_AICc[[i]]<-ranks[ranks$name=="1",]
  #null_model_position[[i]]<-which(ranks$name=="1",)
  coef<-list(summary(models1[[1]]))
  sink(paste0("Coefficients_", x,".txt"))
  lapply(coef, print) 
  sink() 
  
  r2<-summary(l.models1[[1]])
  sink(paste0("R2_", x, ".txt"))
  lapply(r2, print) 
  sink() 
  
  cat(paste("\n\n Done", x, '\n\n'))
}