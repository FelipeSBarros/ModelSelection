rm(list = ls())


## Libraries
require("plyr")
require("Rcpp")
require("MuMIn")
library(data.table)

### Set global options to prevent fitting models to different datasets
options(na.action = "na.fail")


## Read data
#setwd("C:/Users/Projetos_3/Dropbox/IIS/CATTLE INTENSIFICATION/model")
#setwd("/Users/benphalan/Dropbox/FILE_BACKUP/Projects/Brazil_IIS/EllenFonte/Analysis/2016") #Ben
#setwd("D:/Ellen")
data<-read.table("dados2.txt", header=T, sep="\t")

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
top_ranked<-list()
null_model_AICc<-list()
null_model_position<-list()
coef<-list()
r2<-list()
ni <- 10000## number of iterations
for (i in 1:ni){

  ## Sample 1 data per study
  data4<-ddply(data,.(study),randomRows,1)
  
  ### First fit full model ### EDIT THESE FULL MODELS                                     ###### EDIT FULL MODELS
  m1 <- glm(RR.total.yield ~ biome+Y+control.c+legumes.c+crops.c+nutri.c+supple.c+tree.c+legumes.t+crops.t+tree.t+nutri.t+suppe.t+rotation+gender+type+stock.rate.control, data=data4, gaussian("identity"))
  lm1 <- lm(RR.total.yield ~ biome+Y+control.c+legumes.c+crops.c+nutri.c+supple.c+tree.c+legumes.t+crops.t+tree.t+nutri.t+suppe.t+rotation+gender+type+stock.rate.control, data=data4)
  
  #m1 <- glm(treat.total.yield ~ biome+Y+legumes+crops+nutri+supple+tree+rotation+gender+goal, data=data4, gaussian("identity"))
  #lm1 <- lm(treat.total.yield ~ biome+Y+legumes+crops+nutri+supple+tree+rotation+gender+goal, data=data4)
  
  ### Then get and fit all possible models with no more than three variables
  models <- dredge(m1, rank = "AICc", trace = TRUE)
  models1 <- get.models(models, subset=TRUE)
  ranks <- m.AICc(models1, as.numeric(names(models1[1])), length(data4$RR))

  ### Repeat for linear models
  l.models <- dredge(lm1, rank = "AICc", m.lim=c(0,3), trace = TRUE)
  l.models1 <- get.models(l.models, subset=TRUE)
  
  ### Get the top-ranked model from this iteration 
  top_ranked[[i]]<-ranks[1,]
  #null_model_AICc[[i]]<-ranks[ranks$name=="1",]
  #null_model_position[[i]]<-which(ranks$name=="1",)
  coef[[i]]<-list(summary(models1[[1]]))
  r2[[i]]<-summary(l.models1[[1]])
}

## Transforming lists in data.frames
df_top_ranked<-data.frame(matrix(unlist(top_ranked), nrow=ni, byrow=T))
#df_null_model_AICc<-data.frame(matrix(unlist(null_model_AICc), nrow=ni, byrow=T))          # not needed I think
#df_null_model_position<-data.frame(matrix(unlist(null_model_position), nrow=ni, byrow=T))  # not needed I think

## Saving data.frames
write.table(df_top_ranked, "Top_ranked.txt",quote=F)
#write.table(df_null_model_AICc, "Null_model_AICc.txt",quote=F)
#write.table(df_null_model_position, "Null_model_position.txt",quote=F)
sink("Coefficients.txt") 
lapply(coef, print) 
sink() 
sink("R2.txt") 
lapply(r2, print) 
sink() 

## Top-model frequence
plausible_mod_frequency<-as.data.frame(ftable(df_top_ranked[,7]))
plausible_mod_table_AICc<-aggregate(df_top_ranked, list(df_top_ranked[,7]), mean)
#plausible_Null_table_AICc<-aggregate(df_null_model_AICc, list(df_null_model_AICc[,7]), mean)

## Saving data.frames
write.table(plausible_mod_frequency, "Plausible_mod_frequency.txt",quote=F, sep="\t")
write.table(plausible_mod_table_AICc, "Plausible_mod_table_AICc_mean.txt",quote=F, sep="\t")
#write.table(plausible_Null_table_AICc, "Plausible_Null_table_AICc_mean.txt",quote=F, sep="\t")

## R2 values for top-ranked model
r2_estimate<-list()
for(i in 1:ni){
  r2<-r2
  df<-data.frame(r2[[i]][9])
  r2_estimate[[i]]<-df[,1]
}

df_r2_estimate<-data.frame(matrix(unlist(r2_estimate), nrow=ni, byrow=T))
r2_table<-cbind(df_top_ranked[,7],r2_estimate)

### This next part is a little complex to deal with the (rare?) case
### when there is more than one top models (a tie)

# which model(s) most often selected
mostoften <- as.numeric(as.character(plausible_mod_frequency[which(plausible_mod_frequency[,2]==max(plausible_mod_frequency[,2])),1]))
r2_plausible_model_temp <- list()

# Extract from r2_table for each top model
for (j in 1: length(mostoften)) {
r2_plausible_model_temp[[j]] <- subset(r2_table, r2_table[,1]==mostoften[j])
}

# rbind all of the r2 values from top models together
r2_plausible_model <- r2_plausible_model_temp[[1]]
ifelse(length(mostoften)>1, k<-2, k<-length(mostoften)+1)
while (k<=length(mostoften)) {
r2_plausible_model <- rbind(r2_plausible_model, r2_plausible_model_temp[[k]])
k <- k+1}

# Get mean of the r2 values and write to file
r2_plausible_model_mean<-mean(as.numeric(r2_plausible_model[,2]))
write.table(r2_plausible_model_mean, "m.r2_plausible_model.txt", quote=F, sep="\t")

### Repeat for coefficient values
top_ranked_model.x_temp <- list()

# Coefficient values for top-ranked model(s)
for (j in 1: length(mostoften)) {
  top_ranked_model.x_temp[[j]] <- as.data.frame(which(df_top_ranked[,7]==mostoften[j]))
}

# rbind all of the coefficient values from top models together
top_ranked_model.x <- top_ranked_model.x_temp[[1]]
ifelse(length(mostoften)>1, k<-2, k<-length(mostoften)+1)
while (k<=length(mostoften)) {
  top_ranked_model.x <- rbind(top_ranked_model.x, top_ranked_model.x_temp[[k]])
  k <- k+1}

# Write table to file
write.table(top_ranked_model.x,"top_ranked_model.1.5.txt", sep="\t")

## Extract coefficients for top model from each iteration when it was top (I think!)
nm <- nrow(top_ranked_model.x)
dfn <- list()

for (i in 1:nm) {
  nmn <- top_ranked_model.x[i,1]
  dfn[[i]] <- data.frame(coef[[nmn]][[1]][12])
}

# Then iterate for all nm sets of stored coefficients
df.join <- dfn[[1]]
m <- 2
while (m<=nm) {
  df.join <- rbind(df.join, dfn[[m]])
  m <- m+1}

write.table(df.join, "m.Coef_plausible_model.bind.txt", quote=F, sep="\t")

summary<-models
write.table(summary, "summary mod total. txt", quote=F, sep="\t")
summary