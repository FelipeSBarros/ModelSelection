quit()#leaving R, when using rgedit like me :)
#SSH conection with buriti
ssh felipe@buriti.lncc.br

cd Ellen/
# creating tmux session
tmux
# IF a tmux session were previously crreated, get attached to it:
tmux attach

# running an empty R session, if necessary
R

rm(list = ls())

## Libraries
require("plyr")
require("Rcpp")
require("MuMIn")
library("data.table")
library("snowfall")

### Set global options to prevent fitting models to different datasets
options(na.action = "na.fail")

## Read data
#setwd("C:/Users/Projetos_3/Dropbox/IIS/CATTLE INTENSIFICATION/model")
#setwd("/Users/benphalan/Dropbox/FILE_BACKUP/Projects/Brazil_IIS/EllenFonte/Analysis/2016") #Ben
#setwd("D:/Ellen")
data<-read.table("dados2.txt", header=T, sep="\t")
head(data)

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

# stating paralel
sfInit(parallel=T,cpus=10)

#exporta variáveis e funções e pacotes
sfExportAll()
#sfLibrary(rJava) #removido aspas.
sfLibrary(plyr)
sfLibrary(Rcpp)
sfLibrary(MuMIn)
sfLibrary(data.table)
sfLibrary(snowfall)
sfSource("./model selection_paralel.R")

# executing e.fct in paralel mode
tInicial <- Sys.time()
#sfLapply(x=1:2, E.fct(data=data) )
sfClusterApplyLB(x=1:10000, E.fct, data=data)
Sys.time() - tInicial
sfStop()

#######################
# vendo quantidade de arquivos na pasta:
#for especie in *; do echo "$especie" $(ls -1 "$especie" | wc -l); done
#for especie in *; do echo "$especie" $(ls -1 "$especie" | wc -l); done | grep -v 121
#########################

# compactando os dados
tar cjf Ellen_results.tar.bz2 ./Ellen/

# DESCOMPACTANDO
tar -jxvf Ellen.tar.bz2

# fazendo download 
# Fora do buriti executar
sftp felipe@buriti.lncc.br
get Ellen_results.tar.bz2

#ou
get -r nome_da_pasta
