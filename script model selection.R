
rm(list = ls())

## Libraries
library("plyr")
library("Rcpp")
library("MuMIn")
        
## Read data
setwd("C:/Users/Projetos_3/Dropbox/IIS/CATTLE INTENSIFICATION/model")
data<-read.table("dados.txt", header=T, sep="\t")

##data<-data[1:155,1:15]
data<-data[-c(70,71,72,73,74,75),]## tirei  semi-confinamento



data<-subset(data, data$focous=="l" )## legumes
data<-subset(data, data$focous=="s" )## system
data<-subset(data, data$focous=="n" )## nutrients
data<-subset(data, data$focous=="su" )## su


## Model selection

##hist(data$RR, breaks=10)
#shapiro.test(data2$RR)
#qqnorm(data2$RR)
#qqline(data2$RR)


##modelg<-glm(RR~system+leg+plant+graz, data=data2, gaussian(link="identity"))
##summary(modelg)
##plot(modelg)
##residuals(modelg)
##hist(residuals(modelg))
##plot(residuals(modelg))

## test class
class(data$RR)
class(data$system)
class(data$leg)
class(data$plant)
class(data$graz)

## Outlyers
#boxplot<-boxplot(data2$RR)
#outlyers<-boxplot$out
#sort(outlyers)
#sort(data2$RR)
#data3<-subset(data2, data2$RR>-0.8835009 & data2$RR<0.8059427)

## Ploting
par(mfrow=c(2,2))
plot(RR~plant, data=data)
plot(RR~graz, data=data)
plot(RR~leg, data=data)
plot(RR~system, data=data)
plot(RR~focous, data=data)

## Model selection - AICc
m.AICc <- function(modelos,data,n){
  LL <- sapply(modelos,logLik)
  k <- sapply(lapply(modelos,logLik),attr,"df")
  AIC <- -2*LL+2*k
  AICc <- AIC+((2*k*(k+1))/(n-k-1))
  d.AICc <- AICc-min(AICc)
  w.AICc <- (exp(-0.5*d.AICc))/sum(exp(-0.5*d.AICc))
  data<-data
  data.frame(n.par=k,log.lik=LL,AICc=AICc,AIC=AIC,delta.AICc=d.AICc,w.AICc=w.AICc,name=data,row.names=names(LL))[order(d.AICc),]     
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
for (i in 1:10000){
  
  
  
  ## Sample 1 data per study
  data4<-ddply(data,.(study),randomRows,1)
  m1<-glm(RR~1, data=data4, gaussian("identity"))
  m2<-glm(RR~system, data=data4, gaussian("identity"))
  m3<-glm(RR~leg, data=data4, gaussian("identity"))
  m4<-glm(RR~plant, data=data4, gaussian("identity"))
  m5<-glm(RR~graz, data=data4, gaussian("identity"))
  m6<-glm(RR~system+leg, data=data4, gaussian("identity"))
  m7<-glm(RR~system+plant, data=data4, gaussian("identity"))
  m8<-glm(RR~system+graz, data=data4, gaussian("identity"))
  m9<-glm(RR~leg+plant, data=data4, gaussian("identity"))
  m10<-glm(RR~leg+graz, data=data4, gaussian("identity"))
  m11<-glm(RR~plant+graz, data=data4, gaussian("identity"))
  m12<-glm(RR~system+leg+plant, data=data4, gaussian("identity"))
  m13<-glm(RR~system+leg+graz, data=data4, gaussian("identity"))
  m14<-glm(RR~system+plant+graz, data=data4, gaussian("identity"))
  m15<-glm(RR~leg+plant+graz, data=data4, gaussian("identity"))
  m16<-glm(RR~system+leg+plant+graz, data=data4, gaussian("identity"))
  m17<-glm(RR~focous, data=data4, gaussian("identity"))
  m18<-glm(RR~system+focous, data=data4, gaussian("identity"))
  m19<-glm(RR~leg+focous, data=data4, gaussian("identity"))
  m20<-glm(RR~plant+focous, data=data4, gaussian("identity"))
  m21<-glm(RR~graz+focous, data=data4, gaussian("identity"))
  m22<-glm(RR~system+leg+focous, data=data4, gaussian("identity"))
  m23<-glm(RR~system+plant+focous, data=data4, gaussian("identity"))
  m24<-glm(RR~system+graz+focous, data=data4, gaussian("identity"))
  m25<-glm(RR~leg+plant+focous, data=data4, gaussian("identity"))
  m26<-glm(RR~leg+graz+focous, data=data4, gaussian("identity"))
  m27<-glm(RR~plant+graz+focous, data=data4, gaussian("identity"))
  m28<-glm(RR~system+leg+plant+focous, data=data4, gaussian("identity"))
  m29<-glm(RR~system+leg+graz+focous, data=data4, gaussian("identity"))
  m30<-glm(RR~system+plant+graz+focous, data=data4, gaussian("identity"))
  m31<-glm(RR~leg+plant+graz+focous, data=data4, gaussian("identity"))
  m32<-glm(RR~system+leg+plant+graz+focous, data=data4, gaussian("identity"))
  
  ranks<-m.AICc(list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16, m17, m18, m19, m20, m21, m22,m23,m24,m25,m26,m27,m8,m29,m30,m31, m32),as.integer(c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16", "17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32")),length(data4$RR))
  
  top_ranked[[i]]<-ranks[1,]
  null_model_AICc[[i]]<-ranks[ranks$name=="1",]
  null_model_position[[i]]<-which(ranks$name=="1",)
  coef[[i]]<-list(summary(m4))## mudar
  r2[[i]]<-summary(lm(data4$RR~data4$plant))##data4$plant+data4$grazmudar esse final para o modelo mais plausível
}



## Transforming lists in data.frames
df_top_ranked<-data.frame(matrix(unlist(top_ranked), nrow=10000, byrow=T))
df_null_model_AICc<-data.frame(matrix(unlist(null_model_AICc), nrow=10000, byrow=T))
df_null_model_position<-data.frame(matrix(unlist(null_model_position), nrow=10000, byrow=T))

## Saving data.frames
write.table(df_top_ranked, "Top_ranked.txt",quote=F)
write.table(df_null_model_AICc, "Null_model_AICc.txt",quote=F)
write.table(df_null_model_position, "Null_model_position.txt",quote=F)
sink("Coefficients.txt") 
lapply(coef, print) 
sink() 
sink("R2.txt") 
lapply(r2, print) 
sink() 

## Top-model frequence

plausible_mod_frequency<-as.data.frame(ftable(df_top_ranked[,7]))
plausible_mod_table_AICc<-aggregate(df_top_ranked, list(df_top_ranked[,7]), mean)
plausible_Null_table_AICc<-aggregate(df_null_model_AICc, list(df_null_model_AICc[,7]), mean)

## testing normality

plot(m4)
residuals(m4)
hist(residuals(m4))
plot(residuals(model4))

## Saving data.frames
write.table(plausible_mod_frequency, "Plausible_mod_frequency.txt",quote=F, sep="\t")
write.table(plausible_mod_table_AICc, "Plausible_mod_table_AICc_mean.txt",quote=F, sep="\t")
write.table(plausible_Null_table_AICc, "Plausible_Null_table_AICc_mean.txt",quote=F, sep="\t")

## R2 values for top-ranked model
r2_estimate<-list()
for(i in 1:100){
  r2<-r2
  df<-data.frame(r2[[i]][9])
  r2_estimate[[i]]<-df[,1]
}
df_r2_estimate<-data.frame(matrix(unlist(r2_estimate), nrow=100, byrow=T))
r2_table<-cbind(df_top_ranked[,7],r2_estimate)
r2_plausible_model<-subset(r2_table, r2_table[,1]==4) # LEMBRAR DE MUDAR O SEU NÚMERO 5, PELO NÚMERO DO SEU MELHOR MODELO
r2_plausible_model_mean<-mean(as.numeric(r2_plausible_model[,2]))
write.table(r2_plausible_model_mean, "m.r2_plausible_model.txt", quote=F, sep="\t")

# Coeficient values for top-ranked model - FACTORS
top_ranked_model.x<-as.data.frame(which(df_top_ranked[,7]==4)) ##MUDAR
write.table(top_ranked_model.x,"top_ranked_model.1.5.txt", sep="\t")
## Excel file

join<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21,df22,df23,df24,df25,df26,df27,df28,df29,df30,df31,df32,df33,df34,df35,df36,df37,df38,df39,df40,df41,df42,df43)

write.table(coef_plausible_model, "m.Coef_plausible_model.txt", quote=F, sep="\t")
## Mean using excel by removing numbers from the first column

##
df1<-data.frame(coef[[8]][[1]][12])
df2<-data.frame(coef[[16]][[1]][12])
df3<-data.frame(coef[[21]][[1]][12])
df4<-data.frame(coef[[23]][[1]][12])
df5<-data.frame(coef[[25]][[1]][12])
df6<-data.frame(coef[[29]][[1]][12])
df7<-data.frame(coef[[30]][[1]][12])
df8<-data.frame(coef[[31]][[1]][12])
df9<-data.frame(coef[[33]][[1]][12])
df10<-data.frame(coef[[38]][[1]][12])
df11<-data.frame(coef[[39]][[1]][12])
df12<-data.frame(coef[[45]][[1]][12])
df13<-data.frame(coef[[48]][[1]][12])
df14<-data.frame(coef[[57]][[1]][12])
df15<-data.frame(coef[[58]][[1]][12])
df16<-data.frame(coef[[59]][[1]][12])
df17<-data.frame(coef[[65]][[1]][12])
df18<-data.frame(coef[[71]][[1]][12])
df19<-data.frame(coef[[75]][[1]][12])
df20<-data.frame(coef[[76]][[1]][12])
df21<-data.frame(coef[[80]][[1]][12])
df22<-data.frame(coef[[96]][[1]][12])
df23<-data.frame(coef[[97]][[1]][12])
df24<-data.frame(coef[[99]][[1]][12])

join<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21, df22, df23,df24)


write.table(join, "m.Coef_plausible_model.bind.txt", quote=F, sep="\t")
