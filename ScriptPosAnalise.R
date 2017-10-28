library(data.table)

# Organizando dados ----
# Working with top ranked
top.ranked.list <- list.files('./Ellen', pattern = 'TopRanked', full.names = TRUE)
# Organizando 
top.ranked.list <- sort(top.ranked.list)
head(top.ranked.list)
# Lendo todos os txts
top <- lapply(top.ranked.list, read.table)
# Organizando resultado
Top.Ranked <- as.data.frame(rbindlist(top))
head(Top.Ranked)

# Identificando os mais frequentes
TopRank.Table <- as.data.frame(table(Top.Ranked$name))
# Ordenando pela frequencia
TopRank.Table <- TopRank.Table[order(TopRank.Table$Freq, decreasing = TRUE),]
head(TopRank.Table)
names(TopRank.Table) <- c('ModelName', "Freq")
# Inicio pos analise ----
#write.csv(TopRank.Table, './Results/TopRankTable.csv', row.names = F)
TopRank.Table <- read.csv('./Results/TopRankTable.csv')

# identifying with iteraction had this model as top ranked, to load its r2:
winner <- as.integer(paste(TopRank.Table[1,1]))
numbers.top <- which(Top.Ranked$name == winner)
head(numbers.top)
# confirm 
#read.table(top.ranked.list[4])

# subsetting results of the topranked
WinData <- Top.Ranked[which(Top.Ranked$name == winner),]
head(WinData)
dim(WinData)
AVGData <- colMeans(WinData)

# Loading and organizing R2 results
r2.list <- list.files('./Ellen', pattern='R2', full.names=TRUE)
r2.list <- sort(r2.list)
head(r2.list)
head(top.ranked.list)
r2Results <- NULL
r2 <- NULL
r2a <- NULL
  for (a in 1:length(numbers.top)){
  summary <- read.delim(r2.list[numbers.top[a]], header=F)
  #summary <- read.delim(r2.list[numbers.top[a]])
  #read.delim(r2.list[
  
  #r2  
  r2[a] <- paste(summary[grep("dendf", summary[,])-2,])
  r2[a] <- as.numeric(substr(r2[a], 5, nchar(r2[a])))
  
  #R2 adjusted
  r2a[a] <- paste(summary[grep("dendf", summary[,])-1,])
  r2a[a] <- as.numeric(substr(r2a[a], 5, nchar(r2a[a])))
  }

Rresults <- cbind(as.numeric(r2), as.numeric(r2a))
names(Rresults) <- c('R2', 'R2Adjusted')

AVGR2 <- (colMeans(Rresults))

# Working with coeff
coef.list <- list.files('./Ellen', pattern = 'Coefficients', full.names = TRUE)
coef.list <- sort(coef.list)
length(coef.list)
head(coef.list)

AIC <- NULL
model <- NULL
for (a in 1:length(numbers.top)){
  coef <- read.delim(coef.list[numbers.top[a]])
  
  #r2  
  AIC[a] <- paste(coef[grep("AIC", coef[,]),])
  AIC[a] <- as.numeric(paste(substr(AIC[a], 5, nchar(AIC[a]))))
  
  # Getting model
  model[a] <- paste(coef[1,])
  }

AIC <- (as.numeric(AIC))
mean(AIC)

AVGData <- as.matrix(c(AVGData, AVGR2[1], AVGR2[2], mean(AIC)))
rownames(AVGData)[9:11] <- c('avgR2', 'avgR2Adjusted', 'avgAIC')
# saving result
write.csv(AVGData, './Results/AVGWinData.csv')

head(model)

write.csv(as.data.frame(model), './Results/models.csv')

# identificando coeficientes ----

for (a in 1:length(numbers.top)){
  #a=1
  coef <- read.delim(coef.list[numbers.top[a]])
  
  #r2  
  teste <- paste(coef[
    (grep("Coefficients", coef[,])+1):
      (grep("Coefficients", coef[,])+5)
    ,])
  write.csv(as.data.frame(teste), paste0('./coefWin_', numbers.top[a],".txt"))
}
