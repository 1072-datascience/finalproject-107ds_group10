rm(list=ls());cat('\f')


library('ggplot2');library('ggthemes')
# Loading Train Data
dTrBuy <- read.table('data/train_buy_info.csv',header = TRUE, sep = ',')
dTrCust <- read.table('data/train_cust_info.csv',header = TRUE, sep = ',')
dTrTpy <- read.table('data/train_Tpy_info.csv',header = TRUE, sep = ',')

# head(dTrBuy)
# head(dTrCust)
# head(dTrTpy)

# sort by column
SortCol <- function(DataFrame,ColName){
  return (DataFrame[order(DataFrame[ColName]),])
}

dTrBuy <- SortCol(dTrBuy,'CUST_ID')
dTrCust <- SortCol(dTrCust,'CUST_ID')
dTrTpy <- SortCol(dTrTpy,'CUST_ID')

rm(SortCol)

dTrCust$CUST_ID <- NULL
dTrTpy$CUST_ID <- NULL
# Merge By CUST_ID
dTrAll <- cbind(dTrBuy,dTrCust,dTrTpy)

dTrAll$BUY_YEAR <- NULL

# Transfer BUY_MONTH into factor
dTrAll$BUY_MONTH <- factor(dTrAll$BUY_MONTH)

summary(dTrAll)

# NA-ratio
NA_Ratio <- data.frame(rbind(round(sapply(dTrAll,function(x) sum(is.na(x)))/nrow(dTrAll),digits = 2),sapply(dTrAll,function(x) sum(is.na(x)))))
rownames(NA_Ratio) <- c('Amount','Proportion')
NA_Ratio <- NA_Ratio[,NA_Ratio[2,]>0]
print(t(NA_Ratio))

sapply(dTrAll,function(x) length(unique(x)))


# plot  result distribution for every feature
Vars <- colnames(dTrAll[,-c(1,2)])
Var_Cat <- Vars[sapply(dTrAll[,Vars],class) %in% c('factor','character')]
Var_Num <- Vars[sapply(dTrAll[,Vars],class) %in% c('numeric','integer')]
rm(Vars)
# count for different y
print(ggplot(dTrAll, aes(x=BUY_TYPE,fill = factor(BUY_TYPE)))
      + geom_bar(stat='count', position='dodge')
      + labs(x = 'BUY_TYPE')
      + theme_few())

#for multi-line comment
if (FALSE){
for (c in Var_Cat){
  print(c)
  pdf(paste('img/data/Cat/',c,'.pdf',sep = "" ) )
  print(ggplot(dTrAll, aes_string(x=c,fill = 'factor(BUY_TYPE)'))
        + geom_bar(stat='count', position='dodge')
        + labs(x = c)
        + theme_few())
  dev.off()
}
for (c in Var_Num){
  print(c)
  pdf(paste('img/data/Num/Bar-',c,'.pdf',sep = "" ) )
  print(ggplot(dTrAll, aes_string(x=c,fill = 'factor(BUY_TYPE)')) 
        + geom_bar(stat='count', position='dodge') 
        + labs(x = c) 
        + theme_few())
  dev.off()
  
  pdf(paste('img/data/Num/Den-',c,'.pdf',sep = "" ) )
  print(ggplot(dTrAll, aes_string(x=c))+
    geom_density(aes(colour=factor(BUY_TYPE), fill=factor(BUY_TYPE)))+
    facet_wrap(~factor(BUY_TYPE))+
    theme_few())
  dev.off()
}
}
rm(dTrTpy,dTrBuy,dTrCust)
save.image("01.RData")