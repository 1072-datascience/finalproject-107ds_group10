# unload all package for clean env
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
rm(list=ls());cat('\f')

PlotOrNot <- FALSE
# Load packages
library('ggplot2');library('ggthemes')

# Loading Train Data
dTrBuy <- read.table('data/train_buy_info.csv',header = TRUE, sep = ',')
dTrCust <- read.table('data/train_cust_info.csv',header = TRUE, sep = ',')
dTrTpy <- read.table('data/train_Tpy_info.csv',header = TRUE, sep = ',')

# sort by column
SortCol <- function(DataFrame,ColName){
  return (DataFrame[order(DataFrame[ColName]),])
}

# Sort By ID in order to combine data from different files
dTrBuy <- SortCol(dTrBuy,'CUST_ID')
dTrCust <- SortCol(dTrCust,'CUST_ID')
dTrTpy <- SortCol(dTrTpy,'CUST_ID')
rm(SortCol)

dTrCust$CUST_ID <- NULL
dTrTpy$CUST_ID <- NULL
# Merge By CUST_ID
dTrAll <- cbind(dTrBuy,dTrCust,dTrTpy)

# remove constant column
dTrAll$BUY_YEAR <- NULL

# Transfer BUY_MONTH into factor
dTrAll$BUY_MONTH <- factor(dTrAll$BUY_MONTH)

summary(dTrAll)

# NA-ratio
NA_Ratio <- data.frame(rbind(round(sapply(dTrAll,function(x) sum(is.na(x)))/nrow(dTrAll),digits = 2),sapply(dTrAll,function(x) sum(is.na(x)))))
rownames(NA_Ratio) <- c('Proportion','Amount')
NA_Ratio <- NA_Ratio[,NA_Ratio[2,]>0]
print(t(NA_Ratio))

# Amount of Unique for each columns
sapply(dTrAll,function(x) length(unique(x)))

# plot distribution for every feature
Vars <- colnames(dTrAll[,-c(1,2)])
Var_Cat <- Vars[sapply(dTrAll[,Vars],class) %in% c('factor','character')]
Var_Num <- Vars[sapply(dTrAll[,Vars],class) %in% c('numeric','integer')]
rm(Vars)

PlotByGroup <- function(Data,x,Group,Title,Relative = FALSE){
  Free <- ifelse(Relative,'free_y','fixed')
  Formula <- paste('~factor(',Group,')')
  print(ggplot(Data, aes_string(x = x,fill='factor(BUY_TYPE)') ) + 
          geom_bar(stat='count', position='dodge') + 
          labs(x = x) +
          facet_wrap(as.formula(Formula),scales = Free)  + 
          theme_few() +
          theme(axis.text.x = element_text(size=5, angle=90)) +  
          ggtitle(Title) )
}

# count for different targets
pdf('img/data/OHE-Before/Target.pdf')
print(ggplot(dTrAll, aes(x=BUY_TYPE,fill = factor(BUY_TYPE)))
      + geom_bar(stat='count', position='dodge')
      + labs(x = 'BUY_TYPE')
      + theme_few())
dev.off()

#for multi-line comment
if (PlotOrNot) {
#figure for categorical features
pdf('img/data/OHE-Before/Categorical.pdf' )
print('Plotting Categorical feature')
for (c in c(Var_Cat,'CHILD_NUM')){
  print(paste('Plotting:',c))
  print(ggplot(dTrAll, aes_string(x=c)) 
        + geom_bar(stat='count') 
        + labs(x = c) 
        + theme_few()
        + ggtitle(paste('Full-',c)))
  print(ggplot(dTrAll, aes_string(x=c,fill = 'factor(BUY_TYPE)'))
        + geom_bar(stat='count', position='dodge')
        + labs(x = c)
        + theme_few())
}
dev.off()

#figure for numerical features
pdf('img/data/OHE-Before/Numeric.pdf')
print('Plotting Numerical feature')
for (c in c('HEIGHT','WEIGHT','BUDGET')){
  print(paste('Plotting:',c))
  print(ggplot(dTrAll[!is.na(dTrAll$WEIGHT),],aes_string(x = c)) +
          geom_histogram(aes(y=..density..), colour="black", fill="white") +
          geom_density(alpha=.2, fill="#FF6666",adjust = 2) + 
          labs(x = c)+
          ggtitle(paste('Full -',c)))
    
  print(ggplot(dTrAll[!is.na(dTrAll[,c]),], aes_string(x=c,fill = 'factor(BUY_TYPE)')) 
        + geom_bar(stat='count', position='dodge') 
        + labs(x = c) 
        + theme_few()
        + ggtitle(paste('Bar-',c)))
  
  print(ggplot(dTrAll[!is.na(dTrAll[,c]),], aes_string(x=c))+
          geom_density(aes(colour=factor(BUY_TYPE), fill=factor(BUY_TYPE)))+
          facet_wrap(~factor(BUY_TYPE))+
          theme_few()
        + ggtitle(paste('Density-',c)))
  print(ggplot(dTrAll))
}
dev.off()

pdf('img/data/OHE-Before/GroupCat.pdf')
print('Plotting grouping figure')
for (c in c(Var_Cat,'CHILD_NUM')){
  print(paste('Plotting:', c))
  PlotByGroup(dTrAll,x = c, Group = 'BUY_TYPE',
              Relative = F, Title = paste(c,'\n-Groupby BUY_TYPE,\n Fixed Y'))
  PlotByGroup(dTrAll,x = c, Group = 'BUY_TYPE',
              Relative = T, Title = paste(c,'\n-Groupby BUY_TYPE,\n Relative Y'))
  
  PlotByGroup(dTrAll,x = 'BUY_TYPE', Group = c,
              Relative = F, Title = paste(c,'\n-Groupby',c,',\n Fixed Y'))  
  PlotByGroup(dTrAll,x = 'BUY_TYPE', Group = c,
              Relative = T, Title = paste(c,'\n-Groupby',c,',\n Relative Y'))
}
dev.off()
}


rm(dTrTpy,dTrBuy,dTrCust)
#save image for convenience
save.image("01.RData")