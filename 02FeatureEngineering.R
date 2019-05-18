rm(list=ls()) ; load('01.RData') ; cat('\f')

library('ggplot2');library('ggthemes')
#Map Y into number
dTrAll$BUY <- as.vector(sapply(as.character(dTrAll$BUY_TYPE),utf8ToInt)) -97

#Train Test Val Split
#Shuffle data
dTrAll <- dTrAll[sample(nrow(dTrAll)),]

Y <- unique(dTrAll$BUY)
dTrAll$folds <- 0
nfolds <- 10
for (y in Y){
  dTrAll$folds[dTrAll$BUY  == y] <-  cut(seq(1,sum(dTrAll$BUY == y)),breaks=nfolds,labels=FALSE)
}


#TODO: for loop to CV
library(data.table)
i <- 1
#Use 10th fold to test, 1~9th folds to train/val
Test.Indexes <- which((dTrAll$folds) == nfolds , arr.ind=TRUE)
Val.Indexes <-  which(dTrAll$folds== (i %% (nfolds-1)) ,arr.ind=TRUE)

dTrain <- dTrAll[-c(Test.Indexes,Val.Indexes),]
dVal <- dTrAll[Val.Indexes,]

#NA Dealing
dTrainFix <- copy(dTrain)

Mean_HEIGHT <- mean(dTrain$HEIGHT,na.rm = T)
Mean_WEIGHT <- mean(dTrain$WEIGHT,na.rm = T)

# Replace NA into mean(train) and truncate at +- Range

Threshold <- 5

tmp <- dTrain$HEIGHT
tmp[is.na(tmp)] <- Mean_HEIGHT
tmp[tmp > Threshold] <- Threshold
tmp[tmp < -Threshold] <- -Threshold
dTrainFix$HEIGHT <- tmp


tmp <- dTrain$WEIGHT
tmp[is.na(tmp)] <- Mean_WEIGHT
tmp[tmp > Threshold] <- Threshold
tmp[tmp < -Threshold] <- -Threshold
dTrainFix$WEIGHT <- tmp

library(gridExtra)
plotH_B <- ggplot(dTrain[!is.na(dTrain$HEIGHT),],aes(x = HEIGHT)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666",adjust = 2) + labs(x = 'HEIGHT-Before')
plotH_A <- ggplot(dTrainFix,aes(x = HEIGHT)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666",adjust = 2) + labs(x = 'HEIGHT-After')
grid.arrange(plotH_B, plotH_A, nrow=2, ncol=1)

plotW_B <- ggplot(dTrain[!is.na(dTrain$WEIGHT),],aes(x = WEIGHT)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666",adjust = 2) + labs(x = 'WEIGHT-Before')
plotW_A <- ggplot(dTrainFix,aes(x = WEIGHT)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666",adjust = 2) + labs(x = 'WEIGHT-After')
grid.arrange(plotW_B, plotW_A, nrow=2, ncol=1)

# pdf('img/data/NAfix/HEIGHT-After.pdf')
# dev.off()

#Replace NA with 'NA' in catagorical columns
Var_Cat_NA <-  colnames(dTrainFix)[(sapply(dTrainFix,function(x) sum(is.na(x))) > 0)]
for (c in Var_Cat_NA){
  dTrainFix[,c] <- `levels<-`(addNA(dTrainFix[,c]), c(levels(dTrainFix[,c]), 'NA'))
  #tmp2 <- factor(ifelse(is.na(tmp), 'NA', paste(tmp)), levels = c(levels(tmp), 'NA'))
}
#Dummy Var

library('dataPreparation')
OHEmodel <- build_encoding(dTrainFix, cols = Var_Cat, verbose = TRUE)
dTrainFix_OHE <- one_hot_encoder(dTrainFix, encoding = OHEmodel, drop = TRUE)
