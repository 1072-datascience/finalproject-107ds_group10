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

i <- 1
#Use 10th fold to test, 1~9th folds to train/val
Test.Indexes <- which((dTrAll$folds) == nfolds , arr.ind=TRUE)
Val.Indexes <-  which(dTrAll$folds== (i %% (nfolds-1)) ,arr.ind=TRUE)

dTrain <- 

#NA Dealing
dTrAllFix <- dTrAll

# HEIGHT/WEIGHT NA fix
# Density before
# # pdf('img/data/NAfix/HEIGHT-Before.pdf')
# print(ggplot(dTrAll,aes(x = HEIGHT)) +
#   geom_histogram(aes(y=..density..), colour="black", fill="white")+
#   geom_density(alpha=.2, fill="#FF6666",adjust = 2) +
#     labs(x = 'HEIGHT-Before'))
# dev.off()
summary(dTrAll$HEIGHT)
Heightfix <- dTrAll$HEIGHT
# replace NA with mean
Heightfix[is.na(Heightfix)] <- mean(Heightfix,na.rm = T)
# truncate on -3 and 3
Heightfix[Heightfix > 5] <-  5
Heightfix[Heightfix < -5] <-  -5

dTrAllFix$HEIGHT <- Heightfix
# Density After
# pdf('img/data/NAfix/HEIGHT-After.pdf')
# print(ggplot(data.frame(Heightfix),aes(x = Heightfix)) +
#   geom_histogram(aes(y=..density..), colour="black", fill="white")+
#   geom_density(alpha=.2, fill="#FF6666",adjust = 2) +
#   labs(x = 'HEIGHT-After'))
# dev.off()

# print(ggplot(dTrAll,aes(x = WEIGHT)) +
#   geom_histogram(aes(y=..density..), colour="black", fill="white")+
#   geom_density(alpha=.2, fill="#FF6666",adjust = 2) +
#     labs(x = 'WEIGHT-Before'))

Weightfix <- dTrAll$WEIGHT
# replace NA with mean
Weightfix[is.na(Weightfix)] <- mean(Weightfix,na.rm = T)
# truncate on -3 and 3
Weightfix[Weightfix > 5] <-  5
Weightfix[Weightfix < -5] <-  -5

dTrAllFix$WEIGHT <- Weightfix
# print(ggplot(data.frame(Weightfix),aes(x = Weightfix)) +
#   geom_histogram(aes(y=..density..), colour="black", fill="white")+
#   geom_density(alpha=.2, fill="#FF6666",adjust = 2) +
#   labs(x = 'WEIGHT-After'))

#Replace NA with 'NA'
Var_Cat_NA <-  colnames(dTrAllFix)[(sapply(dTrAllFix,function(x) sum(is.na(x))) > 0)]
for (c in Var_Cat_NA){
  dTrAllFix[,c] <- `levels<-`(addNA(dTrAllFix[,c]), c(levels(dTrAllFix[,c]), 'NA'))
  #tmp2 <- factor(ifelse(is.na(tmp), 'NA', paste(tmp)), levels = c(levels(tmp), 'NA'))
}
#Dummy Var

library('dataPreparation')
OHEmodel <- build_encoding(dTrAllFix, cols = Var_Cat, verbose = TRUE)
dTrAllFixOHE <- one_hot_encoder(dTrAllFix, encoding = OHEmodel, drop = TRUE)
