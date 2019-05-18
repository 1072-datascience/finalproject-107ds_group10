
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
