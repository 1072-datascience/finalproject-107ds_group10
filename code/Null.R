#Null model by mode
cat('Null model training!\n')
dPred <- dTeAll['CUST_ID']
dPred$BUY_TYPE <- getmode(dTrAll$BUY_TYPE)
cat('The accuracy of null model by getting the mode observation is:\n')
cat(Acc(dPred,Answer,'BUY_TYPE'),'\n')

if (dir.exists(paste(Args_output,'Null/',sep = '')) == FALSE )
  dir.create(paste(Args_output,'Null/',sep=''))
write.csv(dPred, file = paste(Args_output,'Null/prediction.csv',sep = '') , row.names = FALSE, quote = FALSE)

CM <- confusionMatrix(dPred$BUY_TYPE,Answer$BUY_TYPE)

CSVOverall <- round(data.frame(CM$overall),digits = 4)
CSVByClass <- round(data.frame(CM$byClass),digits = 4)

# You can then use
write.csv(CSVOverall,file= paste(Args_output,'Null/Overall.csv', sep = '') )
write.csv(CSVByClass,file= paste(Args_output,'Null/ByClass.csv', sep = '') )
rm('CM','CSVByClass','CSVOverall')
