cat('LoadData!\n')

#Load csv
dTrBuy <- read.table(File_Tr1, header = TRUE, sep = ',')
dTrCust <- read.table(File_Tr2,header = TRUE, sep = ',')
dTrTpy <- read.table(File_Tr3,header = TRUE, sep = ',')
dTeBuy <- read.table(File_Te1 ,header = TRUE, sep = ',')
dTeCust <- read.table(File_Te2,header = TRUE, sep = ',')
dTeTpy <- read.table(File_Te3,header = TRUE, sep = ',')

Answer <-  read.table(Args_answer,header = TRUE, sep = ',')
# Sort By ID in order to combine data from different files
SortCol <- function(DataFrame,ColName){
  return (DataFrame[order(DataFrame[ColName]),])
}
dTrBuy <- SortCol(dTrBuy,'CUST_ID')
dTrCust <- SortCol(dTrCust,'CUST_ID')
dTrTpy <- SortCol(dTrTpy,'CUST_ID')
dTrCust$CUST_ID <- NULL
dTrTpy$CUST_ID <- NULL
dTrAll <- cbind(dTrBuy,dTrCust,dTrTpy)

dTeBuy <- SortCol(dTeBuy,'CUST_ID')
dTeCust <- SortCol(dTeCust,'CUST_ID')
dTeTpy <- SortCol(dTeTpy,'CUST_ID')
dTeCust$CUST_ID <- NULL
dTeTpy$CUST_ID <- NULL
dTeAll <- cbind(dTeBuy,dTeCust,dTeTpy)

Answer <-  SortCol(Answer,'CUST_ID')


#Remove useless Vars
rm('File_Tr1','File_Tr2','File_Tr3','File_Te1','File_Te2','File_Te3',
   'dTrBuy','dTrCust','dTrTpy','dTeBuy','dTeCust','dTeTpy','SortCol')

source('code/DataProcess.R')