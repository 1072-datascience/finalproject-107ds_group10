cat('Load Args!\n')
# unload all package for clean env
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
rm(list = ls())
cat('\f')
#Functions for Args
isDigit <- function(str){
  return (!is.na(as.numeric(str)))
}

FindArgs <- function(str,para){
  Index <- which(sapply(str, function(y) para %in% y))
  if (length(Index) == 0)
    return (-1)
  else
    return(Index[[1]])
}

#Error stop function
Usage <- function(s=NULL){
  mess <- "USAGE: Rscript code/Cathay.R --nfolds 5 --train data/train/ --test data/test/ --output results/ --answer data/Description/test_buy_y_info.csv"
  mess <- paste(s, mess,'\n', sep="\n")
  stop(mess, call. = FALSE)
}

# Load Args
args <-  commandArgs(trailingOnly = TRUE)
#For Debug
#args <- '--nfolds 5 --train data/train/ --test data/test/ --output results/ --answer data/Description/test_buy_y_info.csv'
#args <- strsplit(args,split=' ', fixed=TRUE)[[1]]

#Checking error for Args
if (length(args) == 0) {
  Usage()
}

{
# Args init
Args_nfolds <- 5
Args_train <- 'EMPTY'
Args_test <- 'EMPTY'
Args_output <- 'EMPTY'
Args_answer <- 'EMPTY'
#Location of Args
L_nfolds <- FindArgs(args,'--nfolds')
L_train <-  FindArgs(args,'--train')
L_test <- FindArgs(args,'--test')
L_output <- FindArgs(args,'--output')
L_answer <- FindArgs(args,'--answer')
#Checkingn Missing Args
if(L_nfolds == -1) Usage(paste('Missing argument: ', '--nfolds'))
if(L_train == -1) Usage(paste('Missing argument: ', '--train'))
if(L_test == -1) Usage(paste('Missing argument: ', '--test'))
if(L_output == -1) Usage(paste('Missing argument: ', '--output'))
if(L_answer == -1) Usage(paste('Missing argument: ', '--answer'))
}

{
#Load Args
Args_nfolds <- args[L_nfolds + 1]
Args_train <- args[L_train + 1]
Args_test <- args[L_test + 1]
Args_output <- args[L_output + 1]
Args_answer <- args[L_answer + 1]
File_Tr1 <- paste(Args_train,'train_buy_info.csv',sep='')
File_Tr2 <- paste(Args_train,'train_cust_info.csv',sep='')
File_Tr3 <- paste(Args_train,'train_tpy_info.csv',sep='')
File_Te1 <- paste(Args_test,'test_buy_x_info.csv',sep='')
File_Te2 <- paste(Args_test,'test_cust_x_info.csv',sep='')
File_Te3 <- paste(Args_test,'test_tpy_x_info.csv',sep='')
}

{
#Checking Dir and File DNE error
for (L in c(Args_train,Args_test,Args_output)){
  #print(L)
  if (dir.exists(L) == FALSE ){
    stop(paste(L,"Directory does not exist\n\n"))
  }
}
for(FILE in c(File_Tr1,File_Tr2,File_Tr3,File_Te1,File_Te2,File_Te3,Args_answer)){
  #print(FILE)
  if (file.exists(FILE) == FALSE ){
    stop(paste(FILE,"file does not exist\n\n" ))
  }
}
}
{
#Checking nfolds error
if (isDigit(Args_nfolds) == FALSE){
  Usage(paste('Wrong fold number:',Args_nfolds,', need to be a number'))}
if (as.integer(Args_nfolds) != as.numeric(Args_nfolds)){
  Usage(paste('Wrong fold number:',Args_nfolds,', need to be an integer'))}
Args_nfolds <- as.integer(Args_nfolds)
if (Args_nfolds < 3){
  Usage(paste('Wrong fold number:',Args_nfolds,', need to greater than 3'))}
}
#Remove useless Variables and Funnctions
rm('FindArgs', 'isDigit', 'Usage', 'args','FILE','L',
   'L_nfolds','L_output','L_test','L_train','L_answer')

# For Debug
cat('Args_nfolds:\t',Args_nfolds,'\n')
cat('Args_train:\t',Args_train,'\n')
cat('Args_test:\t',Args_test,'\n')
cat('Args_output:\t',Args_output,'\n')
cat('Args_answer:\t',Args_answer,'\n')
#Loading Data
source(file = 'code/LoadData.R')