#!/bin/zsh

# Correct
Rscript code/Cathay.R --nfolds 10 --train data/train/ --test data/test/ --output results/ --answer data/Description/test_buy_y_info.csv
Rscript code/Cathay.R --nfolds 3 --train data/train/ --test data/test/ --output results/ --answer data/Description/test_buy_y_info.csv
  
# Wrong
  
#missing Args
Rscript code/Cathay.R --train data/train/ --test data/test/ --output results/ --answer data/Description/test_buy_y_info.csv
Rscript code/Cathay.R --nfolds 10  --test data/test/ --output results/ --answer data/Description/test_buy_y_info.csv
Rscript code/Cathay.R --nfolds 10 --train data/train/  --output results/ --answer data/Description/test_buy_y_info.csv
Rscript code/Cathay.R --nfolds 10 --train data/train/ --test data/test/ --answer data/Description/test_buy_y_info.csv
Rscript code/Cathay.R --nfolds 3 --train data/train/ --test data/test/ --output results/

#Wrong nfolds
  
Rscript code/Cathay.R --nfolds -1 --train data/train/ --test data/test/ --output results/
Rscript code/Cathay.R --nfolds 2 --train data/train/ --test data/test/ --output results/
Rscript code/Cathay.R --nfolds 2.5 --train data/train/ --test data/test/ --output results/
Rscript code/Cathay.R --nfolds 3.3 --train data/train/ --test data/test/ --output results/

#DNE location
Rscript code/Cathay.R --nfolds 10 --train data/trains/ --test data/test/ --output results/
Rscript code/Cathay.R --nfolds 10 --train data/train/ --test data/tests/ --output results/
Rscript code/Cathay.R --nfolds 10 --train data/train/ --test data/test/ --output result/
Rscript code/Cathay.R --nfolds 10 --train data/train/ --test data/test/ --output results/ --answer data/Description/test_buy_y_infos.csv
