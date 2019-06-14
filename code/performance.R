Acc <- function(PredData, ActualData, TargetColName){
  return (sum(PredData[[TargetColName]] == ActualData[[TargetColName]]) / nrow(PredData))
}
