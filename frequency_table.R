freqtb <- function(x){
  cbind(Freq=table(x), Percentage=prop.table(table(x)))
}