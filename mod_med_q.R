
med_mod_q <- function(x){
  median(x) -> med
  freq <- table(x)
  as.numeric(names(freq)[which.max(freq)]) -> mod
  output <- list(med, mod, quantile(x))
  return(output)
}

