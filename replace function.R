getrid <- function(i){
  dem_2_nd[,i][is.na(dem_2_nd[,i])] <- dempt_2[,i][is.na(dem_2_nd[,i])]
}