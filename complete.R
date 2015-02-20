complete <- function(directory, id=1:332){
  files_full <- list.files(directory,full.names=TRUE)
  
  resultant_length <- length(id)
  resultant_frame <- data.frame(matrix(NA, nrow = resultant_length, ncol = 2))
  colnames(resultant_frame) <- c("id","nobs")
  
  current_resultant_row <- 1
  
  for(i in id){
    dat_current <- read.csv(files_full[i])
    dat_chopped <- na.omit(dat_current)
    
    resultant_frame$id[current_resultant_row] <- i
    resultant_frame$nobs[current_resultant_row] <- nrow(dat_chopped)
    current_resultant_row <- current_resultant_row + 1
  }
  resultant_frame
  
}