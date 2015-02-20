rankall <- function(outcome,num="best"){
  
  the_data_1 <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=FALSE,na.strings="Not Available")
  
  if(!(outcome=="heart attack"|outcome=="heart failure"|outcome=="pneumonia")){
    stop("invalid outcome")
  }
  
  which_one <- 1
  
  if(outcome=="heart attack"){
    which_one <- 11
  }
  
  if(outcome=="heart failure"){
    which_one <- 17
  }
  
  if(outcome=="pneumonia"){
    which_one <- 23
  }
  
  the_data_3 <- the_data_1[,c(2,7,which_one)]
  the_data_4 <- na.omit(the_data_3)
  colnames(the_data_4)<-c("hospital","state","blah")
  double_order <- order(the_data_4[,"blah"],the_data_4[,"hospital"])
  the_data_5 <- the_data_4[double_order,]

  row_to_look_up <- 3
  
  states_vector <- unique(the_data_5[,"state"])
  states_vector_sorted <- states_vector[order(states_vector)]
  
  final.data <- data.frame(1:54,states_vector_sorted)
  rownames(final.data) <- states_vector_sorted
  colnames(final.data) <- c("hospital","state")
  
  for(i in states_vector_sorted){
    
    the_data_yo <- subset(the_data_5,state==i)
    
    if(num=="best"){
      row_to_look_up <- 1
    }
    else if(num=="worst"){
      row_to_look_up <- nrow(the_data_yo)
    }
    else{
      row_to_look_up <- num
    }
    
    if(nrow(the_data_yo)<row_to_look_up){
      NA
    }
    
    value_found <- the_data_yo[row_to_look_up,1]
    final.data[i,1] <- value_found
  }
  
  final.data
}