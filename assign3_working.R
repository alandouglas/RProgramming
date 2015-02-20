best <- function(state, outcome) {

  the_data_1 <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=FALSE,na.strings="Not Available")
  the_data_2 <- subset(the_data_1,State==state)
  
  if(nrow(the_data_2)==0){
    stop("invalid state")
  }
  
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
  
  the_data_3 <- the_data_2[,c(2,7,which_one)]
  the_data_4 <- na.omit(the_data_3)
  column_name <- colnames(the_data_4)[3]
  double_order <- order(the_data_4[,column_name],the_data_4[,"Hospital.Name"])
  the_data_5 <- the_data_4[double_order,]
  the_data_5[1,1]
}


rankhospital <- function(state,outcome,num="best"){
  
  the_data_1 <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=FALSE,na.strings="Not Available")
  the_data_2 <- subset(the_data_1,State==state)
  
  if(nrow(the_data_2)==0){
    stop("invalid state")
  }
  
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
  
  the_data_3 <- the_data_2[,c(2,7,which_one)]
  the_data_4 <- na.omit(the_data_3)
  column_name <- colnames(the_data_4)[3]
  double_order <- order(the_data_4[,column_name],the_data_4[,"Hospital.Name"])
  the_data_5 <- the_data_4[double_order,]
  
  row_to_look_up <- 3
  
  if(nrow(the_data_5)<num){
    return NA
  }
  
  if(num=="best"){
    row_to_look_up <- 1
  }
  
  if(num=="worst"){
    row_to_look_up <- nrow(the_data_5)
  }
  
  the_data_5[row_to_look_up,1]
}


rankall <- function(outcome,num="best"){
  
  the_data_1 <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  if(!(outcome=="heart attack"|outcome=="heart failure"|outcome=="pneumonia")){
    stop("invalid outcome")
  }
  
  if(num=="best"){
    num <- 1
  }
  
}