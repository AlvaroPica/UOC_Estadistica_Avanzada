data <- read.csv("2016_raw.csv")

short.variables <- function(input1, ...){
for (i in 1:length(input1)){
if (!grepl("\\.", (input1)[i])) {
  next } 
  else { 
    separate.words <- strsplit(input1[i], "\\.")
    number.of.words <- length(separate.words[[1]])
    mystring <- c()
    for (jj in 1:number.of.words){
      myletter <- substr(separate.words[[1]][jj], 1,1)
      mystring <- c(mystring, myletter)
    }
    
    input1[i] <- paste(mystring, collapse = "")

    }
  }
return(input1)  
}

var_names <- names(data)
names(data) <- short.variables(var_names)
names(data)

str(data)
class(data)
sapply(data, class)
summary(data)da