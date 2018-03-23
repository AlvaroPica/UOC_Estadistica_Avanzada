
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

long_names <- names(data)
short_names <- short.variables(var_names)
names(data) <- short_names 


#Ejercicio 2


sapply(mydata, class)
cco <- "Categorica-cualitativa-ordinal"
ccn <- "Categorica-cualitativa-nominal"
cnd <- "Cuantitativa Discreta"
cnc <- "Cuantitativa Continua"
mis_categorias <- c(ccn, ccn, cco, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc)
mydf <- as.data.frame(setNames(mis_categorias, names(data)))

#Se observa que las  variable HS, GpC debería ser numérica pero está definida como factor.
#Se observa que la variable HR debería ser un factor pero está definida como integer.

variables <- as.data.frame(cbind(long_names, short_names, mis_categorias))
head(variables)

sapply(data, class)

head(data)

data$HS <- as.numeric(as.character(data$HS))
head(data$HS)

Encoding("í")
mydata.largo

