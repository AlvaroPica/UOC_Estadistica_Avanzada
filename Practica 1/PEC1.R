
#Ejercicio 1 

  #Valores separados por ; por lo que se utiliza read.csv y no read.csv2
  data <- read.csv("2016_raw.csv")

#Ejercicio 2

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
  short_names <- short.variables(long_names)
  names(data) <- short_names
  names(data)

#Ejercicio 3

  sapply(mydata, class)
  cco <- "Categorica-cualitativa-ordinal"
  ccn <- "Categorica-cualitativa-nominal"
  cnd <- "Cuantitativa Discreta"
  cnc <- "Cuantitativa Continua"
  mis_categorias <- c(ccn, ccn, cco, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc)
  mydf <- as.data.frame(setNames(mis_categorias, names(data)))
  mydf

#Ejercicio 4

  #Se observa que las  variable HS, GpC debería ser numérica pero está definida como factor
  
  data$HS <- as.character(data$HS)
  data$HS <- data$HS <- gsub(",", ".", data$HS)
  data$HS <- as.numeric(data$HS)
  
  data$GpC <- as.character(data$GpC)
  data$GpC <- data$GpC <- gsub(",", ".", data$GpC)
  data$GpC <- as.numeric(data$GpC)
  
  #Se observa que la variable HR debería ser un factor pero está definida como integer.
  
  data$HR <- as.ordered(data$HR)

#Ejercicio 5
  
  #Comprobamos si en las variables cuantitativas (eso es , desde la columna 4 hasta la ultima hay alguna coma)
  
  comprobacion <- lapply(data[4:ncol(data)], function(x) any(grepl(",", x)))
  comprobacion
  #No hay más separadores decimales erroneos. Los únicos que había ya fueron corregidos al transformar la clase de la variable HR.
  
#Ejercicio 6. Normalizar variables cualitativas
  
  EstandarFOR <- function(x) {
    s <- strsplit(x, " ")[[1]]
    for (i in 1:length(s)) {
      if (s[i] != "and") {
        ss <- strsplit(s[i], "")[[1]]
        if (ss[1] == "(") {
          s[i] <- paste(substring(s[i],1,1), toupper(substring(s[i],2,2)), substring(s[i], 3),
                        sep="", collapse=" ")
        } else {s[i] <- paste(toupper(substring(s[i], 1,1)), substring(s[i], 2), sep="", collapse=" ")}
      } else { next(i) }
    } 
    return(paste(s, sep="", collapse=" "))
  }
  
  data$Country
  data$Country <- trimws(data$Country)
  data$Country <- sapply(data$Country, EstandarFOR)
  data$Country <- as.factor(data$Country)
 
  #data[4:ncol(data)] <- lapply(data[4:ncol(data)], function (x) ((x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))) 
 
# Ejercicio 7
  
   a <- which(data$LCI > data$UCI)
   fromLCItoUCI <- data$LCI[a]
   fromUCItoLCI <- data$UCI[a]
   data$LCI[a] <- fromUCItoLCI
   data$UCI[a] <- fromLCItoUCI
   which(data$LCI > data$UCI)
   
   data$HR <- order(data$HS, decreasing = TRUE)
   
# Ejercicio 8
   
   # Buscar valores atípicos en las variables cuantitativas
   # i. Presentar un boxplot para cada variable cuantitativa.
   # ii. Realizar un cuadro con las estimaciones robustas y no robustas de tendencia central y dispersión
   # de cada variable cuantitativa.
 
   boxplot(data[4:ncol(data)])
   names(data)
   
   
# Ejercicio 9
   
   # library(VIM)
   # which(is.na(data$Family))
   # data$Family <- kNN(data$Family)
   # which(is.na(data$Family))
   # 
   # mydata.completo <- kNN(mydata)
  

   View(data)
  