
#Ejercicio 1 

  #Valores separados por ; por lo que se utiliza read.csv y no read.csv2
  data <- read.csv("2016_raw.csv")

#Ejercicio 2
  
  short.variables(ejemplo1)

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
  
  data$Region <- trimws(data$Region)
  data$Region <- gsub("SUB-SAHARAN  AFRCA", "SUB-SAHARAN AFRICA", data$Region)
  data$Region <- gsub("MIDDLE EAST AND NORTHERN  AFRCA", "MIDDLE EAST AND NORTHERN AFRICA", data$Region)
  data$Region <- as.factor(data$Region)
  levels(data$Region)
  
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
   
   library(psych)
 
   #Representación de un BOXplot para cada variable (Agrupadas en variables con similares escalas para facilidad de analisis)
   
   boxplot(data[,4:6])
   boxplot(data[,c(7:9,13)])
   boxplot(data[,10:12])

  
   medidasROBUSTAS <- function(x) {
     myvalues <- c(mean(x, na.rm = TRUE), median(x, na.rm = TRUE), mean(x, na.rm = TRUE, trim=), winsor.mean(x, na.rm = TRUE, trim=), sd(x, na.rm = TRUE), IQR(x, na.rm = TRUE),  mad(x, na.rm = TRUE))
     return(myvalues)
    }

    mytable <- round(sapply(data[,4:ncol(data)],medidasROBUSTAS),3)
    medidas <- c("Mean", "Median", "Media Recortada", "Media winsorizada", "Desviacion estandar", "Rango Intercuartilico (PIC)", "Desviación absoluta DAM")
    dimnames(mytable)[[1]] <- medidas
    mytable

# Ejercicio 9
    
    #Busco NAs por columnas
    
   mycolswNA <-  colnames(data)[colSums(is.na(data)) > 0]
   myrowswNA <-   which(rowSums(is.na(data)) > 0)
   
   sprintf("En la columna/columnas - %s - hay Valores perdidos. En las fila/filas - %s - hay valores perdidos", mycolswNA, myrowswNA)
   
   datafillinNA <- data[,7:ncol(data)]
   str(datafillinNA)
   
   #Rellenar NAs en Familiy con las 6 últimas variables (GpC, Family, LE, Freedom, GC, GEnerosiity)
   library(VIM)
   mydata.completo <- kNN(datafillinNA, datafillinNA[,2], metric = NULL, k = 6)
  
   library(caret)

   
   View(data)
  
   
   
# Ejercicio 10
   
   
   write.csv(data, file = "2016_preprocessed.csv")
   
   library(dplyr)
   continent <- data %>% 
     group_by(Region) %>% 
     summarize(AverageHS = mean(HS))
  
   continent <- as.data.frame(continent)
  str(continent)

continent$Ordenados <- order(continent$AverageHS, decreasing = TRUE)
View(continent)

arrange(continent, desc(continent$AverageHS))   
   