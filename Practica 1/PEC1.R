
#Ejercicio 1 

  #Requirements: dplyr, VIM, pysch

  #Valores separados por ; por lo que se utiliza read.csv y no read.csv2
  mydata <- read.csv("2016_raw.csv")

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
  
  long_names <- names(mydata)
  short_names <- short.variables(long_names)
  names(mydata) <- short_names
  names(mydata)

#Ejercicio 3

  sapply(mymydata, class)
  cco <- "Categorica-cualitativa-ordinal"
  ccn <- "Categorica-cualitativa-nominal"
  cnd <- "Cuantitativa Discreta"
  cnc <- "Cuantitativa Continua"
  mis_categorias <- c(ccn, ccn, cco, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc)
  mydf <- as.data.frame(setNames(mis_categorias, names(mydata)))
  mydf

#Ejercicio 4

  #Se observa que las  variable HS, GpC debería ser numérica pero está definida como factor
  
  mydata$HS <- as.character(mydata$HS)
  mydata$HS <- mydata$HS <- gsub(",", ".", mydata$HS)
  mydata$HS <- as.numeric(mydata$HS)
  
  mydata$GpC <- as.character(mydata$GpC)
  mydata$GpC <- mydata$GpC <- gsub(",", ".", mydata$GpC)
  mydata$GpC <- as.numeric(mydata$GpC)
  
  #Se observa que la variable HR debería ser un factor pero está definida como integer.
  
  mydata$HR <- as.ordered(mydata$HR)

#Ejercicio 5
  
  #Comprobamos si en las variables cuantitativas (eso es , desde la columna 4 hasta la ultima hay alguna coma)
  
  comprobacion <- lapply(mydata[4:ncol(mydata)], function(x) any(grepl(",", x)))
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
  
  
  mydata$Country
  mydata$Country <- trimws(mydata$Country)
  mydata$Country <- sapply(mydata$Country, EstandarFOR)
  mydata$Country <- as.factor(mydata$Country)
  
  mydata$Region <- trimws(mydata$Region)
  mydata$Region <- gsub("SUB-SAHARAN  AFRCA", "SUB-SAHARAN AFRICA", mydata$Region)
  mydata$Region <- gsub("MIDDLE EAST AND NORTHERN  AFRCA", "MIDDLE EAST AND NORTHERN AFRICA", mydata$Region)
  mydata$Region <- as.factor(mydata$Region)
  levels(mydata$Region)
  
#mydata[4:ncol(mydata)] <- lapply(mydata[4:ncol(mydata)], function (x) ((x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))) 
 
# Ejercicio 7
  
   a <- which(mydata$LCI > mydata$UCI)
   fromLCItoUCI <- mydata$LCI[a]
   fromUCItoLCI <- mydata$UCI[a]
   mydata$LCI[a] <- fromUCItoLCI
   mydata$UCI[a] <- fromLCItoUCI
   which(mydata$LCI > mydata$UCI)
   
   mydata$HR <- order(mydata$HS, decreasing = TRUE)
   
# Ejercicio 8
   
   library(psych)
 
   #Representación de un BOXplot para cada variable (Agrupadas en variables con similares escalas para facilidad de analisis)
   
   boxplot(mydata[,4:6])
   boxplot(mydata[,c(7:9,13)])
   boxplot(mydata[,10:12])

   medidasROBUSTAS <- function(x) {
     myvalues <- c(mean(x, na.rm = TRUE), median(x, na.rm = TRUE), mean(x, na.rm = TRUE, trim=), winsor.mean(x, na.rm = TRUE, trim=), sd(x, na.rm = TRUE), IQR(x, na.rm = TRUE),  mad(x, na.rm = TRUE))
     return(myvalues)
    }

    mytable <- round(sapply(mydata[,4:ncol(mydata)],medidasROBUSTAS),3)
    medidas <- c("Mean", "Median", "Media Recortada", "Media winsorizada", "Desviacion estandar", "Rango Intercuartilico (PIC)", "Desviación absoluta DAM")
    dimnames(mytable)[[1]] <- medidas
    mytable

# Ejercicio 9
    
    #Busco NAs por columnas
    
   mycolswNA <-  colnames(mydata)[colSums(is.na(mydata)) > 0]
   myrowswNA <-   which(rowSums(is.na(mydata)) > 0)
   
   sprintf("En la columna/columnas - %s - hay Valores perdidos. En las fila/filas - %s - hay valores perdidos", mycolswNA, myrowswNA)

   #Rellenar NAs en Familiy con las 6 últimas variables (GpC, Family, LE, Freedom, GC, GEnerosiity)
   library(VIM)
   mymydata.completo <- kNN(mydatafillinNA, mydatafillinNA[,2], metric = NULL, k = 6)
  
   #Al no dar ningun resultado se prueba a imputar los valores perdidos con un modelo lineal simple:
   lineal.model <- lm(Family ~ GpC + LE + Freedom + GC + Generosity + DR, mydata = mydata)
   I <- is.na(mydata$Family)
   mydata$Family[I] <- predict(lineal.model, newmydata = mydata[I, ])
   mydata$Family[1:10]

   #Se aprecia como los valores de las posiciones 3 y 7 han sido imputados por 1.107697 y 1,070216 respectivamente.
   
# Ejercicio 10
   
   sprintf("Del estudio de los datos sobre felicidad en diferentes paises se desprenden algunas conclusiones interesentantes: 1. El país con mayores indices de
           felicided es Dinamarca (Hapiness Score de 7.526) mientras el que menos es Burundi (Score de 2.905).") 
   
   summary(mydata)
   
   sprintf("En cuanto a Regiones. Aunque varios paises de Europa Occidental aparecen en Cabeza, los mayores indices se encuentran en Australia y Nueva Zelenda (media de 7.32) y
           los menores en el Africa Sub-sahariana (media de 4.14)") 
   
   library(dplyr)
   continent <- mydata %>% 
     group_by(Region) %>% 
     summarize(AverageHS = mean(HS))
     continent <- as.data.frame(continent)
   arrange(continent, desc(continent$AverageHS))   
   
   sprintf("La variable que más correlación directa guarda con el indice de felicidad es el GDP per Capita, aunque como sabemos, esto no implica que el dinero 
           dé la felicidad (aunque tal vez si ayude a conseguirla")
   cor(mydata[, c(4:ncol(mydata))])
   plot(mydata$HS, mydata$GpC)

   sprintf("Se adjunta el archivo pre-procesado en formato .CSV para un estudio más en profundidad")
   
   head(mydata)
   write.csv(mydata, file = "Picatoste_fichero_clean.csv")
   
   
            