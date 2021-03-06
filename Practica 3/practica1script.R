
## Practica 1

library(dplyr)
library(VIM)
library(psych)
library(data.table)
getwd()

#1-. Cargar el fichero de datos en R. Antes de cargar el fichero, se debe inspeccionar que tipo de formato csv es para realizar la lectura apropiada.

#Requirements: dplyr, VIM, pysch

#Valores separados por ; por lo que se utiliza read.csv y no read.csv2
mydata <- read.csv("2016_raw.csv")

#2-. Cambiar los nombres de las variables que son muy largos por otros mas cortos (al final del documento se especifica como).
#Creo una funcion que inspecciona un vector y si contiene nombres separados por "." acorta automaticamente siguiendo las instrucciones del enunciado.

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

#Aplico la funcion al vector que contiene el nombre de las variables
long_names <- names(mydata) 
short_names <- short.variables(long_names)
names(mydata) <- short_names
names(mydata)

#3. Indicar el tipo de variable estadistica de cada variable.
#4. En el caso en que R no haya asignado el tipo apropiado a una variable, realizar la conversi�n necesaria para que el tipo final de cada variable sea el adecuado.
#5. Corregir errores de variables cuantitativas con confusi�n de separador decimal.

#Estudio si las variables son categoricas-cualitativas(nominal u ordinal) o cuantitativas (discreta o continua). Por analisis de las variables analizo c�mo deber�an ser y realizo las conversiones pertinentes (ejercicios 3, 4 y 5 realizados conjuntamente)

sapply(mydata, class)

#Estudio de como tienen que ser las variables:

cco <- "Categorica-cualitativa-ordinal"
ccn <- "Categorica-cualitativa-nominal"
cnd <- "Cuantitativa Discreta"
cnc <- "Cuantitativa Continua"
mis_categorias <- c(ccn, ccn, cco, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc, cnc)
mydf <- as.data.frame(setNames(mis_categorias, names(mydata)))
mydf

#Se observa que: 

# Happiness Rank aparece como cuantitativa numerica pero debera ser cualitativa ordinal.
# Happiness Score aparece como categorica pero debera ser numerica decimal.
# GDP per capita aparece como categorica pero debera ser numerica decimal.

#Para convertir HS a numerico es necesario corregir los errores de separadores decimales por lo que el ejercicio 5 se realiza en este punto.

mydata$HS <- as.character(mydata$HS)
mydata$HS <- mydata$HS <- gsub(",", ".", mydata$HS)
mydata$HS <- as.numeric(mydata$HS)

mydata$GpC <- as.character(mydata$GpC)
mydata$GpC <- mydata$GpC <- gsub(",", ".", mydata$GpC)
mydata$GpC <- as.numeric(mydata$GpC)

mydata$HR <- as.ordered(mydata$HR)

#Comprobamos que las variables han sido correctamente convertidas:

str(mydata)

#Se comprueba adem�s en relaci�n al ejercicio 5 que no hay ning�n separador decimal erroneo:
comprobacion <- lapply(mydata[4:ncol(mydata)], function(x) any(grepl(",", x)))
comprobacion


#6. Normalizar/Estandardizar variables cualitativas.

# La funcion EstandarFOR estandariza variables cualitativas seg�n indicado en enunciado. En caso de que haya una palabra entre par�ntesis tambi�n la convertir� a may�sculas.

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

mydata$Country <- trimws(mydata$Country)
mydata$Country <- sapply(mydata$Country, EstandarFOR)
mydata$Country <- as.factor(mydata$Country)
head(mydata$Country)
mydata$Country[87]
mydata$Country[127]

#Corregimos las categorias de la variable Region
mydata$Region <- trimws(mydata$Region)
mydata$Region <- gsub("SUB-SAHARAN  AFRCA", "SUB-SAHARAN AFRICA", mydata$Region)
mydata$Region <- gsub("MIDDLE EAST AND NORTHERN  AFRCA", "MIDDLE EAST AND NORTHERN AFRICA", mydata$Region)
mydata$Region <- as.factor(mydata$Region)
levels(mydata$Region)

#7. Revisar posibles inconsistencias entre variables.
#i. Lower.Confidence.Interval vs Upper.Confidence.Interval
#ii. Happiness.Rank vs Happiness.Score

# Se observa que hay valores en los que LCI es mayor a UCI. Se sustituyen de acuerdo a enunciado.

a <- which(mydata$LCI > mydata$UCI)
fromLCItoUCI <- mydata$LCI[a]
fromUCItoLCI <- mydata$UCI[a]
mydata$LCI[a] <- fromUCItoLCI
mydata$UCI[a] <- fromLCItoUCI
which(mydata$LCI > mydata$UCI)

#Se observan incongruencias con respecto al Rank respecto al Score. Se corrige el Ran de acuerdo al Score.

mydata$HR <- order(mydata$HS, decreasing = TRUE)

#8. Buscar valores at�picos en las variables cuantitativas
#i. Presentar un boxplot para cada variable cuantitativa.
#ii. Realizar un cuadro con las estimaciones robustas y no robustas de tendencia central y dispersi�n
#de cada variable cuantitativa.

#Representaci�n de un BOXplot para cada variable (Agrupadas en variables con similares escalas para facilidad de analisis)

boxplot(mydata[,4:6])
boxplot(mydata[,c(7:9,13)])
boxplot(mydata[,10:12])

#Tabla con estimadores

medidasROBUSTAS <- function(x) {
  myvalues <- c(mean(x, na.rm = TRUE), median(x, na.rm = TRUE), mean(x, na.rm = TRUE, trim=), winsor.mean(x, na.rm = TRUE, trim=), sd(x, na.rm = TRUE),   IQR(x, na.rm = TRUE),  mad(x, na.rm = TRUE))
  return(myvalues)
}

mytable <- round(sapply(mydata[,4:ncol(mydata)],medidasROBUSTAS),3)
medidas <- c("Media", "Mediana", "Media Recortada", "Media winsorizada", "Desviacion estandar", "Rango Intercuartilico (PIC)", "Desviaci�n absoluta DAM")
dimnames(mytable)[[1]] <- medidas
mytable

#9. Valores perdidos.
#i. Buscar qu� variables y registros tienen valores perdidos.
#ii. Imputar los valores a partir de los k-vecinos m�s pr�ximos usando la distancia de Gower con la
#informaci�n de les 6 �ltimas variables.

#Busco NAs por columnas
mycolswNA <-  colnames(mydata)[colSums(is.na(mydata)) > 0]
myrowswNA <-   which(rowSums(is.na(mydata)) > 0)
sprintf("En la columna/columnas - %s - hay Valores perdidos. En las fila/filas - %s - hay valores perdidos", mycolswNA, myrowswNA)

k <- 6
library(VIM)
View(mydata)
mydata.completo <- kNN(mydata[,(ncol(mydata)-k+1):length(mydata)], variable=colnames(mydata)[8])
mydata.completo
# Imputaci�n
mydata[c(3,7),8] <- mydata.completo[c(3,7),1]

#10. Finalmente, realizar un breve estudio descriptivo de los datos una vez depurados y crear el fichero de
#datos corregido.

print("Del estudio de los datos sobre felicidad en diferentes paises se desprenden algunas conclusiones interesentantes: 1. El pa�s con mayores indices de
      felicided es Dinamarca (Hapiness Score de 7.526) mientras el que menos es Burundi (Score de 2.905).") 
summary(mydata)

print("En cuanto a Regiones. Aunque varios paises de Europa Occidental aparecen en Cabeza, los mayores indices se encuentran en Australia y Nueva Zelenda (media de 7.32) y los menores en el Africa Sub-sahariana (media de 4.14)") 
library(dplyr)
continent <- mydata %>% 
  group_by(Region) %>% 
  summarize(AverageHS = mean(HS))
continent <- as.data.frame(continent)
arrange(continent, desc(continent$AverageHS))   

print("La variable que m�s correlaci�n directa guarda con el indice de felicidad es el GDP per Capita, aunque como sabemos, esto no implica que el dinero 
      d� la felicidad (aunque tal vez si ayude a conseguirla")
cor(mydata[, c(4:ncol(mydata))])
plot(mydata$HS, mydata$GpC)

print("Se adjunta el archivo pre-procesado en formato .CSV para un estudio m�s en profundidad")
head(mydata)
#write.csv(mydata, file = "Picatoste_fichero_clean.csv")

save.image("./misdatospractica1.Rdata")
