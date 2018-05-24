source('practica1script.R')

library(caret)
library(rmarkdown)

#Crear un modelo de RLM para predecir la puntuación de la felicidad (HS) a partir de:
# - Indicador de renta per cápita (GpC)
# - Esperanza de vida en salud (LE)
# - Corrupción (GC)
head(mydata)
HS_PredictorModel_3p <- lm(HS ~ GpC + LE + GC, mydata)
summary(HS_PredictorModel_3p)
varImp(HS_PredictorModel_3p, scale = FALSE)

layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Spend x Residuals Plot
plot(HS_PredictorModel_3p$resid~mydata$HS[order(mydata$HS)],
            main="HS x Residuals\nfor Multiple Regression with 3 predictors",
            xlab="HS", ylab= "Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(HS_PredictorModel_3p$resid, main = "Histogram of Residuals", ylab = "Residuals")
#Q-Q Plot
qqnorm(HS_PredictorModel_3p$resid)
qqline(HS_PredictorModel_3p$resid)


levels(mydata_ej2$Region) <- relevel(mydata_ej2$Region, ref = "WESTERN EUROPE")
HS_PredictorModel_4p <- lm(HS ~ Region + GpC + LE + GC, mydata)
summary(HS_PredictorModel_4p)
varImp(HS_PredictorModel_4p, scale = FALSE)

layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Spend x Residuals Plot
plot(HS_PredictorModel_4p$resid~mydata$HS[order(mydata$HS)],
     main="HS x Residuals\nfor Multiple Regression with",
     xlab="HS", ylab= "Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(HS_PredictorModel_4p$resid, main = "Histogram of Residuals", ylab = "Residuals")
#Q-Q Plot
qqnorm(HS_PredictorModel_4p$resid)
qqline(HS_PredictorModel_4p$resid)

GpC_example <- 1.5
LE_example <- 0.69
GC_example <- 0.35

newcountry <- list("Country" = as.factor("WonderLand") , "Region" = as.factor("WESTERN EUROPE"), "HR" = 0, "HS" = 0, "LCI" = 0, "UCI" = 0, "GpC" = GpC_example, "Family" = 0, "LE" = LE_example, "Freedom" = 0, "GC" = GC_example, "Generosity" = 0, "DR" = 0)
mydata_bis <- mydata

mydata_bis <- rbind(mydata_bis, newcountry)

mysolution_3p <- predict(HS_PredictorModel_3p, mydata_bis[158,])
mysolution_3pb <- predict(HS_PredictorModel_3p,list("GpC" = GpC_example, "LE" = LE_example, "GC" = GC_example))

mysolution_4p <- predict(HS_PredictorModel_4p, mydata_bis[158,])

mysolution_3p
mysolution_4p

#Ejercici 2

#Ejercicio 2.1

library(caret)
dataLG <- mydata
dataLG$Grupo <- factor(ifelse(dataLG$HR <= 32, "best", "worse"), levels = c("worse","best"))
set.seed(1000)
traindataset <- createDataPartition(dataLG$HR, p=0.75, list = FALSE)
logitmodel <- glm(Grupo ~ GpC + GC, data = dataLG[traindataset,], family = binomial)
summary(logitmodel)

dataLG[-traindataset, "Probabilidad_Acierto"] <- predict(logitmodel, newdata = dataLG[-traindataset, ], type = "response" )
dataLG[-traindataset, "Acierto"] <- ifelse(dataLG[-traindataset, "Probabilidad_Acierto"] >= 0.5,1,0)
table(dataLG[-traindataset, "Grupo"], dataLG[-traindataset, "Acierto"], dnn=c("Real", "Predicho"))

#Ejercicio 2.2

GpC <- 1.5
GC <- 0.35
solution <- predict (logit, newdata = list("GpC" = GpC, "GC" = GC), type = 'response')
round(solution*100,2)

logitmodel_freedom <- glm(Grupo ~ GpC + GC + Freedom, data = dataLG[traindataset,], family = binomial)
summary(logitmodel_freedom)

logitmodel_Region <- glm(Grupo ~ GpC + GC + Region, data = dataLG[traindataset,], family = binomial)
summary(logitmodel_Region)

logitmodel_freeReg <- glm(Grupo ~ GpC + GC + Freedom + Region, data = dataLG[traindataset,], family = binomial)
summary(logitmodel_freeReg)

dataLG[-traindataset, "Probabilidad_Acierto"] <- predict(logitmodel_freeReg, newdata = dataLG[-traindataset, ], type = "response" )
dataLG[-traindataset, "Acierto"] <- ifelse(dataLG[-traindataset, "Probabilidad_Acierto"] >= 0.5,1,0)
table(dataLG[-traindataset, "Grupo"], dataLG[-traindataset, "Acierto"], dnn=c("Real", "Predicho"))

#Ejercicio 2.3

#Ejercicio 2.4
logitmodel_freedom <- glm(Grupo ~ GpC + GC + Freedom, data = dataLG[traindataset,], family = binomial)
logitmodel_Region <- glm(Grupo ~ GpC + GC + Region, data = dataLG[traindataset,], family = binomial)
logitmodel_freeReg <- glm(Grupo ~ GpC + GC + Freedom + Region, data = dataLG[traindataset,], family = binomial)

#Ejercicio 2.5

logitmodel_freeReg_ej5 <- glm(Grupo ~ GpC + GC + Freedom + Region, data = dataLG, family = binomial)
dataex5 <- cbind(mydata, "Probabilidad" = logitmodel_freeReg_ej5$fitted.values)
dataex5filtered <- dataex5 %>% filter(Probabilidad > 0.80 & HR > 32)
par(mfrow=c(1,1))
plot(HR~Probabilidad, xlim = c(0.80, 1), 
     xlab = 'Probabilidad de pertenecer a la clase best', 
     ylab = 'Ranking de Felicidad (HR)',
     main = 'HR vs Probabilidad de percetener a clase "best" ',
     data = dataex5filtered)
with(dataex5filtered, text(HR~Probabilidad, labels = dataex5filtered$Country, pos = 2, cex = 0.7))

#Ejercicio 2.6
library(pROC)
str(dataex2)
testdf <- dataLG[-traindataset,]
roc_obj <- roc(testdf$Grupo, testdf$Acierto)
auc(roc_obj)
plot.roc(roc_obj)





