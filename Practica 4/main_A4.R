library("rmarkdown")

df <- read.csv(file = './Fumadores.csv')

levels(df$Tipo) <- list(NF = "NF", FP = "FP", NI = c("NI", "ni"), FL = c("FL", "fL"), FM = "FM", FI = "FI")

resumen <- df %>% group_by(Tipo) %>% summarize(Media.Por.Tipo = mean(AE),
                                               Numero.Personas = length(AE))
print(resumen)
g <- ggplot(resumen, aes(x = reorder(Tipo, -Media.Por.Tipo), y = Media.Por.Tipo)) + geom_col()
print(g) 

misboxplots <- ggplot(df, aes(x = Tipo, y = AE)) + geom_boxplot()
print(misboxplots)

# Ejercicio 2

