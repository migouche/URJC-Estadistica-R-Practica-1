# esto no es un codigo completo o funcional,
# solo son lineas independientes para obtener
# informacion de los datos

dataframe <- read.csv("grupo2_datos.csv", header = TRUE, sep = ";")

(dim(dataframe))
(str(dataframe))
(class(dataframe$TDP.Watt))
(summary(dataframe))
(names(dataframe))

plot(dataframe$Days.old, dataframe$precio)

df3 <- data.frame(mhz = dataframe$MHz, precio = dataframe$precio, fabricante = dataframe$Fabricante)

exponential.model <- lm(log(precio) ~ Days.old, data = dataframe)
timevalues = seq(0, 7000, .1)
plot(dataframe$MHz, dataframe$precio, pch = 20, cex = 1.5, col = c("blue", "red", "gray", "green", "yellow")[dataframe$Fabricante], xlab = "MHz", ylab = "precio")
plot(df3$mhz, df3$precio, pch = 20, cex = 1.5, col = df3$fabricante, xlab = "MHz", ylab = "precio")
lines(timevalues, exp(predict(exponential.model, list(Days.old = timevalues))), col = "red", lwd = 2)

dim(dataframe)

apply(dataframe$Days.old, FUN = mean)

library(ggplot2)

p <- sample(c("AMD", "Intel"))

df <- data.frame(x = dataframe$Days.old, y = dataframe$precio)
ggplot(df, aes(x = Days.old, y = precio)) + geom_point()

table(dataframe$Days.old, dataframe$precio)
table(dataframe$Days.old, dataframe$Cores)[,4]
df <- data.frame(dataframe$Days.old, dataframe$precio[1])
mean(df)

barplot(table(dataframe$Days.old, dataframe$Cores), beside = TRUE, col = "green")


tabla<- read.csv("grupo2_datos.csv", header = TRUE, sep = ";")

names(tabla)
preciosXcores = aggregate(tabla$precio, na.omit(list(tabla$Cores)), FUN=mean)
names(preciosXcores)<- c("nCores","avgPrecio")
preciosXcores
tabla$Days.old

table(tabla$Architecture)
frecuenciaXcores = aggregate(tabla$MHz, list(tabla$Cores), FUN=mean)
names(frecuenciaXcores)<- c("nCores","frecuencia")

#barplot(preciosXcores$avgPrecio, preciosXcores$nCores,
barplot(frecuenciaXcores$frecuencia, frecuenciaXcores$nCores,
        col = rainbow(15),
        width = 1,
        space = NULL,
        ylim = c(0,4000),
        #minor.tick(nx= 0, ny = 4, tick.ratio = 0.5),
        border = 'black',
        main = 'Frecuencia por Cores',
        xlab = 'Cores',
        ylab = 'Frecuencia (MHz)')
for(cosa in tabla){
  boxplot(cosa, main = cosa[1])
}
hist(tabla$precio, col = "blue", main = "Histograma de precios", xlab = "Precio", ylab = "Repeticion de datos")
tabla[tabla$Fabricante == "AMD",]
boxplot(tabla[tabla$Fabricante == 'Microsoft',]$Cinebench.R10.32Bit.Single, col = "blue", main = "Boxplot de Benchmark Cinebench", ylab = "Puntuacion")
boxplot(split(tabla$MHz, tabla$Fabricante), col = rainbow(5), main = "Boxplot frecuencia", ylab = "Frecuencia (MHz)", flatten = 0.5)

names(table)
min(na.omit(tabla$precio))

library(corrplot)
library(naturalsort)
cor_tabla = cor(na.omit(tabla[5: 10]))
corrplot::corrplot(cor_tabla, method = "circle", type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)

t <- table(tabla$precio, tabla$MHz)

ggplot(data = tabla, aes(x = MHz, y = TrueCrypt.AES)) + geom_point(aes (color=as.character(Cores)), size = .1)

ggplot(data = tabla[ !is.na(tabla$precio) & !is.na(tabla$Fabricante),] , aes(x = Fabricante, y = precio, color=Fabricante)) + geom_boxplot(lwd = .3, fatten = .3) + stat_boxplot(geom = 'errorbar', lwd = .3, fatten = .3)

boxplot_sin_nas <- function(x, y, data, ...) {
  ggplot(data = data[!is.na(data[[x]]) & !is.na(data[[y]]),], aes_string(x, y, color=x)) +
    geom_boxplot(..., lwd = .3, fatten = .3) +
    stat_boxplot(geom = "errorbar")
}

boxplot_sin_nas("Fabricante", "TrueCrypt.AES", tabla)

scatterplot_sin_nas_con_regresion <- function(x, y, z, data, ...)
{
    ggplot(data = data[!is.na(data[[x]]) & !is.na(data[[y]]),], aes_string(x, y, color=z)) +
        geom_point(size = .1) +
        geom_smooth(method = lm, se = FALSE, ...)
}

scatterplot_sin_nas_sin_regresion <- function(x, y, z, data, ...)
{
    ggplot(data = data[!is.na(data[[x]]) & !is.na(data[[y]]),], aes_string(x, y, color=z)) +
        geom_point(size = .1)
}

tabla$cores2 <- as.factor(tabla$Cores)
scatterplot_sin_nas_sin_regresion_factores <- function (x, y, z, data, ...)
{
    ggplot(data = data[!is.na(data[[x]]) & !is.na(data[[y]]),], aes_string(x, y, color=z)) +
        scale_color_gradient(low = "purple", high = "green") +
        geom_point(size = .1)

}

scatterplot_sin_nas_con_regresion("TrueCrypt.AES", "MHz", "Fabricante", tabla)

scatterplot_sin_nas_sin_regresion_factores("SuperPI.1M.", "MHz", "Cores", tabla)
scatterplot_sin_nas_sin_regresion("Cinebench.R10.32Bit.Single", "MHz", "Fabricante", tabla)

ggplot(data = tabla[tabla$Cinebench.R10.32Bit.Single != "na" & tabla$Fabricante != "na",], aes(x = Days.old)) + geom_histogram(aes(fill = Fabricante), binwidth = 100, color = "black", lwd = .3) + scale_fill_brewer(palette = "Set1")

(na.omit(tabla[tabla$Fabricante == "Microsoft ",]))

tabla2 <- aggregate(tabla$MHz, tabla$precio, list(tabla$cores), as.character())
ggplot(data = tabla, aes(x = MHz, y = precio, color=Cores)) + geom_point(aes(color=Cores), size = .1)

# grafico MHz vs precio
ggplot(data = tabla, aes(x = MHz, y = precio)) + geom_point(aes(color=Fabricante), size = .1) + geom_smooth(method = "lm", aes(color=Fabricante), se = FALSE)
mean(na.omit(tabla$L2.Cache.MB.))
names(tabla)
min(na.omit(tabla$L2.Cache.MB.))





(cov(na.omit(tabla$precio),na.omit(tabla$MHz)))


for(var in names(tabla)){
  if(class(tabla[[var]]) == "integer" | class(tabla[[var]]) == "numeric")
  {
    print(paste(var, " min:", min(na.omit(tabla[[var]]))))
    print(paste(var, "max:", max(na.omit(tabla[[var]]))))
    print(paste(var, " media:", mean(na.omit(tabla[[var]]))))
    print(paste(var, " mediana:", median(na.omit(tabla[[var]]))))
    qvar <- var(na.omit(tabla[[var]]))
    var2 <- qvar * length(na.omit(tabla[[var]])) / (length(na.omit(tabla[[var]])) - 1)
    print(paste(var, "varianza:", var2))
    dt <- sqrt(var2)
    print(paste(var, "desviacion tipica: ", dt))
    coef_var <- dt / mean(na.omit(tabla[[var]])) * 100
    print(paste(var, "coeficiente de variacion:", coef_var))
  #(mean(na.omit(tabla[[var]])))
  #(min(na.omit(tabla[,var])))
  #(qvar <- var(na.omit(tabla[,var])))
  #(var <- qvar * length(na.omit(tabla[,var])) / (length(na.omit(tabla[,var])) - 1))
  #(dt <- sqrt(var))
  #(coef_var <- dt / mean(na.omit(tabla[,var])) * 100)
  }
}

çmin(na.omit(tabla$TDP.Watt))
mean(na.omit(tabla$TDP.Watt))
(qvar <- var(na.omit(tabla$TDP.Watt)))
(var <-qvar * length(na.omit(tabla$TDP.Watt)) / (length(na.omit(tabla$TDP.Watt)) - 1))
(dt <- sqrt(var))


(coef_var <- dt / mean(na.omit(tabla$precio)) * 100)

#lineas multiples
info <- melt(df, id.vars = "x")
ggplot(data = tabla, aes(x = MHz)) +

  tb2 = tabla[complete.cases(tabla[,1:16]),]

corrplot::corrplot(cor(tabla[6: 9,]), method = "circle", type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)