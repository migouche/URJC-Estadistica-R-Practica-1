
#  2. LEER EL FICHERO DE DATOS
DF <- read.csv("grupo2_datos.csv",
               header = TRUE, sep = ";")

#  3. VERIFICAR LOS TIPOS DE DATOS DE LAS VARIABLES
str(DF)
class(DF$var1)
sapply(DF, class)

#  4. EXAMINAR RAPIDAMENTE LOS VALORES DE LAS VARIABLES
head(DF)
# 4.1 conteo de NA
colSums(is.na(DF))

#  5. ANALISIS DESCRIPTIVO UNIVARIANTE
summary(DF$var1)
summary(DF$var2)

# 5.1 Calculos de descriptivas por grupos
MediasByTipo <- aggregate(var1 ~ tipo, DF, FUN=mean)
MedianasByTipo <- aggregate(var1 ~ tipo, DF, FUN=median)

# histogramas
hist(DF$var1)

# boxplot por un factor
boxplot(var1 ~ tipo, data = DF)
boxplot(var1 ~ clase, data = DF)
# boxplot por 2 factores
boxplot(var1 ~ tipo*clase, data=DF,notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Var1 por grupos", xlab="clase y tipo")

#  6. ANALISIS BIVARIANTE: CATEGORICAS
# Si hay mas de una variable categorica hacer tablas
table(DF$tipo)
table(DF$clase)
table(DF$tipo,DF$clase)
barplot(table(DF$tipo,DF$clase),beside = TRUE,
        col = terrain.colors(2), legend = TRUE,
        ylim = c(0,50) )

#  7. ANALISIS BIVARIANTE: Variables continuas
DF$tipo <- as.factor(DF$tipo)
plot(DF$var1,DF$var2, col=DF$tipo)
legend("topright",legend=c(unique(DF$tipo)),col=1:length(DF$tipo),pch=1)

DF$clase <- as.factor(DF$clase)
plot(DF$var1,DF$var2,col=DF$clase)
legend("topright",legend=c(unique(DF$clase)),col=1:length(DF$clase),pch=1)

#  8. ANALISIS CORRELACIONES MULTIPLES
# matriz de correlaciones
cor(DF)  # error porque la ultima columna no es numerica
names(DF)
Mcor <- cor(DF[,1:4])
Mcor <- cor(na.omit(DF[,1:4])) #  si hay datos faltantes

#  8.1 Grafico Correlaciones
library(corrplot)
corrplot(Mcor, method = 'number', col = 'black', cl.pos = 'n')
corrplot(Mcor)
corrplot(Mcor, order = 'AOE')

#  8.2 parallel plot
library(ggplot2)
library(GGally)
ggparcoord(DF,
           columns = 1:4, groupColumn = 5
)
ggparcoord(DF,
           columns = 1:4, groupColumn = 6
)

#  8.3 parallel plot reescalando variables
library(viridis)
DF2 <- DF[complete.cases(DF), ]
ggparcoord(DF2,
           columns = 1:4, groupColumn = 5, order = "anyClass",
           scale="uniminmax",
           showPoints = TRUE,
           title = "Standardize to Min = 0 and Max = 1",
           alphaLines = 0.3
) +
  scale_color_viridis(discrete=TRUE) +
  theme(
    legend.position="right",
    plot.title = element_text(size=13)
  ) +
  xlab("")


#  9. REGRESION
model1 <- lm(var4 ~ var1 ,data=DF2)
summary(model1)

model2 <- lm(var2 ~ var1 ,data=DF2)
summary(model2)

model3 <- lm(var2 ~ var1+var4 ,data=DF2)
summary(model3)

# Visualizamos alguno de los modelos
par(mfrow=c(1,2))
plot(DF2$var1,DF2$var4,pch=19,main="Recta de Regresion 1",cex.main=0.95,ylim=c(3,15))
abline(reg=model1,lwd=1.5)
plot(DF2$var1, model1$residuals,pch=19,main="residuos");
abline(h=0)

#  9.  PREDICCION para algun valor de var1 con el modelo 1
predict(model1) # predice var 4 para todos los valores de var 1 los
new.dat <- data.frame(var1=c(7,17))
predict(model1,newdata =new.dat)

#  9.1 Grafico enfrentando valores Reales y Predichos
Real_var4 <- DF2[!is.na(DF2$var1),c("var4")] #extrae valores reales si hay faltantes
par(mfrow=c(1,1))
plot(x=Real_var4, y= predict(model1),
     xlab='Real Values',
     ylab='Predicted Values',
     main='Valores Reales vs. Predichos')
abline(a=0, b=1,col="red")


