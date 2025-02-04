#librerias
library(ggplot2)
library(ggpubr)
library(mixtools)
library(psych)
library(gridExtra)
library(DT)
library(DescTools)
library(boot)
library(fitdistrplus)
library(lmtest)
library(sandwich)
library(car)
library(MASS)
library(dplyr)

View(swiss)
help(swiss)
describe(swiss)
swiss

#############intervalos de confianza y pruebas de hipotesis#####################
intervFertmean <- t.test(swiss$Fertility)
intervFertvar <- VarCI(swiss$Fertility)
hip1_meanFert <- t.test(swiss$Fertility, alternative = "greater", mu = 66.5)
hip2_meanFert <- t.test(swiss$Fertility, alternative = "less", mu = 74)

intervAgrmean <- t.test(swiss$Agriculture)
intervAgrvar <- VarCI(swiss$Agriculture)
hip1_meanAgr <- t.test(swiss$Agriculture, alternative = "greater", mu = 44)
hip2_meanAgr <- t.test(swiss$Agriculture, alternative = "less", mu = 57.3)

intervExmean <- t.test(swiss$Examination)
intervExvar <- VarCI(swiss$Examination)
hip1_meanEx <- t.test(swiss$Examination, alternative = "greater", mu = 14)
hip2_meanEx <- t.test(swiss$Examination, alternative = "less", mu = 18.8)

intervIMmean <- t.test(swiss$Infant.Mortality)
intervIMvar <- VarCI(swiss$Infant.Mortality)
hip1_meanIM <- t.test(swiss$Infant.Mortality, alternative = "greater", mu = 19)
hip2_meanIM <- t.test(swiss$Infant.Mortality, alternative = "less", mu = 20.8)

mean_function <- function(data, indices) {return (mean(data[indices]))}
var_function <- function(data, indices) {return(var(data[indices]))}

boot_result_meanEd <- boot(data = swiss$Education, statistic = mean_function, R = 1000)
bca_interval_meanEd <- boot.ci(boot_result_meanEd, type = "bca")
boot_result_varEd <- boot(data = swiss$Education, statistic = var_function, R = 1000)
bca_interval_varEd <- boot.ci(boot_result_varEd, type = "bca")

boot_result_meanCat <- boot(data = swiss$Catholic, statistic = mean_function, R = 1000)
bca_interval_meanCat <- boot.ci(boot_result_meanCat, type = "bca")
boot_result_varCat <- boot(data = swiss$Catholic, statistic = var_function, R = 1000)
bca_interval_varCat <- boot.ci(boot_result_varCat, type = "bca")
bca_interval_varCat
################################################################################

########################analizando normalidad de las variables##################
#usando shapiro
shapiro_test <- lapply(swiss, function(columna) {
  shapiro.test(columna)$p.value
})
for (elemento in shapiro_test) {
  if(elemento < 0.05) 
    print("no es normal")
  else
    print("es normal")
}
shapiro_test
# Prueba de Kolmogorov-Smirnov
ks_test <- lapply(swiss, function(columna) {
  ks.test(columna, "pnorm", mean = mean(columna), sd = sd(columna))$p.value
})
for (elemento in ks_test) {
  if(elemento < 0.05) 
    print("no es normal")
  else
    print("es normal")
}

################################################################################

#############################Fertility##########################################
swiss$Fertility
#Medidas de Tendencia Central
fertility_mean <- mean(swiss$Fertility) #media
densidad <- density(swiss$Fertility)
fertility_freq <- densidad$x[which.max(densidad$y)] #moda
fertility_median <- median(swiss$Fertility) #mediana
fertility_MTC <- data.frame(Media = fertility_mean, Moda = fertility_freq, Mediana = fertility_median)
fertility_MTC
#Medidas de Dispersion
fertility_var <- var(swiss$Fertility) #varianza
fertility_sd <- sd(swiss$Fertility) #desviacion estandar
fertility_min <- min(swiss$Fertility) #valor minimo
fertility_max <- max(swiss$Fertility) #valor maximo
fertility_range <- fertility_max - fertility_min #rango
fertility_cv <- (fertility_sd / fertility_mean) * 100 #coeficiente de variacion heterogeneo
fertility_MD <- data.frame(
  Varianza = fertility_var, Desviación_Estandar = fertility_sd, Mínimo = fertility_min, 
  Máximo = fertility_max, Rango = fertility_range, Coeficiente_de_Variación = fertility_cv)
fertility_MD
#Medidas de Posicion
fertility_quantile <- quantile(swiss$Fertility, c(0.25, 0.50, 0.75))
fertility_quantile
#plots
#histograma
hist(swiss$Fertility, breaks = 20, probability = TRUE, col = "lightblue", 
     main = "Histograma de Fertilidad con Densidad Normal", 
     xlab = "Valores de Fertilidad", ylab = "Frecuencia")
# Crear una secuencia de valores para la función normal
x <- seq(fertility_min - 3*fertility_sd, fertility_max + 3*fertility_sd, length = 100)
# Calcular la densidad normal
y <- dnorm(x, mean = fertility_mean, sd = fertility_sd)
# Añadir la función de densidad normal al histograma
lines(x, y, col = "red", lwd = 2)  # Agregar la línea de la densidad normal
lines(density(swiss$Fertility), col = "green", lwd = 2)
#caja y bigotes
boxplot(swiss$Fertility, main="Gráfico de Caja y Bigotes", xlab="Fertilidad", ylab="Valores", col="lightblue", border="black")
#verificar normalidad
qqnorm(swiss$Fertility, xlab = "Distribución Normal", ylab = "Distribución de Fertility")
qqline(swiss$Fertility, col = "red")  # Línea de referencia
################################################################################

##############################Agriculture#######################################

swiss$Agriculture
help(mean)
#Medidas de Tendencia Central
agriculture_mean <- mean(swiss$Agriculture) #media
densidad <- density(swiss$Agriculture)
agriculture_freq <- densidad$x[which.max(densidad$y)] #moda
agriculture_median <- median(swiss$Agriculture) #mediana
agriculture_MTC <- data.frame(Media = agriculture_mean, Moda = agriculture_freq, Mediana = agriculture_median)
agriculture_MTC
#Medidas de Dispersion
agriculture_var <- var(swiss$Agriculture) #varianza
agriculture_sd <- sd(swiss$Agriculture) #desviacion estandar
agriculture_min <- min(swiss$Agriculture) #valor minimo
agriculture_max <- max(swiss$Agriculture) #valor maximo
agriculture_range <- agriculture_max - agriculture_min #rango
agriculture_cv <- (agriculture_sd / agriculture_mean) * 100 #coeficiente de variacion heterogeneo
agriculture_MD <- data.frame(
  Varianza = agriculture_var, Desviación_Estandar = agriculture_sd, Mínimo = agriculture_min, 
  Máximo = agriculture_max, Rango = agriculture_range, Coeficiente_de_Variación = agriculture_cv)
agriculture_MD
#Medidas de Posicion
agriculture_quantile <- quantile(swiss$Agriculture, c(0.25, 0.50, 0.75))
agriculture_quantile
#plots
#histograma
hist(swiss$Agriculture, breaks = 10, probability = TRUE, col = "lightpink", 
     main = "Histograma de Agriculture con Densidad Normal", 
     xlab = "Valores de Agriculture", ylab = "Frecuencia")
# Crear una secuencia de valores para la función normal
x <- seq(agriculture_min - 3*agriculture_sd, agriculture_max + 3*agriculture_sd, length = 100)
# Calcular la densidad normal
y <- dnorm(x, mean = agriculture_mean, sd = agriculture_sd)
# Añadir la función de densidad normal al histograma
lines(x, y, col = "red", lwd = 2)  # Agregar la línea de la densidad normal
lines(density(swiss$Agriculture), col = "green", lwd = 2)
#caja y bigotes
boxplot(swiss$Agriculture, main="Gráfico de Caja y Bigotes", xlab="Agriculture", ylab="Valores", col="lightpink", border="black")
#verificar normalidad
qqnorm(swiss$Agriculture, xlab = "Distribución Normal", ylab = "Agriculture")
qqline(swiss$Agriculture, col = "red")  # Línea de referencia
################################################################################ 

##############################Examination#######################################

swiss$Examination
#Medidas de Tendencia Central
examination_mean <- mean(swiss$Examination) #media
examination_freq <- as.numeric(names(which.max(table(swiss$Examination)))) #moda
examination_median <- median(swiss$Examination) #mediana
examination_MTC <- data.frame(Media = examination_mean, Moda = examination_freq, Mediana = examination_median)
examination_MTC
#Medidas de Dispersion
examination_var <- var(swiss$Examination) #varianza
examination_sd <- sd(swiss$Examination) #desviacion estandar
examination_min <- min(swiss$Examination) #valor minimo
examination_max <- max(swiss$Examination) #valor maximo
examination_range <- examination_max - examination_min #rango
examination_cv <- (examination_sd / examination_mean) * 100 #coeficiente de variacion heterogeneo
examination_MD <- data.frame(
  Varianza = examination_var, Desviación_Estandar = examination_sd, Mínimo = examination_min, 
  Máximo = examination_max, Rango = examination_range, Coeficiente_de_Variación = examination_cv)
examination_MD
#Medidas de Posicion
examination_quantile <- quantile(swiss$Examination, c(0.25, 0.50, 0.75))
examination_quantile
#plots
#histograma
hist(swiss$Examination, breaks = 10, probability = TRUE, col = "lightyellow", 
     main = "Histograma de Examination con Densidad Normal", 
     xlab = "Valores de Examination", ylab = "Frecuencia")
# Crear una secuencia de valores para la función normal
x <- seq(examination_min - 3*examination_sd, examination_max + 3*examination_sd, length = 50)
# Calcular la densidad normal
y <- dnorm(x, mean = examination_mean, sd = examination_sd)
# Añadir la función de densidad normal al histograma
lines(x, y, col = "red", lwd = 2)  # Agregar la línea de la densidad normal
lines(density(swiss$Examination), col = "green", lwd = 2)
#caja y bigotes
boxplot(swiss$Examination, main="Gráfico de Caja y Bigotes", xlab="Examination", ylab="Valores", col="lightyellow", border="black")
#verificar normalidad
qqnorm(swiss$Examination, xlab = "Distribución Normal", ylab = "Examination")
qqline(swiss$Examination, col = "red")  # Línea de referencia
################################################################################

#################################Education######################################

swiss$Education
#Medidas de Tendencia Central
education_mean <- mean(swiss$Education) #media
education_freq <- as.numeric(names(which.max(table(swiss$Education)))) #moda
education_median <- median(swiss$Education) #mediana
education_MTC <- data.frame(Media = education_mean, Moda = education_freq, Mediana = education_median)
education_MTC
#Medidas de Dispersion
education_var <- var(swiss$Education) #varianza
education_sd <- sd(swiss$Education) #desviacion estandar
education_min <- min(swiss$Education) #valor minimo
education_max <- max(swiss$Education) #valor maximo
education_range <- education_max - education_min #rango
education_cv <- (education_sd / education_mean) * 100 #coeficiente de variacion heterogeneo
education_MD <- data.frame(
  Varianza = education_var, Desviación_Estandar = education_sd, Mínimo = education_min, 
  Máximo = education_max, Rango = education_range, Coeficiente_de_Variación = education_cv)
education_MD
#Medidas de Posicion
education_quantile <- quantile(swiss$Education, c(0.25, 0.50, 0.75))
education_quantile
#plots
#histograma
hist(swiss$Education, breaks = 10, probability = TRUE, col = "purple", 
     main = "Histograma de Education con Densidad Normal", 
     xlab = "Valores de Education", ylab = "Frecuencia")
# Crear una secuencia de valores para la función normal
x <- seq(education_min - 3*education_sd, education_max + 3*education_sd, length = 50)
# Calcular la densidad normal
y <- dnorm(x, mean = education_mean, sd = education_sd)
# Añadir la función de densidad normal al histograma
lines(x, y, col = "red", lwd = 2)  # Agregar la línea de la densidad normal
lines(density(swiss$Education), col = "green", lwd = 2)
#caja y bigotes
boxplot(swiss$Education, main="Gráfico de Caja y Bigotes", xlab="Education", ylab="Valores", col="purple", border="black")
#verificar normalidad
qqnorm(swiss$Education, xlab = "Distribución Normal", ylab = "Education")
qqline(swiss$Education, col = "red")  # Línea de referencia
################################################################################

###############################Catholic#########################################

swiss$Catholic
#Medidas de Tendencia Central
catholic_mean <- mean(swiss$Catholic) #media
densidad <- density(swiss$Catholic)
catholic_freq <- densidad$x[which.max(densidad$y)] #moda
catholic_median <- median(swiss$Catholic) #mediana
catholic_MTC <- data.frame(Media = catholic_mean, Moda = catholic_freq, Mediana = catholic_median)
catholic_MTC
#Medidas de Dispersion
catholic_var <- var(swiss$Catholic) #varianza
catholic_sd <- sd(swiss$Catholic) #desviacion estandar
catholic_min <- min(swiss$Catholic) #valor minimo
catholic_max <- max(swiss$Catholic) #valor maximo
catholic_range <- catholic_max - catholic_min #rango
catholic_cv <- (catholic_sd / catholic_mean) * 100 #coeficiente de variacion heterogeneo
catholic_MD <- data.frame(
  Varianza = catholic_var, Desviación_Estandar = catholic_sd, Mínimo = catholic_min, 
  Máximo = catholic_max, Rango = catholic_range, Coeficiente_de_Variación = catholic_cv)
catholic_MD
#Medidas de Posicion
catholic_quantile <- quantile(swiss$Catholic, c(0.25, 0.50, 0.75))
catholic_quantile
#plots
#histograma
# Normalizar los datos al rango [0, 1]
catholic_norm <- (swiss$Catholic - catholic_min) / (catholic_max - catholic_min)
# Estimar los parámetros de la distribución beta bimodal
fit <- normalmixEM(catholic_norm, k = 2)
alpha1 <- fit$mu[1] * (fit$mu[1] * (1 - fit$mu[1]) / fit$sigma[1]^2 - 1)
beta1 <- (1 - fit$mu[1]) * (fit$mu[1] * (1 - fit$mu[1]) / fit$sigma[1]^2 - 1)
alpha2 <- fit$mu[2] * (fit$mu[2] * (1 - fit$mu[2]) / fit$sigma[2]^2 - 1)
beta2 <- (1 - fit$mu[2]) * (fit$mu[2] * (1 - fit$mu[2]) / fit$sigma[2]^2 - 1)

hist(swiss$Catholic, breaks = 10, probability = TRUE, col = "lightgreen", 
     main = "Histograma de Catholic con Densidad Beta Bimodal", 
     xlab = "Valores de Catholic", ylab = "Frecuencia")

lines(density(swiss$Catholic), col = "green", lwd = 2)

# Generar puntos para la curva de densidad
curve_catholic <- seq(catholic_min, catholic_max, length.out = 1000)
curve_catholic_norm <- (curve_catholic - catholic_min) / (catholic_max - catholic_min)
# Calcular la densidad beta bimodal
density_y <- fit$lambda[1] * dbeta(curve_catholic_norm, alpha1, beta1) + 
  fit$lambda[2] * dbeta(curve_catholic_norm, alpha2, beta2)
# Escalar la densidad al rango original de x
density_y <- density_y / (catholic_max - catholic_min)
# Superponer la curva de densidad
lines(curve_catholic, density_y, col = "red", lwd = 2)
# Agregar leyenda
#legend("topright", legend = "Densidad beta bimodal", col = "red", lwd = 2)
#caja y bigotes
boxplot(swiss$Catholic, main="Gráfico de Caja y Bigotes", xlab="Catholic", ylab="Valores", col="lightgreen", border="black")
#verificar normalidad
qqnorm(swiss$Catholic, xlab = "Distribución Normal", ylab = "Catholic")
qqline(swiss$Catholic, col = "red")  # Línea de referencia
################################################################################

##############################Infant.Mortality##################################

swiss$Infant.Mortality
#Medidas de Tendencia Central
infantMortality_mean <- mean(swiss$Infant.Mortality) #media
densidad <- density(swiss$Infant.Mortality)
infantMortality_freq <- densidad$x[which.max(densidad$y)] #moda
infantMortality_median <- median(swiss$Infant.Mortality) #mediana
infantMortality_MTC <- data.frame(Media = infantMortality_mean, Moda = infantMortality_freq, Mediana = infantMortality_median)
infantMortality_MTC
#Medidas de Dispersion
infantMortality_var <- var(swiss$Infant.Mortality) #varianza
infantMortality_sd <- sd(swiss$Infant.Mortality) #desviacion estandar
infantMortality_min <- min(swiss$Infant.Mortality) #valor minimo
infantMortality_max <- max(swiss$Infant.Mortality) #valor maximo
infantMortality_range <- infantMortality_max - infantMortality_min #rango
infantMortality_cv <- (infantMortality_sd / infantMortality_mean) * 100 #coeficiente de variacion heterogeneo
infantMortality_MD <- data.frame(
  Varianza = infantMortality_var, Desviación_Estandar = infantMortality_sd, Mínimo = infantMortality_min, 
  Máximo = infantMortality_max, Rango = infantMortality_range, Coeficiente_de_Variación = infantMortality_cv)
infantMortality_MD
#Medidas de Posicion
infantMortality_quantile <- quantile(swiss$Infant.Mortality, c(0.25, 0.50, 0.75))
infantMortality_quantile
#plots
#histograma
hist(swiss$Infant.Mortality, breaks = 10, probability = TRUE, col = "blue", 
     main = "Histograma de Infant.Mortality con Densidad Normal", 
     xlab = "Valores de Infant.Mortality", ylab = "Frecuencia")
# Crear una secuencia de valores para la función normal
x <- seq(infantMortality_min - 3*infantMortality_sd, infantMortality_max + 3*infantMortality_sd, length = 50)
# Calcular la densidad normal
y <- dnorm(x, mean = infantMortality_mean, sd = infantMortality_sd)
# Añadir la función de densidad normal al histograma
lines(x, y, col = "red", lwd = 2)  # Agregar la línea de la densidad normal
lines(density(swiss$Infant.Mortality), col = "green", lwd = 2)
#caja y bigotes
boxplot(swiss$Infant.Mortality, main="Gráfico de Caja y Bigotes", xlab="Infant.Mortality", ylab="Valores", col="blue", border="black")
#verificar normalidad
qqnorm(swiss$Infant.Mortality, xlab = "Distribución Normal", ylab = "Infant.Mortality")
qqline(swiss$Infant.Mortality, col = "red")  # Línea de referencia
################################################################################

#############################Correlación de Variables###########################
plot(swiss)
cor(swiss, method = "spearman")

#Fertility-Agriculture (no hay)
spearman_cor <- cor(swiss$Fertility, swiss$Agriculture, method = "spearman")
plot(swiss$Fertility, swiss$Agriculture, pch = 19, col = "lightblue", 
     main = "Diagrama de Dispersión", 
     xlab = "Fertility", 
     ylab = "Agriculture")

#Fertility-Examination (no se sabe, pero casi)
par(mfrow=c(1,2))
pearson_cor <- cor(swiss$Fertility, swiss$Examination, method = "pearson")
plot(swiss$Fertility, swiss$Examination, pch = 19, col = "lightgreen", 
     main = "Diagrama de Dispersión", 
     xlab = "Fertility", 
     ylab = "Examination")
plot(swiss$Examination, swiss$Fertility, pch = 19, col = "lightgreen", 
     main = "Diagrama de Dispersión", 
     xlab = "Examination", 
     ylab = "Fertility")

#Fertility-Education (no se sabe, pero casi)
spearman_cor <- cor(swiss$Fertility, swiss$Education, method = "spearman")
plot(swiss$Fertility, swiss$Education, pch = 19, col = "lightpink", 
     main = "Diagrama de Dispersión", 
     xlab = "Fertility", 
     ylab = "Education")

#Fertility-Catholic (no se sabe, pero casi no)
spearman_cor <- cor(swiss$Fertility, swiss$Catholic, method = "spearman")
plot(swiss$Fertility, swiss$Catholic, pch = 19, col = "yellow", 
     main = "Diagrama de Dispersión", 
     xlab = "Fertility", 
     ylab = "Catholic")

#Fertility-Infant.Mortality (no se sabe pero casi no)
pearson_cor <- cor(swiss$Fertility, swiss$Infant.Mortality, method = "pearson")
plot(swiss$Fertility, swiss$Infant.Mortality, pch = 19, col = "purple", 
     main = "Diagrama de Dispersión", 
     xlab = "Fertility", 
     ylab = "Infant.Mortality")

#Agriculture-Examination (no se sabe, pero casi)
spearman_cor <- cor(swiss$Agriculture, swiss$Examination, method = "spearman")
par(mfrow=c(1,2))
plot(swiss$Agriculture, swiss$Examination, pch = 19, col = "red", 
     main = "Diagrama de Dispersión", 
     xlab = "Agriculture", 
     ylab = "Examination")
plot(swiss$Examination, swiss$Agriculture, pch = 19, col = "red", 
     main = "Diagrama de Dispersión", 
     xlab = "Examination", 
     ylab = "Agriculture")

#Agriculture-Education (no se sabe, pero casi)
spearman_cor <- cor(swiss$Agriculture, swiss$Education, method = "spearman")
par(mfrow=c(1,2))
plot(swiss$Agriculture, swiss$Education, pch = 19, col = "blue", 
     main = "Diagrama de Dispersión", 
     xlab = "Agriculture", 
     ylab = "Education")
plot(swiss$Education, swiss$Agriculture, pch = 19, col = "blue", 
     main = "Diagrama de Dispersión", 
     xlab = "Education", 
     ylab = "Agriculture")

#Agriculture-Catholic (no se sabe pero casi no)
spearman_cor <- cor(swiss$Agriculture, swiss$Catholic, method = "spearman")
plot(swiss$Agriculture, swiss$Catholic, pch = 19, col = "brown", 
     main = "Diagrama de Dispersión", 
     xlab = "Agriculture", 
     ylab = "Catholic")

#Agriculture-Infant.Mortaity (no)
spearman_cor <- cor(swiss$Agriculture, swiss$Infant.Mortality, method = "spearman")
plot(swiss$Agriculture, swiss$Infant.Mortaity, pch = 19, col = "cyan", 
     main = "Diagrama de Dispersión", 
     xlab = "Agriculture", 
     ylab = "Infant.Mortaity")

#Examination-Education (no se sabe pero casi)
spearman_cor <- cor(swiss$Examination, swiss$Education, method = "spearman")
par(mfrow=c(1,2))
plot(swiss$Examination, swiss$Education, pch = 19, col = "salmon4"  , 
     main = "Diagrama de Dispersión", 
     xlab = "Examination", 
     ylab = "Education")
plot(swiss$Education, swiss$Examination, pch = 19, col = "salmon4"  , 
     main = "Diagrama de Dispersión", 
     xlab = "Education", 
     ylab = "Examination")

#Examination-Catholic(no se sabe pero casi no)
spearman_cor <- cor(swiss$Examination, swiss$Catholic, method = "spearman")
plot(swiss$Examination, swiss$Catholic, pch = 19, col = "black", 
     main = "Diagrama de Dispersión", 
     xlab = "Examination", 
     ylab = "Catholic")

#Examination-Infant.Mortality (no)
spearman_cor <- cor(swiss$Examination, swiss$Infant.Mortality, method = "pearson")
plot(swiss$Examination, swiss$Infant.Mortality, pch = 19, col = "green", 
     main = "Diagrama de Dispersión", 
     xlab = "Examination", 
     ylab = "Infant.Mortality")

#Education-Catholic(no)
spearman_cor <- cor(swiss$Education, swiss$Catholic, method = "spearman")
plot(swiss$Education, swiss$Catholic, pch = 19, col = "chartreuse4", 
     main = "Diagrama de Dispersión", 
     xlab = "Education", 
     ylab = "Catholic")

#Education-Infant.Mortality(no)
spearman_cor <- cor(swiss$Education, swiss$Infant.Mortality, method = "spearman")
plot(swiss$Education, swiss$Infant.Mortality, pch = 19, col = "tan3", 
     main = "Diagrama de Dispersión", 
     xlab = "Education", 
     ylab = "Infant.Mortality")

#Catholic-Infant.Mortality(no)
spearman_cor <- cor(swiss$Catholic, swiss$Infant.Mortality, method = "spearman")
plot(swiss$Catholic, swiss$Infant.Mortality, pch = 19, col = "violet", 
     main = "Diagrama de Dispersión", 
     xlab = "Catholic", 
     ylab = "Infant.Mortality")

################################################################################

#########################Categorizando variables################################
#Ferility
swiss$Fertility
Categ_Fertility <- cut(swiss$Fertility,
                       breaks = quantile(swiss$Fertility, probs = seq(0,1, by = 1/3)),
                       include.lowest = TRUE,
                       labels = c("Baja", "Media", "Alta"),
                       right = TRUE)
Categ_Fertility
table(Categ_Fertility)

#Agriculture
swiss$Agriculture
Categ_Agriculture <- cut(swiss$Agriculture,
                       breaks = quantile(swiss$Agriculture, probs = seq(0,1, by = 1/3)),
                       include.lowest = TRUE,
                       labels = c("Urbanitos", "Agr_Urb", "Agricultores"),
                       right = TRUE)
Categ_Agriculture
table(Categ_Agriculture)

#Examination
swiss$Examination
Categ_Examination <- cut(swiss$Examination,
                         breaks = quantile(swiss$Examination, probs = seq(0,1, by = 1/3)),
                         include.lowest = TRUE,
                         labels = c("Peor", "Normal", "Mejor"),
                         right = TRUE)
Categ_Examination
table(Categ_Examination)

#Education
swiss$Education
Categ_Education <- cut(swiss$Education,
                        breaks = quantile(swiss$Education, probs = seq(0,1, by = 1/3)),
                        include.lowest = TRUE,
                        labels = c("Mala", "Normal", "Buena"),
                        right = TRUE)
Categ_Education
table(Categ_Education)

#Catholic
swiss$Catholic
Categ_Catholic <- cut(swiss$Catholic,
                      breaks = quantile(swiss$Catholic, probs = seq(0,1, by = 1/3)),
                      include.lowest = TRUE,
                      labels = c("Protestantes", "Mezclados", "Católicos"),
                      right = TRUE)
Categ_Catholic
table(Categ_Catholic)

#Infant.Mortality
swiss$Infant.Mortality
Categ_Infant.Mortality <- cut(swiss$Infant.Mortality,
                      breaks = quantile(swiss$Infant.Mortality, probs = seq(0,1, by = 1/3)),
                      include.lowest = TRUE,
                      labels = c("Baja", "Media", "Alta"),
                      right = TRUE)
Categ_Infant.Mortality
table(Categ_Infant.Mortality)

################################################################################

###########Tabla de Contingencia y Prueba Chi Cuadrado de Independencia#########
tabla_proporciones <- prop.table(tabla_contingencia, margin = 1)
# Crear un gráfico de mosaico
mosaicplot(tabla_contingencia, col = "tan", main = "Tabla de Contingencia: Género vs Producto")

#Fertility-Agriculture
TC_F_A <- table(Categ_Fertility, Categ_Agriculture)
chisq_F_A <- chisq.test(TC_F_A)
TC_F_A <- addmargins(TC_F_A)
tabla <- matrix(c(6, 7, 3, 16,
                  5, 3, 7, 15,
                  5, 5, 6, 16,
                  16, 15, 16, 47), 
                nrow = 4, byrow = TRUE)
rownames(tabla) <- c("Baja", "Media", "Alta", "Sum")
colnames(tabla) <- c("Urbanitos", "Agr_Urb", "Agricultores", "Sum")
View(tabla)
qchisq(1-0.05, 4)

#Fertility-Examination
TC_F_Ex <- table(Categ_Fertility, Categ_Examination)
chisq_F_Ex <- chisq.test(TC_F_Ex)
TC_F_Ex <- addmargins(TC_F_Ex)
TC_F_Ex
matriz <- matrix(c(2, 4, 10, 16,
                   7, 3, 5, 15,
                   11, 5, 0, 16,
                   20, 12, 15, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Baja", "Media", "Alta", "Sum")
colnames(matriz) <- c("Peor", "Normal", "Mejor", "Sum")
View(matriz)

#Fertility-Education
TC_F_Ed <- table(Categ_Fertility, Categ_Education)
chisq_F_Ed <- chisq.test(TC_F_Ed)
TC_F_Ed <- addmargins(TC_F_Ed)
TC_F_Ed
matriz <- matrix(c(3, 4, 9, 16,
                   8, 4, 3, 15,
                   10, 2, 4, 16,
                   21, 10, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Baja", "Media", "Alta", "Sum")
colnames(matriz) <- c("Mala", "Normal", "Buena", "Sum")
View(matriz)

#Fertility-Catholic
TC_F_C <- table(Categ_Fertility, Categ_Catholic)
chisq_F_C <- chisq.test(TC_F_C)
TC_F_C <- addmargins(TC_F_C)
TC_F_C
chisq_F_C
matriz <- matrix(c(6, 9, 1, 16,
                   8, 4, 3, 15,
                   2, 2, 12, 16,
                   16, 15, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Baja", "Media", "Alta", "Sum")
colnames(matriz) <- c("Protestantes", "Mezclados", "Católicos", "Sum")
View(matriz)

#Fertility-Infant.Mortality
TC_F_IM <- table(Categ_Fertility, Categ_Infant.Mortality)
chisq_F_IM <- chisq.test(TC_F_IM)
TC_F_IM <- addmargins(TC_F_IM)
TC_F_IM
chisq_F_IM
matriz <- matrix(c(9, 3, 4, 16,
                   4, 7, 4, 15,
                   3, 5, 8, 16,
                   16, 15, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Baja", "Media", "Alta", "Sum")
colnames(matriz) <- c("Baja", "Media", "Alta", "Sum")
View(matriz)

#Agriculture-Examination
TC_A_Ex <- table(Categ_Agriculture, Categ_Examination)
chisq_A_Ex <- chisq.test(TC_A_Ex)
TC_A_Ex <- addmargins(TC_A_Ex)
TC_A_Ex
chisq_A_Ex
matriz <- matrix(c(3, 3, 10, 16,
                   5, 6, 4, 15,
                   12, 3, 1, 16,
                   20, 12, 15, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Urbanitos", "Agr_Urb", "Agricultores", "Sum")
colnames(matriz) <- c("Peor", "Normal", "Mejor", "Sum")
View(matriz)

#Agriculture-Education
TC_A_Ed <- table(Categ_Agriculture, Categ_Education)
chisq_A_Ed <- chisq.test(TC_A_Ed)
TC_A_Ed <- addmargins(TC_A_Ed)
TC_A_Ed
chisq_A_Ed
matriz <- matrix(c(5, 2, 9, 16,
                   3, 5, 7, 15,
                   13, 3, 0, 16,
                   21, 10, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Urbanitos", "Agr_Urb", "Agricultores", "Sum")
colnames(matriz) <- c("Mala", "Normal", "Buena", "Sum")
View(matriz)

#Agriculture-Catholic
TC_A_C <- table(Categ_Agriculture, Categ_Catholic)
chisq_A_C <- chisq.test(TC_A_C)
TC_A_C <- addmargins(TC_A_C)
TC_A_C
chisq_A_C
matriz <- matrix(c(4, 10, 2, 16,
                   7, 4, 4, 15,
                   5, 1, 10, 16,
                   16, 15, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Urbanitos", "Agr_Urb", "Agricultores", "Sum")
colnames(matriz) <- c("Protestantes", "Mezclados", "Católicos", "Sum")
View(matriz)

#Agriculture-Infant.Mortality
TC_A_IM <- table(Categ_Agriculture, Categ_Infant.Mortality)
chisq_A_IM <- chisq.test(TC_A_IM)
chisq_A_IM
TC_A_IM <- addmargins(TC_A_IM)
TC_A_IM
matriz <- matrix(c(3, 9, 4, 16,
                   7, 1, 7, 15,
                   6, 5, 5, 16,
                   16, 15, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Urbanitos", "Agr_Urb", "Agricultores", "Sum")
colnames(matriz) <- c("Baja", "Media", "Alta", "Sum")
View(matriz)

#Examination-Education
TC_Ex_Ed <- table(Categ_Examination, Categ_Education)
chisq_Ex_Ed <- chisq.test(TC_Ex_Ed)
TC_Ex_Ed <- addmargins(TC_Ex_Ed)
TC_Ex_Ed
chisq_Ex_Ed
matriz <- matrix(c(15, 4, 1, 20,
                   3, 4, 5, 12,
                   3, 2, 10, 15,
                   21, 10, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Peor", "Normal", "Mejor", "Sum")
colnames(matriz) <- c("Mala", "Normal", "Buena", "Sum")
View(matriz)

#Examination-Catholic
TC_Ex_C <- table(Categ_Examination, Categ_Catholic)
chisq_Ex_C <- chisq.test(TC_Ex_C)
TC_Ex_C <- addmargins(TC_Ex_C)
TC_Ex_C
chisq_Ex_C
matriz <- matrix(c(5, 1, 14, 20,
                   6, 4, 2, 12,
                   5, 10, 0, 15,
                   16, 15, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Peor", "Normal", "Mejor", "Sum")
colnames(matriz) <- c("Protestantes", "Mezclados", "Católicos", "Sum")
View(matriz)

#Examination-Infant.Mortality
TC_Ex_IM <- table(Categ_Examination, Categ_Infant.Mortality)
chisq_Ex_IM <- chisq.test(TC_Ex_IM)
TC_Ex_IM <- addmargins(TC_Ex_IM)
TC_Ex_IM
chisq_Ex_IM
matriz <- matrix(c(6, 6, 8, 20,
                   2, 4, 6, 12,
                   8, 5, 2, 15,
                   16, 15, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Peor", "Normal", "Mejor", "Sum")
colnames(matriz) <- c("Baja", "Media", "Alta", "Sum")
View(matriz)

#Education-Catholic
TC_Ed_C <- table(Categ_Education, Categ_Catholic)
chisq_Ed_C <- chisq.test(TC_Ed_C)
TC_Ed_C <- addmargins(TC_Ed_C)
TC_Ed_C
chisq_Ed_C
matriz <- matrix(c(7, 3, 11, 21,
                   5, 2, 3, 10,
                   4, 10, 2, 16,
                   16, 15, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Mala", "Normal", "Buena", "Sum")
colnames(matriz) <- c("Protestantes", "Mezclados", "Católicos", "Sum")
View(matriz)

#Education-Infant.Mortality
TC_Ed_IM <- table(Categ_Education, Categ_Infant.Mortality)
chisq_Ed_IM <- chisq.test(TC_Ed_IM)
TC_Ed_IM <- addmargins(TC_Ed_IM)
TC_Ed_IM
chisq_Ed_IM
matriz <- matrix(c(6, 8, 7, 21,
                   3, 3, 4, 10,
                   7, 4, 5, 16,
                   16, 15, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Mala", "Normal", "Buena", "Sum")
colnames(matriz) <- c("Baja", "Media", "Alta", "Sum")
View(matriz)

#Catholic-Infant.Mortality
TC_C_IM <- table(Categ_Catholic, Categ_Infant.Mortality)
chisq_C_IM <- chisq.test(TC_C_IM)
TC_C_IM <- addmargins(TC_C_IM)
chisq_C_IM
TC_C_IM
matriz <- matrix(c(5, 6, 5, 16,
                   6, 5, 4, 15,
                   5, 4, 7, 16,
                   16, 15, 16, 47),
                 nrow = 4, byrow = TRUE)
rownames(matriz) <- c("Protestantes", "Mezclados", "Católicos", "Sum")
colnames(matriz) <- c("Baja", "Media", "Alta", "Sum")
View(matriz)

################################################################################

################Definir tipo de correlación que tienen las variables############
plot(swiss)
cor(swiss, method = "spearman")
cor(swiss, method = "pearson")

#Fertility y Examination
#según el coeficiente de correlación es lineal:
ggplot(swiss, aes(x = Fertility, y = Examination)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  stat_regline_equation(label.x = 60, label.y = 50)

ggplot(swiss, aes(x = Examination, y = Fertility)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ log(x)) +
  stat_regline_equation(label.x = 20, label.y = 90)

#sin embargo, existen valores atípicos de Fertility que pueden cambiar el tipo 
#de correlación, recortemos los varlores atípicos de Fertility en las dos variables
library(dplyr)
datos_filtrados <- swiss %>%
  filter(Fertility > 45)

plot(datos_filtrados)
cor(datos_filtrados, method = "spearman")
cor(datos_filtrados, method = "pearson")

#examination sería dependiete de fertility cuadraticamente
ggplot(datos_filtrados, aes(x = Fertility, y = Examination)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  stat_regline_equation(label.x = 60, label.y = 60)

#Fertility y Education
#education seria dependiente de fertility cuadraticamente
ggplot(swiss, aes(x = Fertility, y = Education)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  stat_regline_equation(label.x = 60, label.y = 60)

#Agriculturey Examination
ggplot(swiss, aes(x = Agriculture, y = Examination)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  stat_regline_equation(label.x = 90, label.y = 35)

ggplot(swiss, aes(x = Examination, y = Agriculture)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  stat_regline_equation(label.x = 35, label.y = 90)

#Agriculture y Education
ggplot(swiss, aes(x = Agriculture, y = Education)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  stat_regline_equation(label.x = 50, label.y = 50)

ggplot(swiss, aes(x = Education, y = Agriculture)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  stat_regline_equation(label.x = 20, label.y = 100)

#Examination y Education
ggplot(swiss, aes(x = Examination, y = Education)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  stat_regline_equation(label.x = 20, label.y = 50)

ggplot(swiss, aes(x = Education, y = Examination)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ log(x)) +
  stat_regline_equation(label.x = 20, label.y = 50)

#Catholic y casos de categorización
ggplot(swiss, aes(x = Fertility, y = Catholic, colour = Categ_Catholic)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  theme_minimal()

ggplot(swiss, aes(x = Agriculture, y = Catholic, colour = Categ_Catholic)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  theme_minimal()

ggplot(swiss, aes(x = Examination, y = Catholic, colour = Categ_Catholic)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  theme_minimal()

ggplot(swiss, aes(x = Education, y = Catholic, colour = Categ_Catholic)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  theme_minimal()

ggplot(swiss, aes(x = Infant.Mortality, y = Catholic, colour = Categ_Catholic)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  theme_minimal()

  ggplot(swiss, aes(x = Agriculture, y = Examination, colour = Categ_Examination)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  theme_minimal()


################################################################################

####################Regresión###################################################
plot(swiss)
cor(swiss, method = "spearman")
cor(swiss, method = "pearson")
boxplot(swiss)
pairs(swiss)

#predecir Fertility
modelo <- lm(Fertility ~ Examination, data = swiss)
summary(modelo)
par(mfrow = c(2, 2))
plot(modelo)
# Diagnóstico de supuestos
# 1. Linealidad
plot(modelo$fitted.values, modelo$residuals, main="Residuos vs. Valores Ajustados", xlab="Valores Ajustados", ylab="Residuos")
abline(h=0, col="red")
# 2. Normalidad de los residuos
shapiro.test(modelo$residuals)
qqnorm(modelo$residuals)
qqline(modelo$residuals, col="red")
mean(modelo$residuals) #veridicar que la media de los errores sea 0
sum(modelo$residuals) #veridficar que la suma de los errores sea 0
# 3. Homocedasticidad (Prueba de Breusch-Pagan)
bptest(modelo)
dwtest(modelo) #verificar que los errores sean indepientdetes
# 4. Multicolinealidad (Factor de Inflación de Varianza - VIF)
vif(modelo)
# Predicción si el modelo es válido
if (shapiro.test(modelo$residuals)$p.value > 0.05 & bptest(modelo)$p.value > 0.05 & max(vif(modelo)) < 10) {
  nueva_obs <- data.frame(Agriculture = 50, Examination = 10, Education = 10, Catholic = 50, Infant.Mortality = 20)
  prediccion <- predict(modelo, newdata = nueva_obs)
  print(paste("Predicción de Fertility:", round(prediccion, 2)))
} else {
  print("El modelo no cumple con los supuestos necesarios para hacer predicciones confiables.")
}

plot(swiss$Examination, swiss$Fertility, main = "Scatter Plot con línea de regresión")
abline(lm(swiss$Fertility ~ swiss$Examination), col = "red") # Agrega la línea de regresión

plot(swiss$Fertility, swiss$Examination, main = "Scatter Plot con línea de regresión")
abline(lm(swiss$Examination ~ swiss$Fertility), col = "red") # Agrega la línea de regresión

#predecir Examination
modelo <- lm(Examination ~ Agriculture + Fertility + Education , data = swiss)
summary(modelo)
par(mfrow = c(2, 2))
plot(modelo)
shapiro.test(modelo$residuals)
mean(modelo$residuals) #veridicar que la media de los errores sea 0
sum(modelo$residuals) #veridficar que la suma de los errores sea 0
# 3. Homocedasticidad (Prueba de Breusch-Pagan)
bptest(modelo)
dwtest(modelo) #verificar que los errores sean indepientdetes
# 4. Multicolinealidad (Factor de Inflación de Varianza - VIF)
vif(modelo)
################################################################################

######################Analisi de varianzas######################################
anova_model <- aov(swiss$Fertility ~ Categ_Catholic)
summary(anova_model)
par(mfrow = c(2, 2))
plot(anova_model)
res <- anova_model$residuals
plot(res)
plot(swiss$Fertility~Categ_Catholic)
#Si ANOVA es significativo, usamos Tukey HSD para ver qué grupos son diferentes.
TukeyHSD(anova_model)
shapiro.test(res)
bartlett.test(res, Categ_Catholic)
dwtest(anova_model)

anova_model <- aov(swiss$Examination ~ Categ_Catholic + Categ_Education )
res <- anova_model$residuals
shapiro.test(res)
dwtest(anova_model)
plot(swiss$Examination~Categ_Catholic)
plot(swiss$Examination~Categ_Agriculture)
summary(anova_model)
par(mfrow = c(2, 2))
plot(anova_model)
TukeyHSD(anova_model)

par(mfrow = c(1, 2))
boxplot(residuals(anova_model) ~ Categ_Catholic, 
        main = "Boxplot de Residuos por Categoría de Religión",
        xlab = "Categoría Católica", ylab = "Residuos",
        col = "lightblue")

boxplot(residuals(anova_model) ~ Categ_Education, 
        main = "Boxplot de Residuos por Categoría de Nivel Educativo",
        xlab = "Nivel Educativo", ylab = "Residuos",
        col = "lightblue")
################################################################################

#######################Análisis de componentes principales######################
#Verificar y estandarizar los datos
swiss_scaled <- scale(swiss) 
#Aplicar el PCA con prcomp()
pca_result <- prcomp(swiss_scaled, center = TRUE, scale. = TRUE)
#Examinar los resultados
summary(pca_result)  # Varianza explicada por cada componente
pca_result$rotation  # Carga de los componentes principales
pca_result$x  # Coordenadas de los datos en el nuevo espacio
#Visualizar los resultados
plot(pca_result, type = "l") #Varianza explicada por cada componente
biplot(pca_result, scale = 0) # Muestra observaciones y variables
#Gráfico de dispersión de los dos primeros componentes
pca_df <- as.data.frame(pca_result$x) 
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point() +
  ggtitle("PCA - Swiss Dataset") +
  theme_minimal()
################################################################################
