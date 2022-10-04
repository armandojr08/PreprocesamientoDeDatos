
# Librerias ---------------------------------------------------------------

library(mice)
library(VIM)
library(funModeling)
library(lattice)


# Datos -------------------------------------------------------------------

data("airquality")


# Exploracion basica de datos ---------------------------------------------

summary(airquality)
describe(airquality)
funModeling::profiling_num(airquality)
df_status(airquality)


# Exploracion de datos faltantes ------------------------------------------

# Patron de perdida
VIM::aggr(airquality,
          col = mice::mdc(1:2),
          numbers = TRUE,
          sortVars = TRUE,
          labels = names(airquality), 
          ylab = c("Proporcion de perdida", "Patron de perdida"))

md.pattern(airquality, plot = F)

# Distribucion de observaciones compeltas 
# eincompletas por pares de variables
colnames(airquality)
VIM::marginplot(airquality[,c(1,2)],
                col = c("blue", "red", "orange"),
                pch = 10)


# Imputacion a traves de la media -----------------------------------------

air_imput_media <- mice(airquality, method = "mean", m = 1, maxit = 1)
stripplot(air_imput_media, pch = 19, xlab = "Numeros imputados")
air_imput_media <- complete(air_imput_media)


# Imputacion por regresion ------------------------------------------------

imp_air_reg <- mice(airquality, method = "norm.predict", m = 1, maxit = 1)
imp_air_reg$imp$Ozone
imp_air_reg$imp$Solar.R
stripplot(imp_air_reg, pch = 19, xlab = "Numeros imputados")
imp_air_reg <- complete(imp_air_reg)

