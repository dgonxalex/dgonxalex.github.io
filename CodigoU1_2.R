#===============================================================================
#
# CONSTRUCCION DE TABLAS E INDICADORES DESCRIPTIVOS
#
#===============================================================================
# Resumirlos datos en tablas facilita su analisis y tambien su representacion
# grafica. Varios graficos estan basados en tablas como veremos en la unidad 1.3

# Antes de empezar vamos a cargar la base construida en el codigo anterior y
# las dos bases de Dataset de R

Colombia=readRDS(file = "Colombia.RDS")
data(iris)
data(car)


# TABLAS DE FRECUENCIA ---------------------------------------------------------
# VARIABLES CUALITATIVAS (@)
# Ubicacion
FrecAbs=table(Colombia$ubicacion) # frecuencia absoluta
FrecAbs
FreRel=prop.table(FrecAbs) # frecuencia relativa
cbind(FrecAbs,FreRel)


# otra altermativa directa

summarytools::freq(Colombia$ubicacion,big.mark = ",", cumul = FALSE, headings = FALSE)
#-------------------------------------------------------------------------------
# Ubicacion por estado
# esta es una tabla de doble entrada
table(Colombia$ubicacion, Colombia$estado)


#-------------------------------------------------------------------------------
# VARIABLES CUANTITATIVAS
# Primero revisamos el rango de la variable
summary(Colombia$edad)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   27.00   38.00   40.23   53.00  114.00 

# Si queremos establecer los limites y cuantos intervalo vamos aconstruir
# procedemos asi:
# Consultados los ciclos de la vida tenemos:
# Primera Infancia (0-5 años)
# Infancia (6 - 11 años)
# Adolescencia (12 - 17 años)
# Juventud (18 - 26 años)
# Adultez (27- 59 años)
# Persona Mayor (60 años o mas) envejecimiento y vejez

# Constrimos la tabla a partir del histograma asi:
h=hist(Colombia$edad, breaks = c(0,4.9, 10.9,17.9,25.9, 58.9,115))
# Con los valores contenidos en el objeto h construimos la tabla
LI=h$breaks[1:6]
LS=h$breaks[2:7]
FreAbs=h$counts
FreRel=round(h$counts/sum(h$counts),2)
FreAbAc=cumsum(FreAbs)
FreRAc=cumsum(FreRel)
data.frame(LI,LS,FreAbs,FreRel, FreAbAc, FreRAc)
#===============================================================================
# INDICADORES ESTADISTICOS
#===============================================================================
# VARIABLE CUALITATIVA (@)
install.packages("modeest")
library(modeest)
mfv(Colombia$ubicacion)
#-------------------------------------------------------------------------------
# VARIABLE CUANTITATIVA (#)
summarytools::descr(Colombia$edad)
#-------------------------------------------------------------------------------
summarytools::descr(iris)
#-------------------------------------------------------------------------------
summarytools::descr(cars)

# YA ESTAMOS LISTOS PARA INTERPRETAR LA INFORMACION PROCESADA
#===============================================================================
