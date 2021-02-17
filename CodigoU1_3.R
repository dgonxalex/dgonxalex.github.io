#===============================================================================
#
# GRÁFICOS EN R  
#===============================================================================
# 
# Inicialmente recordemos que podemos correr los códigos mediante :
#  + R
#  + RStudio
#  + R Comander ( librerias Rcmdr y RcomdrMisc)
#  + Tambien las anternativas en linea : 
#        + rextester (https://rextester.com/l/r_online_compiler )
#        + rdrr ( https://rdrr.io/snippets/ ) : permite cargar paquetes
#
#==============================================================================
# Existen varios paquetes que ayudan en el proceso de construcción de gráficos
#  + Con R base ( sin instalar paquetes adicionales a los que tiene el programa inicialmente)
#  + ggplot2
#  + highcharter  
#  + plotly
#  + shyni
#
#  Iniciaremos con R base
# Variables cualitativas (@) ---------------------------------------------------
#  Resumen
#  pie()  : para la elaboración de gráficos de tortas
#  barplot() : para diagrama de barras
#  Variables cuantitativas (#)--------------------------------------------------
#  hist() : para histogramas
#  boxplot() : para diagramas de cajas
#  plot(density()) : para diagramas de densidad
#  plot(x,y) : para diagramas de dispersión
#  plot(t,y, type="l") : para la representación de series de tiempo
# ------------------------------------------------------------------------------
#  Vamos a utilizar las  bases que trae R base
data("iris")
data("mtcars")

# datos de estudiantes que cursaron Matemáticas Fundamentales
cc=c(20, 10, 20, 20, 20, 20, 20, 20, 20, 30, 20, 20, 20, 10, 30, 20, 20, 30, 20,
     30, 30, 20, 10, 30, 20, 20, 30, 30, 10, 20, 10, 20, 20, 20, 10, 20, 10, 20, 
     20, 30, 30, 30, 10, 30, 20, 20, 20, 20, 20, 20, 10, 20, 30, 30, 10, 10, 10, 
     20, 10, 20, 10, 30, 20, 10, 20, 30, 10, 30, 30, 30, 20, 30, 30, 30, 30, 30, 
     30, 20, 10, 30, 10, 20, 20, 10, 20, 20, 20, 20, 10, 20) 
labs=c("Ing. Industrial","Administración ","Contaduría ")

nf=c(4.1, 2.7, 3.1, 3.2, 3.0, 3.2, 2.0, 2.4, 1.6, 3.2, 3.1, 2.6, 2.0, 2.4, 2.8, 
     3.3, 4.0, 3.4, 3.0, 3.1, 2.7, 2.7, 3.0, 3.8, 3.2, 2.2, 3.5, 3.5, 3.8, 3.5, 
     3.9, 4.2, 4.3, 3.9, 3.2, 3.5, 3.5, 3.7, 4.1, 3.7, 3.5, 3.6, 3.2, 3.1, 3.4, 
     3.0, 3.0, 3.0, 2.7, 1.7, 3.6, 2.1, 2.4, 3.0, 3.1, 2.5, 2.5, 3.6, 2.2, 2.4, 
     3.1, 3.3, 2.7, 3.7, 3.0, 2.7, 3.0, 3.2, 3.1, 2.4, 3.0, 2.7, 2.5, 3.0, 3.0, 
     3.0, 3.2, 3.1, 3.8, 4.1, 3.7, 3.5, 3.0, 3.7, 3.7, 4.1, 3.7, 3.9, 3.7, 2.0)

#.............................................................................
#  VARIABLES CUANLITATIVAS
# .............................................................................
#
# Diagrama de tortas
t1=table(cc)
pie(t1)
# Ahora un poco mas elaborada
labs=c("Ing. Industrial","Administración ","Contaduría ")
pct=round(t1/sum(t1)*100,1)
labs=paste(labs, pct)
labs=paste(labs, "%", sep = " ") 
pie(t1, labels=labs, main=" Distribución por carrera")
#
#-------------------------------------------------------------------------------
# Diagrama de barras
op=c(3, 4, 4, 3, 4, 4, 5, 5, 4, 5, 4, 3, 4, 5, 5, 4, 3, 3, 2, 3, 3, 3, 4, 5, 5,
     3, 2, 5, 4, 3, 4, 4, 4, 4, 5, 5, 5, 3, 5, 4, 3, 4, 4, 4, 5, 5, 4, 3, 5, 4,
     4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 5, 3, 4, 4, 4, 4, 4, 2, 3, 5, 4, 3, 5, 4, 4, 
     4, 4, 5, 5, 5, 4, 4, 4, 5, 1, 4, 5, 3, 5, 4)


t2=table(op)
barplot(t2)
# ahora un grafico mas elaborado
barplot(t2, col=c("red","yellow","orange","green","blue"),
        main = "Evaluación proceso de inducción", ylim = c(0,50))
grid()
# ------------------------------------------------------------------------------
# Diagrama de barras con dos variables
t3=table(cc,op)

rownames(t3)=c("Ing. Industrial","Administración ","Contaduría ")
colnames(t3)=c("Muy Regular","Regular","Bien","Muy bien","Excelente")
barplot(t3, main="Percepción del resultado obtenido por carrera",
        xlab="Carrera",
        ylab="Percepcion", col=c("orange","green","blue"),
        legend = rownames(t3))

#-------------------------------------------------------------------------------
#
# VARIABLES CUANTITATIVAS
#...............................................................................
# Diagrama de tallos y hojas
stem(nf)
#
# histograma
hist(nf)
# si deseo cambiar el numero de intervalos 
hist(nf, breaks=3)
# o indicando lo valores de los cortes
hist(nf, breaks=c(0,0.99,1.99,2.99,3.99,5))
# ahora agregemos las etiquetes y el color
hist(nf, breaks=c(0,0.99,1.99,2.99,3.99,5),
     main = "Notas finales de Matemáticas Fundamentales",
     xlab = "Nota",
     ylab = "Frecuencia relativa",
     col=c("red","yellow","orange","green","blue"), ylim = c(0,0.70))
grid()
# ..............................................................................
# Diagrama de cajas
boxplot(nf)
# ahora agregamos otros componentes
boxplot(nf, 
        main="Nota final matemáticas fundamentales",
        col="dodgerblue3")
abline(h=3, col="red")  # linea roja que indica el limite de perdida
# ------------------------------------------------------------------------------
# Diagrama de cajas por factor
boxplot(nf~cc)

# le agregamos otros componentes estéticos
boxplot(nf~cc,
        main="Nota final matemáticas fundamentales por carrera", 
        col="dodgerblue3", xlab = "Carrera")

abline(h=3, col="red") 
abline(h=4, col="blue")
#-------------------------------------------------------------------------------
# Diagrama de puntos
ed=round(rnorm(90,18,1),1) # se genera la variable numérica  edad
plot(ed,nf)
# ahora le agregamos componentes estéticos
plot(ed,nf, 
     main="Edad vs Nota final matemáticas fundamentales", 
     ylim = c(0,5), 
     xlab = "Edad", 
     ylab = "Nota final",
     col="dodgerblue3",pch=19)
grid()
#-------------------------------------------------------------------------------
# Grafico de serie de tiempo
# para este grafico utilizamos base de datos de R
# la variable esta en formato serie, indexada a unas fechas
plot(AirPassengers)
#  Ahora adicionamos componentes estéticos
plot(AirPassengers, 
     main="Numero de pasajeros por mes", 
     col="dodgerblue3", 
     lwd = 3)
#-------------------------------------------------------------------------------
# se puede construir una ventana con varios graficos al tiempo
par(mfrow=c(2, 2))
hist(nf)
pie(t1)
boxplot(nf~cc)
plot(nf,ed)
#
#===============================================================================
# GRAFICOS EN GGPLOT2
# Para los siguientes gráficos emplearemos la librería "ggplot2"
#
#-------------------------------------------------------------------------------
library(ggplot2)
# a esta librería le dicen la gramática de los gráficos"
# la sintaxis de los gráficos bajo ggplot2 tiene una estructura formada por capas
#
# Data: capa de los datos
# Aesthetics: capa estetica (**aes**), definimos las variables a utilizar en el gráfico
# Geometries: capa de geometrías, se define el tipo de gráfica a realizar
# Facets: capa de facetas, permite detallar la gráfica por categorías
# Statistics: capa de estadística, permite agregar modelos
# Coordinates: capa de coordenadas, permite ajustar las escalas de los ejes
# Theme: capas de características del gráfico que no dependen de  los datos
#
# Todo grafico debe empezar definiendo la base de datos que va a utilizar,
# luego las variables que emplea, luego el tipo de grafica y asi se suman
# en fomra de capas los componentes
# supongamos que tenemos la siguiente base conformada por las siguiente variables
datos=data.frame(cc,nf,ed)

ggplot(data=datos)  # visualiza un lienzo en blanco

ggplot(data=datos, aes(y=nf)) # traza grid y nombra los ejes

ggplot(data=datos, aes(y=nf))+
  geom_boxplot()

ggplot(data=datos, aes(y=nf, factor(cc)))+
  geom_boxplot()+geom_point()

ggplot(data=datos, aes(y=nf, factor(cc)))+
  geom_boxplot()+
  geom_jitter()

p0=ggplot(data=datos, aes(y=nf, factor(cc)))+
  geom_boxplot()+
  geom_jitter(aes(color = nf), size = 1.9,alpha=0.9)+
  xlab("Carreras") +
  ylab("Nota final de Matemáticas Fundamentales") +
  labs(title="Rendimiento académico en Matemáticas Fundamentales")
p0
# -----------------------------------------------------------------------------
clasificacion= read.csv("~/github/dgonxalex.github.io/bases/spi_global_rankings_intl.csv")
p1= ggplot(clasificacion, aes(x=off , y=def))  
p1= p1 +  geom_point()
p1= p1 +  facet_wrap(~ confed)
p1= p1 +  coord_cartesian(ylim = c(0, 10))
p1
#-------------------------------------------------------------------------------
# resumen ggplot2
# https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
#===============================================================================
# GRÁFICOS EN HIGHCRARTER 
#
# ==============================================================================
# Esta librería permite construir graficos interactivos 
# estructura general : 
# hchart(<data.frame>, <type_of_chart>, hcaes(<aesthetics>), ...)
library(highcharter)

hchart(mtcars, "scatter", hcaes(wt, mpg, z = hp)) %>%
  hc_title(text = "Peso vs Millas por galon")
# wt  : peso del vehículo
# mpg : millas por galón
# hp :  caballos de fuerza 

# tipos : "scatter", "column", "line", "spline", "area", areaspine", "treemap"

#-------------------------------------------------------------------------------
hcboxplot(datos$nf, 
          var=datos$cc, 
          name = "Notas",
          color = "#2980b9",
          outliers = TRUE)

#-------------------------------------------------------------------------------
hchart(AirPassengers)
#===============================================================================
# GRAFICOS EN PLOTLY
#
#===============================================================================
library(plotly)
p2 <- plot_ly(iris, x = ~Petal.Length, y = ~Petal.Width,color = ~Species, mode = "markers")
p2
#-------------------------------------------------------------------------------
serie_0 <- rnorm(60, mean = 10)
serie_1 <- rnorm(60, mean = 5)
serie_2 <- rnorm(60, mean = 3)
t <- c(1:60)

data <- data.frame(t, serie_0, serie_1, serie_2)

p3= plot_ly(data, x = ~t)
p3= p3 %>% add_trace(y = ~serie_0, name = 'serie 0', mode = 'lines')
p3= p3 %>% add_trace(y = ~serie_1, name = 'serie 1', mode = 'lines+markers')
p3= p3 %>% add_trace(y = ~serie_2, name = 'serie 2', mode = 'markers')
p3= p3 %>% layout(title = "Comparacion de resultados ultimos 2 meses",
                  xaxis = list(title = "dias"), 
                  yaxis = list(title="indicadores"))
p3
#-------------------------------------------------------------------------------
# convierte un objeto ggplot en plotly
ggplotly(p0)
#-------------------------------------------------------------------------------

# Todo lo anterior lo podemos visualizar en un tablero (Dashboard)
# para ello debemos crear un archivo RMarkdown : File / New File/ R Markdown / From Template
#  Flex Dashboard
# En este archivo podemos colocar los trazos que hemos realzado y construir 
# nuestro tablero de resultados estadísticos

