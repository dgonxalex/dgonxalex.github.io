# Codigo unidad 2
# Determine la probabilidad de que al lanzar dos dados:
#  a. La suma de los resultados sea mayor a 7
#  b. La resta de los números sea negativa
#  c. Su multiplicación sea mayor a 20
#  d. El resultado del dado1 sea mayor al del dado2 (existe un juego llamado 
#     guayabita que consiste en lanzar un dado y apostar dependiendo del 
#     resultado obtenido, pues se gana lo apostado si el segundo lanzamiento 
#     es mayor al valor obtenido en el primero)
#  e. En el caso de agregar otro dado (en total 3 dados), la suma esté 
#     entre 10 y 15

#-------------------------------------------------------------------------------
# sumulacion de dos dados
n= 100000 # numero de lanzamientos
x=sample(1:6, n*2, rep=TRUE)
dd1=matrix(x,nrow = n)
suma=apply(dd1,1,sum)
prob_a=sum(as.numeric(suma>7))/n
prob_a
# uno de los resulados obtenidos es :
# > prob_a
# [1] 0.416732
#-------------------------------------------------------------------------------
rest=function(y){y[1]-y[2]}
resta=apply(dd1,1,rest)
prob_b=sum(as.numeric(resta<0))/n
prob_b
# uno de los resulados obtenidos es :
# > prob_b
# [1] 0.416633
#-------------------------------------------------------------------------------
produc=apply(dd1,1,prod)
prob_c=sum(as.numeric(produc>20))/n
prob_c
# uno de los resulados obtenidos es :
# > prob_c
# [1] 0.167024
#-------------------------------------------------------------------------------
# simulacion segundo par de datos
y=sample(1:6, n*2, rep=TRUE)
dd2=matrix(y,nrow = n)
guay=as.numeric(dd1<dd2)
prob_d=sum(guay)/n
prob_d
# uno de los resulados obtenidos es :
# > prob_d
# [1] 0.8301
# -----------------------------------------------------------------------------
# simulacion tercer par de datos
z=sample(1:6, n*2, rep=TRUE)
dd3=matrix(z,nrow = n)
suma1=apply(dd1,1,sum) 
suma2=apply(dd2,1,sum) 
suma3=apply(dd3,1,sum)
sumat=suma1+suma2+suma3
prob_e=sum(as.numeric(sumat>9 & sumat<16))/n
prob_e
# uno de los resulados obtenidos es :
# > prob_e
# [1] 0.09555
#------------------------------------------------------------------------------




