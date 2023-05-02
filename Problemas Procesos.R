##
# Datos ejercicio 2.25
P1 = matrix(c(  0.84, 0.11, 0.01, 0.04, 0.00,
               0.03, 0.80, 0.04, 0.10, 0.03,
               0.01, 0.15, 0.70, 0.07, 0.07,
               0.03, 0.19, 0.02, 0.75, 0.01,
               0.03, 0.09, 0.05, 0.00, 0.83), nrow = 5,byrow = T)
S = c("s","t","f","m","r")

#Vamos a ver como se comporta la matriz cuando damos muchos pasos en la cadena
X <-matrix.power(P1,10000)
comportamiento <- X[1,]

dist_est <- function(P){
  u = eigen(t(P))$vectors[,1]
  print(u)
  u/sum(u)
}
dist_est(P2)
#Vemos que el comportamiento cuando damos muchos pasos coincide con la distribucion estacionaria,
#por lo que podemos afirmar que los delfines se comportaran de esa forma en el largo plazo.



##
# Datos ejercicio 3.1
P2 = matrix(c(   1/2, 1/4,   0, 1/4,
                0, 1/2, 1/2,   0,
                1/4, 1/4, 1/2,   0,
                0, 1/4, 1/2, 1/4), nrow = 4,byrow = T)

dist_est(P2)
matrix.power(P2,10000)
eigen(t(P1))
solucion <- a(1/5,1/3,2/5,1/15)

##
#Ejercicio 2.1
P <- matrix(c(0.1, 0.3, 0.6,
              0, 0.4, 0.6,
              0.3, 0.2, 0.5), nrow=3, byrow = TRUE)
di <- c(0.2,0.3,0.5)
matrix.power(P, 2)
di%*%matrix.power(P,2) 

##
#Ejercicio 2.2
P <- matrix(c(0, 1/2, 1/2,
              1, 0, 0,
              1/3, 1/3, 1/3), nrow=3, byrow = TRUE)
di <- c(1/2,0,1/2)

di %*% matrix.power(P,2)
matrix.power(P,2)

##
#Ejercicio 2.5
P <- matrix(c(   0, 1, 0, 0,
                 1/4, 0, 3/4,0,
                 0, 1/4, 0, 3/4,
                 0, 0, 1, 0), nrow = 4,byrow = T)
di <- c(1/4,1/4,1/4,1/4)

di%*%matrix.power(P,3)
matrix.power(P,10000)

##
#Ejercicio 2.6
P <- matrix(c(   1/4, 1/4, 1/4, 1/4,
                 0, 2/4, 1/4,1/4,
                 0, 0, 3/4, 1/4,
                 0, 0, 0, 1), nrow = 4,byrow = T)
di <- c(1/4,1/4,1/4,1/4)
di%*%matrix.power(P,4)

##
#Trabajo Procesos
P2 <- matrix(c(0, 1, 0, 0, 0,
              1/8, 1/8, 1/8, 3/8, 2/8,
              3/8, 2/8, 1/8, 1/8, 1/8,
              2/8, 2/8, 1/8, 1/8, 2/8,
              1/8, 2/8, 1/8, 2/8, 2/8), nrow = 5, byrow = T)
di <- c(1,0,0,0,0)
di%*%matrix.power(P2,10000)
matrix.power(P2,10)
dist_est(P2)


##
#Problema 2.14
P3 <- matrix(c(0, 1, 0, 0, 0,
               0, 1/4, 3/4, 0, 0,
               0, 0, 2/4, 2/4, 0,
               0, 0, 0, 3/4, 1/4,
               0, 0, 0, 0, 1), nrow = 5, byrow = T)
v1 <- c(1,0,0,0,0)
v1 %*% matrix.power(P3, 6)


P3 <- matrix(c(0, 1, 0, 0, 0,
               1/2, 0, 1/2, 0, 0,
               0, 1/2, 0, 1/2, 0,
               0, 0, 1/2, 0, 1/2,
               0, 0, 0, 1, 0), nrow = 5, byrow = T)
matrix.power(P3, 999)