##
# simulCM(v0, P, n, S)
# Función para simular n pasos, X0, ..., Xn, de una Cadena de Markov con
# distribución inicial v0, matriz de trasición P y espacio de estados S
# S puede ser cualquier arreglo alfanumérico, el default es 1,2,...,card(S)
simulCM <- function(v0,P,n,S){ 
  if (missing(S)) S <- 1:length(v0)
  X <- numeric(n+1)
  labels <- 1:length(v0)
  X[1] <- sample(labels,1,prob=v0)
  for (paso in 2:(n+1)) 
  { X[paso] <- sample(labels,1,prob=P[X[paso-1],]) }
  S[X]
}
P2 <- matrix(c(0, 1, 0, 0, 0,
               1/8, 1/8, 1/8, 3/8, 2/8,
               3/8, 2/8, 1/8, 1/8, 1/8,
               2/8, 2/8, 1/8, 1/8, 2/8,
               1/8, 2/8, 1/8, 2/8, 2/8), nrow = 5, byrow = T)
di <- c(1,0,0,0,0)
matrix.power(P2, 10)

S <- c(1,2,3,4,5)
simulCM (di, P2, n, S)
m = 10
n = 10



simulacion <- function(){
  P <- matrix(c(0, 1, 0, 0, 0,
                 1/2, 0, 1/2, 0, 0,
                 0, 0.2, 0.1, 0.7, 0,
                 0, 0, 0.5, 0, 0.5,
                 0.2, 0, 0, 0.6, 0.2), nrow = 5, byrow = T)
  matrix.power(P, 1000)
  di <- c(1,0,0,0,0)
  S <- c(1,2,3,4,5)
  m = 100
  n = 10 
  X = matrix(nrow = m, ncol = n+1)
for(k in 1:m){
  X[k,] = simulCM (di, P2, n, S)
}
contador <- 0
for(i in 1: nrow(X)){
  cariño <- 0
  comida <- 0
  for(j in 1:ncol(X)){
      if(X[i,j]== 4){
        comida = comida + 1
      }else if(X[i,j]==5){
        cariño = cariño + 1
      }
  }
  if(cariño > comida){
    contador <- contador + 1
  }
}
print(contador)
prob <- contador/m
print(prob)
}
simulacion()

probfinal <- function(){
H = c()
for(k in 1:1000){
  H[k] <- simulacion()
}
probabilidad <- (sum(H))/1000
return(probabilidad)
}
probfinal()



probfinal2 <- function(){
  H = c()
  for(k in 1:100000){
    H[k] <- simulacion()
    probabilidad[k] <- (sum(H))/k
  }
  plot(probabilidad, type = "l")
  abline(h=0.05)
}
probfinal2()



grafico <- function(){
  f = c()
  for(i in 1:100){
    f[i]<- probfinal()
  }
  plot(f, type="l")
}
grafico()
