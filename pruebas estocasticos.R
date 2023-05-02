plot(x,cos(x))
prob_Ruina <- function(N, k, p){
  resultado <- 0
  q <- 1-p
  if(p==0.5){
    resultado <- (N-k)/N
  }else{
    resultado <- ((q/p)^k -(q/p)^N)/(1-(q/p)^N)
  }
  
  cat("La probabilidad de ruina es", resultado)
  plot(resultado, 1:1000)
}
prob_Ruina(15,5,0.5)
plot(3)


curve(cos(x),add = TRUE,-15,15,1000, col = "red")
curve(sin(x),add = TRUE, -15,15,1000, col = "blue")

linea <- rep(3,15)
linea1 <- rep(4,15)
graf1 <- plot(linea)
graf2 <- plot(linea1)
plot(c(linea,linea1))