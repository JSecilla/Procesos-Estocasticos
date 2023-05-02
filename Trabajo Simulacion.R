
prob_RuinaMMC <- function(p, k, N, trials){
  prob_ruina <- 0
  x <- 2*rbinom(trials,1,p) -1
  S <- cumsum(x)
  
  for(j in 1:trials){
    i <- 1
    S[i] <- k
    while(S[i]!=0 & S[i]!= N){
      S[i+1] <- S[i]+ 2*rbinom(1,1,p) -1
      i <- i + 1
    }
    if(S[i]== 0){prob_ruina <- prob_ruina + 1/trials}
  }
  cat("La probabilidad de  ruina según el método de Monte Carlo es", prob_ruina, "\n")
   resultado <- 0
   q <- 1-p
  if(p==0.5){
    resultado <- (N-k)/N
  }else{
    resultado <- ((q/p)^k -(q/p)^N)/(1-(q/p)^N)
  }
  cat("La probabilidad de ruina usando la fórmula es", resultado, "\n")
  cat("El error en esta prueba ha sido de", abs(resultado-prob_ruina))
}
prob_RuinaMMC(0.5,7,20,1000000)
x <- c(2,3,4,5,6,7, type="l")
plot(x)