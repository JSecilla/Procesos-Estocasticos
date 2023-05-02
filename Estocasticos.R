#Paseo al azar
p <- 0.5
pasos <- 100000
x <- 2*rbinom(pasos,1,p) -1
S <- cumsum(x)
plot(1:pasos,S, type = "l")

#Prob de ruina del jugador
p <- 0.5
k <- 5
n <- 10
trials <- 10000
prob_ruina <- 0

for(j in 1:trials){
  i <- 1
  S[i] <- k
  while(S[i]!=0 & S[i]!= n){
    S[i+1] <- S[i]+ 2*rbinom(1,1,p) -1
    i <- i + 1
  }
  if(S[i]== 0){prob_ruina <- prob_ruina + 1/trials}
}

suppressPackageStartupMessages(library(distrEx))
k <- 5
g <- 3
u <- rpareto(10000, g, k)
head(u)
library(PtProcess)
x <- seq(0.1,10,by=0.1) 
fx <- dpareto(x, 1.5, 0.05) 
Fx <- ppareto(x, 1.5, 0.05) 
plot((1-Fx)/fx ~ x)


juego_urnas<-matrix(c(0,1,0,0,0,0,1/5,0,4/5,0,0,0,0,2/5,0,3/5,0,0,0,0,3/5,0,2/5,0,0,0,0,4/5,0,1/5,0,0,0,0,1,0),ncol=6,byrow=TRUE)
print(juego_urnas)

juego_ruina<-matrix(c(1,0,0,0,0,1/2,0,1/2,0,0,0,1/2,0,1/2,0,0,0,1/2,0,1/2,0,0,0,0,1),ncol=5,byrow=TRUE)
print(juego_ruina)
print(juego_ruina%*%juego_ruina)

juego_ruina^3

matrix.power(juego_ruina, 100000)

