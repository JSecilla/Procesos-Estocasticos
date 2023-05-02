**--# Ejercicio 1

randomwalk<-function(n,p) {                            # Genera los n pasos de una caminata
  steps=sample(c(-1,1),n,replace=TRUE,prob=c(1-p,p))   # aleatoria de probabilidad p.
  walk=cumsum(steps)
  return(walk)
}

originreturn<-function(n,p) {                          # Examina si la caminata aleatoria ha
  if (length(which(randomwalk(n,p)==0))>0) {           # vuelto al origen o no, devolviendo
    return(1)                                          # 1 o 0 respectivamente.
  } else {
    return(0)
  }
}

returnprobability<-function(n,p,k) {                   # Repite la función anterior k veces y
  results=replicate(k,originreturn(n,p))               # halla la frecuencia de regreso al origen.
  probability=mean(results)
  return(probability)
}

                                                       # Elabora una tabla comparando las
diagram<-function(n,k) {                               # frecuencias y probabilidades teóricas
                                                       # de 21 valores de p desde 0 a 1.   
  M=matrix(NA,nrow=4,ncol=21)                          
  rownames(M)=c("p","Frequency","Theoretical probability","Error")   
  for (i in 0:20) {
    p=i/20
    theoretical=1-abs(p-(1-p))
    M[1,i+1]=p
    M[2,i+1]=returnprobability(n,p,k)
    M[3,i+1]=theoretical
    M[4,i+1]=M[2,i+1]-M[3,i+1]
  }
  return(M)
}

# Ejercicio 2

timeofreturn<-function(p) {                            # Simula una caminata aleatoria hasta
  step=0                                               # que vuelve al origen y devuelve
  time=0                                               # el número de pasos hasta que regresó.
  repeat {
    time=time+1
    step=step+sample(c(-1,1),1,replace=TRUE,prob=c(1-p,p))
    if (step==0) break
  }
  return(time)
}

averagetime<-function(n,p) {                           # Simula n caminatas, almacena sus tiempos
  times=c()                                            # de regreso y grafica el tiempo medio
  average=c()                                          # de regreso desde 1 caminata hasta n.
  for (i in 1:n) {
    times=c(times,timeofreturn(p))
    average=c(average,mean(times))
  }
  plot(1:n,average,type="l",xlab="Sample size",ylab="Average time of return")
}

#Ejercicio 3

gamblersruin<-function(N,k,p) {                        # Simula un juego de apuestas unitarias
  capital = k                                          # hasta que algún jugador se arruina,
  step = 0                                             # devolviendo el capital final del primero.  
  repeat {
    step=step+1
    capital=capital+sample(c(-1,1),1,prob=c(1-p,p))
    if (capital==0 || capital==N) break
  }
  return(capital)
}


ruinprobability<-function(N,k,p,n) {                   # Simula n juegos de apuestas y devuelve
  samp=replicate(n,gamblersruin(N,k,p))                # la frecuencia relativa de ruina del
  probability=length(which(samp==0))/n                 # primer jugador.  
  return(probability)
}

theoreticalprobability<-function(N,k,p) {              # Calcula la probabilidad teórica de ruina
  q=1-p                                                # para unos valores N,k,p dados.  
  if (p==q) {
    theoretical = (N-k)/N
  } else if (p==0) {
    theoretical = 1
  } else {
    theoretical = ((q/p)^k-(q/p)^N)/(1-(q/p)^N)
  }
  return(theoretical)
}

randomaverage<-function(N,k,p,n) {                     # Simula la evolución de la frecuencia relativa
  average=c()                                          # de ruina para n apuestas.
  frequency=0
  for (i in 1:n) {
    capital=gamblersruin(N,k,p)
    if (capital==0) {
      frequency=frequency+1
    }
    average=c(average,frequency/i)
  }
  plot(1:n,average,type="l",xlab="Tamaño muestral",ylab="Frecuencia Relativa")
  theory=theoreticalprobability(N,k,p)
  abline(h=theory)
}


tabla1<-function(N,k,n) {                              # Elabora una tabla análoga a la del primer ejercicio.
  M=matrix(NA,nrow=3,ncol=21)                            
  rownames(M)=c("Frecuencia relativa","Probabilidad Teórica","Error")  
  colname=c()
  for (i in 0:20) {
    p=i/20
    colname=c(colname,p)
    theory=theoreticalprobability(N,k,p)
    M[1,i+1]=ruinprobability(N,k,p,n)
    M[2,i+1]=theory
    M[3,i+1]=M[1,i+1]-M[2,i+1]
  }
  colnames(M)=colname
  return(M)
}


tabla2<-function(N,k,n) {                              # Elabora una tabla para p entre 0,4 y 0,6.    
  M=matrix(NA,nrow=3,ncol=21)                            
  rownames(M)=c("Frecuencia relativa","Probabilidad teórica","Error")   
  colname=c()
  for (i in 0:20) {
    p=0.4+i/100
    colname=c(colname,p)
    theory=theoreticalprobability(N,k,p)
    M[1,i+1]=ruinprobability(N,k,p,n)
    M[2,i+1]=theory
    M[3,i+1]=M[1,i+1]-M[2,i+1]
  }
  colnames(M)=colname
  return(M)
}



