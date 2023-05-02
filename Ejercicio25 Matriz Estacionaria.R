
P <- matrix(c(0,1,0,0,0,0,0.25,0,1/4,1/4,1/4,0,0,1/4,0,1/4,1/4,1/4,0,1/4,1/4,0,1/4,1/4,0,1/3,1/3,1/3,0,0,0,0,1/2,1/2,0,0), ncol = 6, byrow = TRUE)
steps = 10
times = 50
I= diag(1, 6, 6)
X = matrix(NA,ncol = steps+1, nrow = times)
X[,1] = rep(1,times)
for(k in 1:times){
  for(n in 1:steps){
    X[k ,n+1] =  sample(c(1:6), size= 1, replace = TRUE, prob = P[X[k,n],])
  }
}

print(X)

h <- matrix.power(P,100)
Pi <- h[1,]
print(Pi)
comprobacion <- t(Pi)%*%(P-I)
print(comprobacion)
det(P)