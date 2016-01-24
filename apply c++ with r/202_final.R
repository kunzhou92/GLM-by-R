library(Rcpp)
sourceCpp("E:\\WorkSpace\\202 final\\202_final.cpp")
hist(uniform_rand(12345, 10000), xlim=c(0, 1), xlab="uniform random number",
     ylab="frequency", main="") #uniform random numbers between[0, 1]
hist(normal_rand(12345, 10000), xlim=c(-4, 4), xlab="normal random number",
     ylab="frequency", main="") #standard normal random numbers

hist(metropolis(n=3000, sigma=1), seq(-5.5,5.5, 1), xlab="sigma = 1",
     ylab="frequency", main="") #metropolis method
hist(metropolis(n=8000, sigma=5), seq(-20.5,20.5, 2), xlab="sigma = 5",
     ylab="frequency", main="")

data = gibbs(T=50, M=200, rho=0) #gibbs method with correlation 0.0
for (i in 1:5*10)
  plot(data[i,rep(c(TRUE,FALSE),200)], data[i,rep(c(FALSE, TRUE),200)],
       xlab="x", ylab="y", main="", xlim = c(-3, 3), ylim=c(-3, 3))
data = gibbs(T=50, M=200, rho=0.9) #gibbs method with correlation 0.9
for (i in 1:5*10)
  plot(data[i,rep(c(TRUE,FALSE),200)], data[i,rep(c(FALSE, TRUE),200)], 
       xlab="x", ylab="y", main="", xlim = c(-3, 3), ylim=c(-3, 3))
