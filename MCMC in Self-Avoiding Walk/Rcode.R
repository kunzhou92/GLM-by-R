library(MASS)
library(Rcpp)
sourceCpp("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project1\\project1.cpp")

theta.est <- function(mu, sigma, n, tar_mu, tar_sigma)
{
  sample = mvrnorm(n, mu=mu, Sigma=sigma)
  ones = matrix(1, nrow=nrow(sample))
  tar_temp = (sample - ones %*% t(tar_mu)) %*% solve(tar_sigma)
  tar_den = exp(applet(tar_temp, (t(sample) - tar_mu %*% t(ones))) / -2) / sqrt(det(tar_sigma))
  apr_temp = (sample - ones %*% t(mu)) %*% solve(sigma)
  apr_den = exp(applet(apr_temp, (t(sample) - mu %*% t(ones))) / -2) / sqrt(det(sigma))
  result = tar_den / apr_den * sample %*% matrix(c(1,1))
  return(mean(result))
}

tar_mu = matrix(c(2,2))
tar_sigma = diag(2)

#step1
mu1 = matrix(c(2,2))
sigma1 = diag(2)
n1=c()
result1=c()
for(i in seq(from=1, to=16, by=0.5))
{
  n1 = c(n1, floor(exp(i)))
  result1 = c(result1, theta.est(mu1, sigma1, floor(exp(i)), tar_mu, tar_sigma))
}
plot(log(n1), result1)

#step2
mu2 = matrix(c(0, 0))
sigma2 = diag(c(1, 1))
n2=c()
result2=c()
for(i in seq(from=1, to=16, by=0.5))
{
  n2 = c(n2, floor(exp(i)))
  result2 = c(result2, theta.est(mu2, sigma2, floor(exp(i)), tar_mu, tar_sigma))
}
plot(log(n2), result2)

#step3
mu3 = matrix(c(0, 0))
sigma3 = diag(c(1, 1))*16
n3=c()
result3=c()
for(i in seq(from=1, to=16, by=0.5))
{
  n3 = c(n3, floor(exp(i)))
  result3 = c(result3, theta.est(mu3, sigma3, floor(exp(i)), tar_mu, tar_sigma))
}

plot(log(n3), result1, type="l", col=1, lty=1, xlim=c(0, 17), ylim=c(0, 14), xlab="log(N)", ylab="estimated theta")
points(log(n2), result2, type="l", col=2, lty=2)
points(log(n3), result3, type="l", col=4, lty=4)
legend(14, 14, legend=c("step 1", "step 2", "step 3"), col=c(1,2,4), lty=c(1,2,4))


#2
mu2 = matrix(c(0, 0))
sigma2 = diag(c(1, 1))
omega_var(mu, sigma, 1000000, tar_mu, tar_sigma)

x^4/(2*x^2-1)*exp(8/(2*x^2-1))


#ii)
#n=5
theta1.n5 =c()
for(i in 1:1000)
{
  theta1.n5 = c(theta1.n5, theta.est(mu1, sigma1, 5, tar_mu, tar_sigma))
}
sd(theta1.n5)

theta2.n5 =c()
for(i in 1:500)
{
  theta2.n5 = c(theta2.n5, theta.est(mu2, sigma2, 210000, tar_mu, tar_sigma))
}
sd(theta2.n5)


theta3.n5 =c()
for(i in 1:500)
{
  theta3.n5 = c(theta3.n5, theta.est(mu3, sigma3, 1000, tar_mu, tar_sigma))
}
sd(theta3.n5)
# step1  step 2; 2 70000 150; 3 130000 250; 5 210000 400; 10 350000 800 ; 12 500000 1000
x = c(2, 3, 5, 10,12)
y = c(70000, 130000, 210000, 350000, 500000)
y_t = x * exp(8)
y.lm = lm(y~y_t)
plot(y_t, y, xlab="ess", ylab="ess*", main="Step 2")
abline(y.lm)

s=4
temp = s^4/(2*s^2-1)*exp(8/(2*s^2-1))
z = c(150, 250, 400, 800, 1000)
z_t = x * temp
z.lm = lm(z~z_t)
plot(z_t, z, xlab="ess", ylab="ess*", main="Step 3")
abline(z.lm)


###########
library(plotrix)
#1
sourceCpp("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project1\\project1.cpp")

result1 = method1_plot()
result2 = method2_plot()
result3 = method3_plot()
plot(log(result1[1,]), log(result1[2,]), type="l", col=1, lty=1, xlab="log(M)", ylab="log(K)", ylim=c(0, 60))
points(log(result2[1,]), log(result2[2,]), type="l", col=2, lty=2)
points(log(result3[1,]), log(result3[2,]), type="l", col=4, lty=4)
legend(14, 57, legend=c("method 1", "method 2", "method 3"), col=c(1,2,4), lty=c(1,2,4))

plot(log(result1[1,]), log(result1[2,]), type="l", col=1, lty=1, xlab="log(M)", ylab="log(K)", ylim=c(55, 60), xlim=c(5, 20))
points(log(result2[1,]), log(result2[2,]), type="l", col=2, lty=2)
points(log(result3[1,]), log(result3[2,]), type="l", col=4, lty=4)
legend(17, 60, legend=c("method 1", "method 2", "method 3"), col=c(1,2,4), lty=c(1,2,4))


#2
resultNN1 = SAWtoEnd1()
mean(resultNN1[1,]/resultNN1[2,])
resultNN2 = SAWtoEnd2()
mean(resultNN2[1,]/resultNN2[2,])
resultNN3 = SAWtoEnd3()
mean(resultNN3[1,]/resultNN3[2,])


#(3)

M = 10^6
result5 = method1_path(M);
plot(result5[1,1], result5[2,1], xlim=c(0, 11), ylim=c(0, 11), xlab="x", ylab="y")
i=1
while(i<200)
{
  if(i==1 || result5[1,i+1] || result5[2,i+1])
  {
    points(result5[1,i+1], result5[2,i+1])
    points(c(result5[1,i], result5[1,i+1]), c(result5[2,i], result5[2,i+1]), type="l")
  }
  i = i+1
}
M=10^8
result6 = SAW1(M)
mean(result6[1,])
dens1 = density(result6[2,], weights=result6[1,], bw = 1)
plot(dens1, xlab="path length", ylab="frequency", main="")
axis(1, at=c(seq(0, 130, 10), 93.7))
text(x=93, y=7.0, "93.7")
abline(v=93, col=2, lty=2)

M = 10^6
result7 = method2_path(M);
plot(result7[1,1], result7[2,1], xlim=c(0, 11), ylim=c(0, 11), xlab="x", ylab="y")
i=1
while(i<200)
{
  if(i==1 || result7[1,i+1] || result7[2,i+1])
  {
    points(result7[1,i+1], result7[2,i+1])
    points(c(result7[1,i], result7[1,i+1]), c(result7[2,i], result7[2,i+1]), type="l")
  }
  i = i+1
}

M=10^8
result8 = SAW2(M)
dens2 = density(result8[2,], weights=result8[1,], bw = 3)
plot(dens2, xlab="path length", ylab="frequency", main="")
text(x=95, y=7.0, "95.3")
abline(v=95.3, col=2, lty=2)

M = 10^6
result9 = method3_path(M);
plot(result9[1,1], result9[2,1], xlim=c(0, 11), ylim=c(0, 11), xlab="x", ylab="y")
i=1
while(i<200)
{
  if(i==1 || result9[1,i+1] || result9[2,i+1])
  {
    points(result9[1,i+1], result9[2,i+1])
    points(c(result9[1,i], result9[1,i+1]), c(result9[2,i], result9[2,i+1]), type="l")
  }
  i = i+1
}
M=10^8
result10 = SAW3(M)
dens3 = density(result10[2,], weights=result10[1,], bw = 1)
plot(dens3, xlab="path length", ylab="frequency", main="")
abline(v=94.7, col=2, lty=2)
text(x=94, y=7.0, "94.7")

M=10^8
q2(M)

