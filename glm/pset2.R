# Since we use BFGS method in optim(), there may be errors due to gradient 
#calculation. In that case, reuse probit(), because I use randomized intial
#paramenters in optim() to guarantee the program runs well for most initial
#values 
#
#y is a n-length vector, X is a n*p matrix
probit <-function(y, X) 
{
  #switch y to a n*1 matrix 
  Y = matrix(y, ncol=1)  
  #check if the dimensions of y and X are compatible
  if(nrow(Y) != nrow(X))   
    return("the number of responds and the number of 
           observations are not consistent")
  #set the initial coefficients
  initial_beta = matrix(rnorm(ncol(X), 0, 0.1),ncol=1) 
  #check if X contains intercept.  If not, add it to X and modify the dimension
  # of initial parameter. 
  if(prod(apply(X, 2, var))!= 0)
  {
    X = cbind(1, X)
    initial_beta = rbind(rnorm(1, 0, 0.1), initial_beta)
  }
  
  #establish the targetd log-likelyhood function 
  loglik <- function(beta) 
  {
    psi_value = pnorm(X %*% beta)  #use matrix multiplication for conveniece 
    return( t(Y)%*%log(psi_value) + t(1-Y) %*% log(1-psi_value)) 
  }
  #optimization
  probit_optim = optim(par = initial_beta, fn=loglik, 
                       method = "BFGS", control = list(fnscale = -1), 
                       hessian = TRUE)  
  #result
  result = list("coefficients"=probit_optim$par, "log-likelihood value"=
                  probit_optim$value, "variance"=
                  solve(-probit_optim$hessian)) 
  return(result)
}


#generate matrix X
X = matrix(NaN,nrow=200, ncol=3)
for(i in 1:3)
  X[,i] = rnorm(nrow(X))
#choose beta as (0, 1, 2)
beta = matrix(c(1, 2, 1), ncol=1)
#generate y
prob = pnorm(X%*%beta)
y = matrix(NaN, nrow=nrow(X), ncol=1)
for(i in (1:nrow(X)))
  y[i,] = rbinom(1, 1, prob[i])
#calculate the result
probit(y, X)  

result = probit(y, X)



#Distribution gotten from the asymptotic distribution
#
#set the coefficients of estimator, beta1+beta2+beta3
tran = matrix(c(0, 1, 1, 1), ncol=1)
#calculate variance
estimator_var = t(tran) %*% result$variance %*% tran
#calculate expectation
estimator_mean = sum(tran * result$coefficients)
#generate random points
random_point = rnorm(5000, estimator_mean, estimator_var)
#plot
plot(density(random_point, bw = 0.05))




#Distribution gotten from bootstrap
#
Itera = 8000
#set the coefficients of estimator, beta1+beta2+beta3
tran = matrix(c(0, 1, 1, 1), ncol=1)
#the set of all the estimators
estimator = matrix(NaN, nrow = Itera, ncol = 1)
i=1
N = nrow(X)

while(i <= Itera)
{
  #There may be errors due to method BFGS, so use tryCatch
  tryCatch(
    {
      #sample from X, y
      index = sample(1:N, N, replace = TRUE)
      X_new = X[index,]
      y_new = y[index, , drop=FALSE]
      #calculate the estimator
      estimator_new = sum(tran * probit(y_new, X_new)$coefficients)
      estimator[i] = estimator_new
      i = i + 1
    },
    error = function(e){}
  )
}
plot(density(estimator))




#Distribution gotten from simulation approximation
#
library(MASS)
Itera2 = 8000
estimator2 = matrix(NaN, nrow = Itera2, ncol = 1)
tran = matrix(c(0, 1, 1, 1), ncol=1)
i = 1

while(i <= Itera2)
{
  tryCatch(
    {
      #generate a new beta
      new_beta = matrix(mvrnorm(n = 1, mu = result$coefficients[-1], 
                                Sigma = result$variance[-1, -1]),
        ncol = 1)
      #generate new y
      prob_new = pnorm(X%*%new_beta)
      y_new_2 = matrix(NaN, nrow=nrow(X), ncol=1)
      for(j in (1:nrow(X)))
        y_new_2[j,] = rbinom(1, 1, prob[j])
      #calculate the estimator
      estimator_new = sum(tran * probit(y_new_2, X)$coefficients)
      estimator2[i] = estimator_new
      i = i + 1
    },
    error = function(e){}
  )
}
plot(density(estimator2))


#read_data
PS2_data = read.csv(file = "E:\\PS2_data.csv", header = TRUE)

cook_inc_risk = PS2_data$cook_inc_risk
diff = PS2_data$diff
same = PS2_data$same
scaled_comp_challenger = scale(PS2_data$comp_challenger) #standardize the data
scaled_comp_incumbent = scale(PS2_data$comp_incumbent) #standardize the data
inc_tenure = PS2_data$inc_tenure
tenure_squared = inc_tenure^2
inc_age = PS2_data$inc_age
age_squared = inc_age^2
#construct X
Senate_X = cbind(cook_inc_risk, diff, same, scaled_comp_challenger,
                    scaled_comp_incumbent, inc_tenure,  
                    inc_age)
#construct y
Senate_y = PS2_data$vote_incumbent
Senate_result = probit(Senate_y, Senate_X)
Senate_result$coefficients
sqrt(diag(Senate_result$variance))




library(sandwich)
library(zoo)
library(lmtest)
#regenerate a data frame about all the factors.
vote = PS2_data$vote_incumbent
state = PS2_data$state
Senate_2 = data.frame(cook_inc_risk, diff, same, scaled_comp_challenger,
                            scaled_comp_incumbent, inc_tenure,  
                            inc_age, vote, state)
#perform glm with binomial and link function, probit 
glm_Senate_2 = glm(vote~.-state, data = Senate_2, 
                 family = binomial(link = "probit"))
unique_state = unique(state)
m = length(unique_state)
p = ncol(model.matrix(glm_Senate_2))
#calculate score
scores=estfun(glm_Senate_2)
#clustered score
Senate_clust=matrix(NA,nrow=m,ncol=p)
for(j in 1:p)
  Senate_clust[,j]= tapply(scores[,j], state, sum)
meat.cl = (m/(m-1)) * t(Senate_clust) %*% Senate_clust
vcov.cl = vcov(glm_Senate_2)
vcov.cl = vcov.cl %*% meat.cl %*% vcov.cl
#retest.
round(coeftest(glm_Senate_2, vcov=vcov.cl),4)


