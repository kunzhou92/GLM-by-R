#include <Rcpp.h>
#include<ctime>
#include<stdint.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector uniform_rand(double seed = 76543, int n = 1)
{
  double x = seed;
  NumericVector u;
  for(int i=0;i<n;i++)
  {
    x = fmod(pow(7,5)*x, pow(2,31)-1.0);
    u.push_back(x / (pow(2,31)-1.0));
  }
  return u;
}

// [[Rcpp::export]]
NumericVector normal_rand(double seed = 76543, int n = 1)
{
  const double pi = 3.14159265358979323846;
  NumericVector u;
  int half_n = n / 2;
  NumericVector temp;
  
  for (int i=0;i<half_n;i++)
  {
    temp = uniform_rand(seed*(i+1), 2);
    u.push_back(sqrt(-2 * log(temp[0])) * cos(2 * pi * temp[1]));
    u.push_back(sqrt(-2 * log(temp[0])) * sin(2 * pi * temp[1]));
  }
  if (n%2 == 1)
  {
    temp = uniform_rand(seed*(half_n+1), 2);
    u.push_back(sqrt(-2 * log(temp[0])) * cos(2 * pi * temp[1]));
  }
  return u;
}

// [[Rcpp::export]]
NumericVector metropolis(double seed = 76543, int n = 1, double sigma = 1)
{
  NumericVector result;
  int X=0, Y;
  double ratio;
  NumericVector unif = uniform_rand(76543, 2*n+2000);
  for(int i=0; i<999; i++)
  {
    if(unif[2*i] < 0.5)
      Y = X + 1;
    else 
      Y = X - 1;
    ratio = exp(-Y*Y / (2*sigma*sigma)) / exp(-X*X / (2*sigma*sigma));
    ratio = ratio<1.0? ratio: 1.0;
    if(unif[2*i+1] < ratio)
      X = Y;
  }
  for(int i=999; i<(n+999); i++)
  {
    if(unif[2*i] < 0.5)
      Y = X + 1;
    else 
      Y = X - 1;
    ratio = exp(-Y*Y / (2*sigma*sigma)) / exp(-X*X / (2*sigma*sigma));
    ratio = ratio<1.0? ratio: 1;
    if(unif[2*i+1] < ratio)
      X = Y;
    result.push_back(X);
  }
  return result;
}

// [[Rcpp::export]]
NumericMatrix gibbs(double seed = 76543, int T=10, int M=1, double rho = 0.0,  double x=0.0, double y=0.0)
{
  NumericMatrix result(T, 2*M);
  NumericVector norm1 = normal_rand(seed, 2*T*M);
  double coefficient = sqrt(1 - rho*rho);
  for(int j=0;j<M;j++)
  {
    result(0,2*j) = x;
    result(0,2*j+1) = y;
    for(int i=1;i<T;i++)
    {
      result(i,2*j) = rho * result(i-1,2*j+1) + coefficient * norm1[j*M+i];
      result(i,2*j+1) = rho * result(i,2*j) + coefficient * norm1[M*T+j*M+i];
    }
    
  }
  return result;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//


