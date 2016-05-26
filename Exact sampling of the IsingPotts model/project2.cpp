#include <Rcpp.h>
#include<time.h>
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


class Ising
{
private:
  int positionA[64][64];
  int positionB[64][64];
  double beta;
  double step;
  
public:
  Ising(double _beta);
  Ising(double _beta, int _rand);
  void update();
  int sumA();
  int sumB();
  bool equal();
  double printStep();
  double return_h();
};

Ising::Ising(double _beta)
{
  beta = _beta;
  for (int i = 0; i < 64; i++)
    for (int j = 0; j < 64; j++)
      positionA[i][j] = 1;
  for (int i = 0; i < 64; i++)
    for (int j = 0; j < 64; j++)
      positionB[i][j] = 0;
  step = 0;
  srand((unsigned)time(NULL));
}
Ising::Ising(double _beta, int _rand)
{
  beta = _beta;
  for (int i = 0; i < 64; i++)
    for (int j = 0; j < 64; j++)
      positionA[i][j] = 1;
  for (int i = 0; i < 64; i++)
    for (int j = 0; j < 64; j++)
      positionB[i][j] = 0;
  step = 0;
  srand(_rand);
}
void Ising::update()
{
  for (int row = 0; row < 64; row++)
  {
    for (int column = 0; column < 64; column++)
    {
      double r = 1.0 * rand() / (1.0*RAND_MAX);
      
      int sumA1, sumA0;
      sumA1 = (1 == positionA[(row + 63) % 64][column]) + (1 == positionA[(row + 1) % 64][column]) +
        (1 == positionA[row][(column + 63) % 64]) + (1 == positionA[row][(column + 1) % 64]);
      sumA0 = 4 - sumA1;
      double pA0;
      pA0 = exp(beta*sumA0) / (exp(beta*sumA0) + exp(beta*sumA1));
      if (r < pA0)
        positionA[row][column] = 0;
      else
        positionA[row][column] = 1;
      
      int sumB1, sumB0;
      sumB1 = (1 == positionB[(row + 63) % 64][column]) + (1 == positionB[(row + 1) % 64][column]) +
        (1 == positionB[row][(column + 63) % 64]) + (1 == positionB[row][(column + 1) % 64]);
      sumB0 = 4 - sumB1;
      double pB0;
      pB0 = exp(beta*sumB0) / (exp(beta*sumB0) + exp(beta*sumB1));
      if (r < pB0)
        positionB[row][column] = 0;
      else
        positionB[row][column] = 1;
    }
  }
  step++;
}
int Ising::sumA()
{
  int sum = 0;
  for (int i = 0; i < 64; i++)
    for (int j = 0; j < 64; j++)
      sum += positionA[i][j];
  return sum;
}
int Ising::sumB()
{
  int sum = 0;
  for (int i = 0; i < 64; i++)
    for (int j = 0; j < 64; j++)
      sum += positionB[i][j];
  return sum;
}
bool Ising::equal()
{
  for (int i = 0; i < 64; i++)
  {
    for (int j = 0; j < 64; j++)
    {
      if (positionA[i][j] != positionB[i][j])
        return false;
    }
  }
  return true;
}
double Ising::printStep()
{
  return step;
}
double Ising::return_h()
{
  double result = 0;
  for (int i = 0; i < 64; i++)
  {
    for (int j = 0; j < 64; j++)
    {
      if (positionA[i][j] != positionA[(i + 1) % 64][j])
        result++;
      if (positionA[i][j] != positionA[i][(j + 1) % 64])
        result++;
    }
  }
  return result / (8192);
}




// [[Rcpp::export]]
DataFrame ChainState(double beta, int iterations = 10000)
{
  std::vector<double> A, B, step;
  Ising chain = Ising(beta);
  
  A.push_back(chain.sumA());
  B.push_back(chain.sumB());
  step.push_back(chain.printStep());
  long count=0;
  while(count < iterations)
  {
    chain.update();
    A.push_back(chain.sumA());
    B.push_back(chain.sumB());
    step.push_back(chain.printStep());
    count++;
  }
  DataFrame result = DataFrame::create(Named("step")=step, Named("X1")=A, Named("X2")=B);
  return result;
}


// [[Rcpp::export]]
DataFrame BetaTao(NumericVector beta)
{
  
  std::vector<double> tao;
  int count;
  
  for(int i=0; i<beta.size(); i++)
  {
    count=0;
    Ising chain =Ising(beta[i]);
    while(!chain.equal())
    {
      chain.update();
      count++;
    }
    tao.push_back(count);
  }
  DataFrame result = DataFrame::create(Named("beta")=beta, Named("tao")=tao);
  return result;
}

// [[Rcpp::export]]
NumericVector findSweep(double beta, int random, int itera)
{
  NumericVector result = NumericVector(2);
  long count = 0;
  Ising chain = Ising(beta, random*(unsigned)time(NULL));
  while(!chain.equal() || count <=itera)
  {
    chain.update();
    count++;
  }
  result[0] = count;
  result[1] = chain.return_h();
  return result;
}

