#include <Rcpp.h>
#include<cstdlib>
#include <time.h> 
#include <math.h>
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
NumericMatrix applet(NumericMatrix x, NumericMatrix y)
{
  NumericMatrix result = NumericMatrix(x.nrow(), 1);
  for(int i=0; i<x.nrow(); i++)
  {
    result(i, 0) = 0;
    for(int j=0; j<x.ncol(); j++)
    {
      result(i, 0) = result(i, 0) + x(i, j) * y(j, i);
    }
  }
  return result;
}



class SinglePath
{
public:
  SinglePath();
  SinglePath(double _epsilon);
  SinglePath(int _x, int _y, bool _stop, int _Length, double _probInv, int _arrive[][13], int _cor_x[], int _cor_y[]);
  void Simulate();
  int printX();
  int printY();
  double printProbInv();
  int printL();
  NumericMatrix printCor();

  
  int printX_50();
  int printY_50();
  bool printStop_50();
  int printLength_50();
  void printArrive_50(int _arrive[][13]);
  double printProbInv_50();
  void printCor_x_50(int _cor_x[]);
  void printCor_y_50(int _cor_y[]);
  
  
private:
  int x;
  int y;
  bool stop;
  int Length;
  int arrive[13][13];
  double probInv;
  double epsilon;
  void OneStep();
  
  int x_50;
  int y_50;
  bool stop_50;
  int Length_50;
  int arrive_50[13][13];
  double probInv_50;

  int cor_x[200];
  int cor_y[200];
  int cor_x_50[200];
  int cor_y_50[200];
  
};

SinglePath::SinglePath()
{
  x = 1;
  y = 1;
  stop = false;
  Length = 0;
  probInv = 1;
  for (int i = 0; i<13; i++)
  {
    for (int j = 0; j<13; j++)
    {
      if (i == 0 || j == 0 || i == 12 || j == 12)
        arrive[i][j] = 1;
      else
        arrive[i][j] = 0;
    }
  }
  epsilon = 0;
  for(int i=0;i<200;i++)
    cor_x[i] = 0;
  for(int i=0;i<200;i++)
    cor_y[i] = 0;
  
    
}
SinglePath::SinglePath(double _epsilon)
{
  x = 1;
  y = 1;
  stop = false;
  Length = 0;
  probInv = 1;
  for (int i = 0; i<13; i++)
  {
    for (int j = 0; j<13; j++)
    {
      if (i == 0 || j == 0 || i == 12 || j == 12)
        arrive[i][j] = 1;
      else
        arrive[i][j] = 0;
    }
  }
  epsilon = _epsilon;
  for(int i=0;i<200;i++)
    cor_x[i] = 0;
  for(int i=0;i<200;i++)
    cor_y[i] = 0;
}
SinglePath::SinglePath(int _x, int _y, bool _stop, int _Length, double _probInv, int _arrive[][13], int _cor_x[], int _cor_y[])
{
  x = _x;
  y = _y;
  stop = _stop;
  Length = _Length;
  probInv = _probInv;
  for (int i = 0; i<13; i++)
  {
    for (int j = 0; j<13; j++)
    {
      if (i == 0 || j == 0 || i == 12 || j == 12)
        arrive[i][j] = 1;
      else
        arrive[i][j] = _arrive[i][j];
    }
  }
  epsilon = 0;
  for(int i=0;i<200;i++)
    cor_x[i] = _cor_x[i];
  for(int i=0;i<200;i++)
    cor_y[i] = _cor_y[i];
}
void SinglePath::OneStep()
{
  if (stop)
    return;
  bool direction[] = { arrive[x][y + 1] == 0, arrive[x][y - 1] == 0, arrive[x - 1][y] == 0, arrive[x + 1][y] == 0 };//up down left right
  int count = direction[0] + direction[1] + direction[2] + direction[3];
  if (count == 0)
  {
    stop = true;
    return;
  }
  
  if (rand() * 1.0 / RAND_MAX < epsilon)
  {
    stop = true;
    return;
  }
  else
  {
    Length++;
    probInv = probInv * count / (1 - epsilon);
  }
  
  int direct = rand() % count;
  int index = -1;
  int value = -1;
  while (value != direct)
  {
    index++;
    value += direction[index];
  }
  arrive[x][y] = 1;
  switch (index) {
  case 0: y = y + 1; break;
  case 1: y = y - 1; break;
  case 2: x = x - 1; break;
  case 3: x = x + 1; break;
  }
  cor_x[Length] = x;
  cor_y[Length] = y;
  
}
void SinglePath::Simulate()
{
  while (!stop && Length < 50)
    OneStep();
  if (Length == 50 && !stop)
  {
    x_50 = x;
    y_50 = y;
    stop_50 = stop;
    Length_50 = Length;
    for (int i = 0; i<13; i++)
    {
      for (int j = 0; j<13; j++)
      {
        if (i == 0 || j == 0 || i == 12 || j == 12)
          arrive_50[i][j] = 1;
        else
          arrive_50[i][j] = arrive[i][j];
      }
    }
    probInv_50 = probInv;
    for(int i=0;i<200;i++)
      cor_x_50[i] = cor_x[i];
    for(int i=0;i<200;i++)
      cor_y_50[i] = cor_y[i];
  }
  while (!stop)
    OneStep();
}
int SinglePath::printX()
{
  return x - 1;
}
int SinglePath::printY()
{
  return y - 1;
}
double SinglePath::printProbInv()
{    
  return probInv;
}
int SinglePath::printL()
{
  return Length;
}
int SinglePath::printX_50()
{
  return x_50;
}
int SinglePath::printY_50()
{
  return y_50;
}
bool SinglePath::printStop_50()
{
  return stop_50;
}
int SinglePath::printLength_50()
{
  return Length_50;
}
void SinglePath::printArrive_50(int _arrive[][13])
{
  for (int i = 0; i<13; i++)
  {
    for (int j = 0; j<13; j++)
    {
      if (i == 0 || j == 0 || i == 12 || j == 12)
        arrive[i][j] = 1;
      else
        _arrive[i][j] = arrive_50[i][j];
    }
  }
}
double SinglePath::printProbInv_50()
{
  return probInv_50;
}
NumericMatrix SinglePath::printCor()
{
  NumericMatrix result = NumericMatrix(2, 200);
  for(int i=0; i<200; i++)
  {
    if(i==0 || cor_x[i]!=0 || cor_y[i]!=0)
    {
      result(1,i) = cor_x[i];
      result(2,i) = cor_y[i];
    }
  }
  return result;
}
void SinglePath::printCor_x_50(int _cor_x[])
{
  for(int i=0;i<200;i++)
    _cor_x[i] = cor_x_50[i];
}
void SinglePath::printCor_y_50(int _cor_y[])
{
  for(int i=0;i<200;i++)
    _cor_y[i] = cor_y_50[i];
}




// [[Rcpp::export]]
NumericMatrix method1_plot()
{
  NumericMatrix result = NumericMatrix(2, 15);
  SinglePath ex;
  long M;
  double sum;
  srand(time(NULL));
  for(int j=0;j<16;j++)
  {
    sum = 0;
    M = (long)pow(10,(j+1)/2.0);
    for(long i=0; i<M; i++)
    {
      ex = SinglePath();
      ex.Simulate();
      sum += ex.printProbInv();
    }
    result(0,j) = M;
    result(1,j) = sum / M;
  }
  return result;
}

// [[Rcpp::export]]
NumericMatrix method2_plot()
{
  NumericMatrix result = NumericMatrix(2, 15);
  SinglePath ex;
  double M;
  double sum;
  srand(time(NULL));
  for(int j=0;j<16;j++)
  {
    sum = 0;
    M = (long)pow(10,(j+1)/2.0);
    for(long i=0; i<M; i++)
    {
      ex = SinglePath(0.1);
      ex.Simulate();
      sum += ex.printProbInv();
    }
    result(0,j) = M;
    result(1,j) = sum / M;
  }
  return result;
}


// [[Rcpp::export]]
NumericMatrix SAWtoEnd()
{
  int M = 1000000;
  NumericMatrix result = NumericMatrix(2, M);
  SinglePath ex;
  srand(time(NULL));
  int count = 0;
  double u = 0;
  while(count<M)
  {
    ex = SinglePath();
    ex.Simulate();
    u++;
    if(ex.printX() == 10 && ex.printY() == 10)
    {
      result(0, count) = ex.printProbInv();
      result(1, count) = u;
      u = 0;
      count++;
    }
  }
  return result;
}

// [[Rcpp::export]]
NumericMatrix method1_cor(long M)
{
  NumericMatrix result = NumericMatrix(2, 200);
  SinglePath ex;
  int max = 0;
  srand(time(NULL));
  for(long i=0; i<M; i++)
  {
    ex = SinglePath();
    ex.Simulate();
    if(ex.printL() > max)
    {
      result = ex.printCor();
      max = ex.printL();
    }
  }
  return result;
}




// [[Rcpp::export]]
NumericMatrix SAW(long M)
{
  NumericMatrix result = NumericMatrix(4, M);
  SinglePath ex;
  srand(time(NULL));
  for(long i=0; i<M; i++)
  {
    ex = SinglePath();
    ex.Simulate();
    result(0, i) = ex.printX();
    result(1, i) = ex.printY();
    result(2, i) = ex.printProbInv();
    result(3, i) = ex.printL();
  }
  return result;
}

// [[Rcpp::export]]
NumericMatrix SAW2(long M)
{
  NumericMatrix result = NumericMatrix(4, M);
  SinglePath ex;
  srand(time(NULL));
  for(long i=0; i<M; i++)
  {
    ex = SinglePath(0.1);
    ex.Simulate();
    result(0, i) = ex.printX();
    result(1, i) = ex.printY();
    result(2, i) = ex.printProbInv();
    result(3, i) = ex.printL();
  }
  return result;
}


// [[Rcpp::export]]
NumericMatrix SAW3(long M)
{
  NumericMatrix result = NumericMatrix(2, M);
  SinglePath ex;
  srand(time(NULL));
  int x_50;
  int y_50;
  bool stop_50;
  int Length_50;
  double probInv_50;
  int arrive_50[13][13];
  double sumProbInv;
  double sumL;
  int cor_x_50[200];
  int cor_y_50[200];
  
  for(long i=0; i<M; i++)
  {
    ex = SinglePath();
    ex.Simulate();
    if(ex.printL() > 50)
    {
      x_50 = ex.printX_50();
      y_50 = ex.printY_50();
      stop_50 = ex.printStop_50();
      Length_50 = ex.printLength_50();
      probInv_50 = ex.printProbInv_50();
      ex.printArrive_50(arrive_50);
      sumProbInv = 0;
      sumL = 0;
      ex.printCor_x_50(cor_x_50);
      ex.printCor_y_50(cor_x_50);
      for(int j=0; j<5; j++)
      {
        ex = SinglePath(x_50, y_50, stop_50, Length_50, probInv_50, arrive_50, cor_x_50, cor_y_50);
        ex.Simulate();
        sumProbInv += ex.printProbInv() * 0.2;
        sumL += ex.printL() * 0.2;
      }
      result(0,i) = sumProbInv;
      result(1,i) = sumL;
    }
    else
    {
      result(0,i) = ex.printProbInv();
      result(1,i) = ex.printL();
    }
  }
  return result;
}














