#include <Rcpp.h>
#include<cstdlib>
#include <time.h> 
#include <math.h>
using namespace Rcpp;

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
  bool printStop();
  void printCor_x(int _cor_x[]);
  void printCor_y(int _cor_y[]);
  void printArrive(int _arrive[][13]);
  void OneStep();
  NumericMatrix printCor();
  
private:
  int x;
  int y;
  bool stop;
  int Length;
  int arrive[13][13];
  double probInv;
  double epsilon;
  
  int cor_x[200];
  int cor_y[200];
  
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
  for (int i = 1; i<200; i++)
    cor_x[i] = 0;
  for (int i = 1; i<200; i++)
    cor_y[i] = 0;
  cor_x[0] = 1;
  cor_y[0] = 1;
  
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
  for (int i = 1; i<200; i++)
    cor_x[i] = 0;
  for (int i = 1; i<200; i++)
    cor_y[i] = 0;
  cor_x[0] = 1;
  cor_y[0] = 1;
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
  for (int i = 0; i<200; i++)
    cor_x[i] = _cor_x[i];
  for (int i = 0; i<200; i++)
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
  Length++;
  probInv = probInv * count / (1 - epsilon);
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
bool SinglePath::printStop()
{
  return stop;
}
void SinglePath::printCor_x(int _cor_x[])
{
  for (int i = 0; i < 200; i++)
    _cor_x[i] = cor_x[i];
}
void SinglePath::printCor_y(int _cor_y[])
{
  for (int i = 0; i < 200; i++)
    _cor_y[i] = cor_y[i];
}
void SinglePath::printArrive(int _arrive[][13])
{
  for (int i = 0; i < 13; i++)
    for (int j = 0; j < 13; j++)
      _arrive[i][j] = arrive[i][j];
}
NumericMatrix SinglePath::printCor()
{
  NumericMatrix result = NumericMatrix(2, 200);
  for (int i = 0; i<200; i++)
  {
    if (i == 0 || cor_x[i] != 0 || cor_y[i] != 0)
    {
      result(0, i) = cor_x[i]-1;
      result(1, i) = cor_y[i]-1;
    }
    else
    {
      result(0, i) = 0;
      result(1, i) = 0;
    }
  }
  return result;
}


class SinglePath50 : public SinglePath
{
public:
  SinglePath50();
  SinglePath50(double _epsilon);
  void SimulateBefore50();
  
  int printX_50();
  int printY_50();
  double printProbInv_50();
  int printL_50();
  bool printStop_50();
  void printCor_x_50(int _cor_x[]);
  void printCor_y_50(int _cor_y[]);
  void printArrive_50(int _arrive[][13]);
  
  
private:
  int x_50;
  int y_50;
  bool stop_50;
  int Length_50;
  int arrive_50[13][13];
  double probInv_50;
  int cor_x_50[200];
  int cor_y_50[200];
  
};
SinglePath50::SinglePath50()
  :SinglePath()
{}
SinglePath50::SinglePath50(double _epsilon)
  : SinglePath(_epsilon)
{}
void SinglePath50::SimulateBefore50()
{
  while (!printStop() && printL() < 50)
    OneStep();
  if (printL() == 50 && !printStop())
  {
    x_50 = printX();
    y_50 = printY();
    stop_50 = printStop();
    Length_50 = printL();
    printArrive(arrive_50);
    probInv_50 = printProbInv();
    printCor_x(cor_x_50);
    printCor_y(cor_y_50);
  }
}
int SinglePath50::printX_50()
{
  return x_50;
}
int SinglePath50::printY_50()
{
  return y_50;
}
double SinglePath50::printProbInv_50()
{
  return probInv_50;
}
int SinglePath50::printL_50()
{
  return Length_50;
}
bool SinglePath50::printStop_50()
{
  return stop_50;
}
void SinglePath50::printCor_x_50(int _cor_x[])
{
  for (int i = 0; i < 200; i++)
    _cor_x[i] = cor_x_50[i];
}
void SinglePath50::printCor_y_50(int _cor_y[])
{
  for (int i = 0; i < 200; i++)
    _cor_y[i] = cor_y_50[i];
}
void SinglePath50::printArrive_50(int _arrive[][13])
{
  for (int i = 0; i < 13; i++)
    for (int j = 0; j < 13; j++)
      _arrive[i][j] = arrive_50[i][j];
}






// [[Rcpp::export]]
NumericMatrix SAW1(long M)
{
  NumericMatrix result = NumericMatrix(2, M);
  SinglePath ex;
  srand(time(NULL));
  for(long i=0; i<M; i++)
  {
    ex = SinglePath();
    ex.Simulate();
    result(0, i) = ex.printProbInv();
    result(1, i) = ex.printL();
  }
  return result;
}

// [[Rcpp::export]]
NumericMatrix method1_plot()
{
  NumericMatrix result = NumericMatrix(2, 16);
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
NumericMatrix method1_path(long M)
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
NumericMatrix SAW2(long M)
{
  NumericMatrix result = NumericMatrix(2, M);
  SinglePath ex;
  srand(time(NULL));
  for(long i=0; i<M; i++)
  {
    ex = SinglePath(0.05);
    ex.Simulate();
    result(0, i) = ex.printProbInv();
    result(1, i) = ex.printL();
  }
  return result;
}

// [[Rcpp::export]]
NumericMatrix method2_plot()
{
  NumericMatrix result = NumericMatrix(2, 16);
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
      ex = SinglePath(0.05);
      ex.Simulate();
      sum += ex.printProbInv();
    }
    result(0,j) = M;
    result(1,j) = sum / M;
  }
  return result;
}


// [[Rcpp::export]]
NumericMatrix method2_path(long M)
{
  NumericMatrix result = NumericMatrix(2, 200);
  SinglePath ex;
  int max = 0;
  srand(time(NULL));
  for(long i=0; i<M; i++)
  {
    ex = SinglePath(0.05);
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
NumericMatrix SAW3(long M)
{
  NumericMatrix result = NumericMatrix(2, M);
  SinglePath50 ex;
  SinglePath ex2;
  srand(time(NULL));
  int x_50;
  int y_50;
  bool stop_50;
  int Length_50;
  int arrive_50[13][13];
  double probInv_50;
  int cor_x_50[200];
  int cor_y_50[200];
  
  for(long i=0; i<M;)
  {
    ex = SinglePath50();
    ex.SimulateBefore50();
    ex.Simulate();
    result(0, i) = ex.printProbInv();
    result(1, i) = ex.printL();
    i++;
    if(ex.printL()>50)
    {
      x_50 = ex.printX_50();
      y_50 = ex.printY_50();
      stop_50 = ex.printStop_50();
      Length_50 = ex.printL_50();
      ex.printArrive_50(arrive_50);
      probInv_50 = ex.printProbInv_50();
      ex.printCor_x_50(cor_x_50);
      ex.printCor_y_50(cor_y_50);
      for(int j=0; j<5&&i< M ;j++)
      {
        ex2 = SinglePath(x_50+1, y_50+1, stop_50, Length_50, probInv_50, arrive_50, cor_x_50, cor_y_50);
        ex2.Simulate();
        result(0, i) = ex2.printProbInv() * 0.2;
        result(1, i) = ex2.printL();
        i++;
      }
    }
    
  }
  return result;
}

// [[Rcpp::export]]
NumericMatrix method3_plot()
{
  NumericMatrix result = NumericMatrix(2, 16);
  SinglePath50 ex;
  SinglePath ex2;
  srand(time(NULL));
  int x_50;
  int y_50;
  bool stop_50;
  int Length_50;
  int arrive_50[13][13];
  double probInv_50;
  int cor_x_50[200];
  int cor_y_50[200];
  
  long M;
  double sum;
  srand(time(NULL));
  for(int j=0;j<16;j++)
  {
    sum = 0;
    M = (long)pow(10,(j+1)/2.0);
    for(long i=0; i<M;)
    {
      ex = SinglePath50();
      ex.SimulateBefore50();
      ex.Simulate();
      sum += ex.printProbInv();
      i++;
      if(ex.printL()>50)
      {
        x_50 = ex.printX_50();
        y_50 = ex.printY_50();
        stop_50 = ex.printStop_50();
        Length_50 = ex.printL_50();
        ex.printArrive_50(arrive_50);
        probInv_50 = ex.printProbInv_50();
        ex.printCor_x_50(cor_x_50);
        ex.printCor_y_50(cor_y_50);
        for(int k=0; k<5&&i< M ;k++)
        {
          ex2 = SinglePath(x_50+1, y_50+1, stop_50, Length_50, probInv_50, arrive_50, cor_x_50, cor_y_50);
          ex2.Simulate();
          sum += ex2.printProbInv() * 0.2;
          i++;
        }
      }
    }
    result(0,j) = M;
    result(1,j) = sum / M;
  }
  return result;
}


// [[Rcpp::export]]
NumericMatrix method3_path(long M)
{
  NumericMatrix result = NumericMatrix(2, 200);
  SinglePath50 ex;
  SinglePath ex2;
  srand(time(NULL));
  int x_50;
  int y_50;
  bool stop_50;
  int Length_50;
  int arrive_50[13][13];
  double probInv_50;
  int cor_x_50[200];
  int cor_y_50[200];
  int max = 0;
  srand(time(NULL));
  for(long i=0; i<M; i++)
  {
    ex = SinglePath50();
    ex.SimulateBefore50();
    ex.Simulate();
    if(ex.printL() > max)
    {
      result = ex.printCor();
      max = ex.printL();
    }
    if(ex.printL()>50)
    {
      x_50 = ex.printX_50();
      y_50 = ex.printY_50();
      stop_50 = ex.printStop_50();
      Length_50 = ex.printL_50();
      ex.printArrive_50(arrive_50);
      probInv_50 = ex.printProbInv_50();
      ex.printCor_x_50(cor_x_50);
      ex.printCor_y_50(cor_y_50);
      for(int k=0; k<5&&i< M ;k++)
      {
        ex2 = SinglePath(x_50+1, y_50+1, stop_50, Length_50, probInv_50, arrive_50, cor_x_50, cor_y_50);
        ex2.Simulate();
        i++;
        if(ex2.printL() > max)
        {
          result = ex2.printCor();
          max = ex2.printL();
        }
      }
    }
  }
  return result;
}


// [[Rcpp::export]]
NumericMatrix SAWtoEnd1()
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
NumericMatrix SAWtoEnd2()
{
  int M = 1000000;
  NumericMatrix result = NumericMatrix(2, M);
  SinglePath ex;
  srand(time(NULL));
  int count = 0;
  double u = 0;
  while(count<M)
  {
    ex = SinglePath(0.05);
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
NumericMatrix SAWtoEnd3()
{
  int M = 1000000;
  NumericMatrix result = NumericMatrix(2, M);
  SinglePath50 ex;
  SinglePath ex2;
  srand(time(NULL));
  int count = 0;
  double u = 0;
  int x_50;
  int y_50;
  bool stop_50;
  int Length_50;
  int arrive_50[13][13];
  double probInv_50;
  int cor_x_50[200];
  int cor_y_50[200];
  
  while(count<M)
  {
    ex = SinglePath50();
    ex.SimulateBefore50();
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
  if(ex.printL()>50)
  {
    x_50 = ex.printX_50();
    y_50 = ex.printY_50();
    stop_50 = ex.printStop_50();
    Length_50 = ex.printL_50();
    ex.printArrive_50(arrive_50);
    probInv_50 = ex.printProbInv_50();
    ex.printCor_x_50(cor_x_50);
    ex.printCor_y_50(cor_y_50);
    for(int j=0; j<5 && count< M ;j++)
    {
      ex2 = SinglePath(x_50+1, y_50+1, stop_50, Length_50, probInv_50, arrive_50, cor_x_50, cor_y_50);
      ex2.Simulate();
      u++;
      if(ex2.printX() == 10 && ex2.printY() == 10)
      {
        result(0, count) = ex2.printProbInv() * 0.2;
        result(1, count) = u;
        u = 0;
        count++;
      }
    }
    
  }
  return result;
}

// [[Rcpp::export]]
NumericMatrix qaq (long M)
{
  NumericMatrix result = NumericMatrix(1, M);
  long count = 0;
  double u = 0;
  SinglePath ex = SinglePath();
  while(count < M)
  {
    u++;
    ex = SinglePath();
    while(!ex.printStop())
    {
      ex.OneStep();
      if(ex.printX()==10 && ex.printY()==10)
      {
        result(1, count) = ex.printProbInv() / u;
        u = 0;
        count++;
        break;
      }
    }
  }
  return result;
}

// [[Rcpp::export]]
double q2 (long M)
{
  double result = 0;
  long i=0;
  bool arrive = false;
  SinglePath ex = SinglePath();
  while(i < M)
  {
    i++;
    ex = SinglePath();
    while(!ex.printStop())
    {
      ex.OneStep();
      if(ex.printX()==10 && ex.printY()==10)
      {
        result += ex.printProbInv();
        break;
      }
    }
  }
  return result / M;
}

