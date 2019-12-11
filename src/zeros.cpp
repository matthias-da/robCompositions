#include <math.h>
#include <Eigen/Dense>
#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>
#include <functional>
#include <numeric>
#include "zeros.h"

std::vector<double>
help::divide
(const std::vector<double> & vect, const double & D)
{
  assert(D>0 && " Error: dividing by zero..");
  std::vector<double> out;
  for(auto it:vect) out.push_back(it/D);
  return out;
};

double
help::sum
(const std::vector<double> & vect)
{
  return std::accumulate(vect.begin(),vect.end(),0.0);
};

std::vector<double>
help::uniform
(const unsigned int & n)
{
  std::vector<double> tmp;
  tmp.insert(tmp.begin(),n,1.0/(double)(n));
  return tmp;
};

double
help::geom_mean
(const std::vector<double> & vect)
{
  double out = 1.0;
  for(const auto it:vect){
    out=out*it;
  }
  return pow(out, 1.0/vect.size());
};


void
BM
(std::vector<double> & numbers,
  const Eigen::Block<Eigen::Map<Eigen::Matrix<double, -1, -1>, 0, Eigen::Stride<0, 0> >, 1, -1, false> & data,
  PRIOR p)
{
  if(p == PRIOR::DEFAULT)
  {
    if( data.sum() > (double) (data.size()*data.size()) )  // n > D^2
        p = PRIOR::SQ;
    else
        p = PRIOR::BAYES_LAPLACE;
  }

  double s;

  switch(p){
    case PRIOR::PERKS :
    {
      s = 1;
      BM(numbers, data, s);
      break;
    }
    case PRIOR::JEFFREYS :
    {
      s = (double)(data.size())/2;
      BM(numbers, data, s);
      break;
    }
    case PRIOR::BAYES_LAPLACE :
    {
      s = (double)(data.size());
      BM(numbers, data, s);
      break;
    }
    case PRIOR::SQ :
    {
      s = sqrt(data.sum());
      BM(numbers, data, s);
      break;
    }
    default: {}
  }

  return;
};

void
BM
(std::vector<double> & numbers,
  const Eigen::Block<Eigen::Map<Eigen::Matrix<double, -1, -1>, 0, Eigen::Stride<0, 0> >, 1, -1, false> & data,
  const double & s, const bool is_strength_inverse)
{
  std::vector<double> t = help::uniform(data.size());

  assert(s>=0 && " Error (BM): strength must be >=0..");
  assert(data.size()==t.size() && " Error(BM): different sizes of input..");

  double n = data.sum();
  double tmp = 0.0;

  double t_tot = 0.0;

  for(std::size_t i = 0; i < data.size(); i++)
  {   //computing term of the summation
    assert(data[i]>=0 && " Error (BM): input must be >=0..");
    assert(t[i]>=0 && " Error (BM): input must be >=0..");

    if(data(i)==0)
      tmp += t[i];

    t_tot += t[i];
  }

  // assert()  NOTE: need to check that t_tot is "equal" to 1
  for(std::size_t i = 0; i < data.size(); i++)
  {  //applying BM method
    if(data[i] == 0)
    {
      if(is_strength_inverse == false)
        numbers.push_back(t[i]*s/(n + s));
      else
        numbers.push_back(t[i]/(s*n + 1));
    }
    else
    {
      if(is_strength_inverse == false)
        numbers.push_back(data[i]*(1 - s*tmp/(n+s))/n);
      else
        numbers.push_back(data[i]*(1 - tmp/(s*n+1))/n);
    }
  }
  return;
}
