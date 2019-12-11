#include <math.h>
#include <Eigen/Dense>
#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>
#include <functional>
#include <Rcpp.h>
#include "zeros.h"
#include "density_estimation.h"
#include "classData.h"

void
dataManager::readData
(const Eigen::Block<Eigen::Map<Eigen::Matrix<double, -1, -1>,0, Eigen::Stride<0, 0> >, 1, -1, false> & row,
       PRIOR prior, const int & cancel)
{
  numbers.clear();

  if( (row.array()!=0.0).any() )
    BM(numbers, row, prior);
  else
  {
    for(unsigned int i = 0, n = row.size(); i < n; i++)
      numbers.push_back(row(i));
  }

  if(cancel!=-1) numbers.erase(numbers.begin() + cancel);

  howmanyclasses = numbers.size();
};

void
dataManager::transfData
()
{
  // clr transformation of prop_data in transf_data
  double a = 0.0;

  // computing geometric mean
  for (const auto& y:numbers)
    a += log(y);

  // clr transformation
  for (auto& y:numbers)
    y = log(y) - a/howmanyclasses;
};

std::vector<double>
dataManager::getNumbers
()
{
  return numbers;
}

void
dataManager::pacs
(densityEstimator & dens, Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> bspline)
{
  dens.solve(bspline,numbers);
};

void
dataManager::antitData
(Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> x)
{
  // Using trapezoidal integration in continuous setting
  double len = (grid.back() - grid.front())/(grid.size()-1);
  unsigned int x_len = x.size();
  double den = 0.5*exp(x(0))*len + 0.5*exp(x(x_len-1))*len;
  for(int i=1; i<x_len-1;i++){
    den += exp(x(i))*len;
  }

  for(int i=0; i<x.size();i++){
    x(i) = exp(x(i))/den;
  }
};

void
dataManager::fillGrid
(double start, double end, unsigned int numPoints)
{
  double step = (end - start)/numPoints;
  grid.resize(numPoints);
  grid[0] = start;
  for (unsigned int i = 1; i < numPoints-1 ; ++i) {
    grid[i] = grid[i-1] + step;
  }
  grid[numPoints-1] = end;
};

void
dataManager::plotData
(const densityEstimator & dens, unsigned long int numPoints,
  Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> bspline,
  Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> yplot)
{

  double start = dens.get_u();
  double end = dens.get_v();
  unsigned int degree = dens.get_k();
  unsigned int G = dens.get_G();
  const std::vector<double> knots = dens.get_lambda();

  fillGrid(start, end, numPoints);

  Eigen::ArrayXd N;

  for (int i = 0; i < grid.size(); ++i)
  {
    int j = bspline::findspan(degree,grid[i],knots);
    N = Eigen::ArrayXd::Constant(G, 0.0);
    bspline::basisfun(j, grid[i], degree, knots, N);
    long double fvalue = compute_fvalue(bspline, N);
    yplot(i)=fvalue;
  }

  antitData(yplot);
  return;
};

void
dataManager::plotData_Clr
(const densityEstimator & dens, unsigned long int numPoints,
  Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> bspline,
  Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> yplot)
{

  double start = dens.get_u();
  double end = dens.get_v();
  unsigned int degree = dens.get_k();
  unsigned int G = dens.get_G();
  const std::vector<double> knots = dens.get_lambda();

  fillGrid(start, end, numPoints);

  Eigen::ArrayXd N;

  for (int i = 0; i < grid.size(); ++i) {
    int j = bspline::findspan(degree,grid[i],knots);
    N = Eigen::ArrayXd::Constant(G, 0.0);
    bspline::basisfun(j, grid[i], degree, knots, N);
    long double fvalue = compute_fvalue(bspline, N);
    yplot(i)=fvalue;
  }

  return;
};

long double
dataManager::compute_fvalue
(Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> vec1, Eigen::ArrayXd vec2 )
{
  long double res = 0.0;
  unsigned int n = vec1.size();
  if(vec2.size() != n){
    Rcpp::Rcerr << "Error in compute_fvalue function. Check dimensions of the vectors.."
              << std::endl;
    Rcpp::stop("Error in the C++ execution");
    // exit(1);
  }
  for (int i = 0; i < n; ++i) {
    res += vec1[i]*vec2[i];
  }
  return res;
};
