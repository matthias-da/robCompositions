#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <iostream>
#include <iterator>
#include <vector>
#include <Rcpp.h>
#include "bspline.h"
#include "sandia_rules.h"
#include "gauss_points_weights.h"
#include "density_estimation.h"


/************* parametersManager class ***************/

void
parametersManager::readKnots
(const double * inputKnots, const unsigned int & size)
{
  
  // read knots by copy
  knots.clear();
  for(std::size_t i=0; i < size; i++)
    knots.push_back(inputKnots[i]);
  
/*  int k = 0; */

  g = knots.size()-2;
  G = g+k+1;    /*G = g+k+1;*/
  u = knots.front();
  v = knots.back();
  /*G = g+k+1;  BERNHARD
 Woher kommt k? Ist das das eigentliche 
 Problem(?), weil folgende Warnings (nur in Linux gcc)
 density_estimation.h:25:7: warning: ‘<anonymous>.parametersManager::G’ is used uninitialized in this function [-Wuninitialized]
 density_estimation.h:25:7: warning: ‘*((void*)&<anonymous> +16)’ is used uninitialized in this function [-Wuninitialized]
 density_estimation.h:25:7: warning: ‘<anonymous>.parametersManager::u’ is used uninitialized in this function [-Wuninitialized]
 density_estimation.h:25:7: warning: ‘<anonymous>.parametersManager::v’ is used uninitialized in this function [-Wuninitialized]
   
   Probiert habe ich (error)
   Rcpp::NumericVector G = g+k+1;
   Rcpp::NumericVector u = knots.front();
   Rcpp::NumericVector v = knots.back();

   */
}

void
parametersManager::readXcp
(const double * inputXcp, const unsigned int & size, const int & cancel)
{
  // read xcp by copy, if specified skip column indexed by cancel (-1 means nothing to cancel)
  xcp.clear();
  for(std::size_t i=0; i < size; i++)
    if(i!=cancel) xcp.push_back(inputXcp[i]);

  n = xcp.size();
}

/************* densityEstimator class ***************/

void
densityEstimator::fill_C
(const std::vector<double>& cp)
{
  C.resize(n,G);
  Eigen::ArrayXd N;
  for (unsigned int i = 0; i < n; i++)
  {
    N = Eigen::ArrayXd::Constant(G, 0.0);
    int fs = bspline::findspan(k, cp[i], lambda);
    bspline::basisfun(fs, cp[i], k, lambda, N);
    C.row(i) = N;
  }
}

void
densityEstimator::fill_M
()
{
  M.resize(G-l,G-l);
  M.setZero();
  Eigen::ArrayXd N = Eigen::ArrayXd::Constant(G-l, 0.0);
  std::vector<double> x(n);
  std::vector<double> w(n);
  integral::grule(x, w);
  for (unsigned int ll = 0; ll < n; ++ll)
  {
    x[ll] = (v - u) / 2 * x[ll] + (v + u) / 2;
    w[ll] = (v - u) / 2 * w[ll];
  }

  int fs;
  for (unsigned int i = 0; i < n; ++i)
  {
    N.setZero();
    fs = bspline::findspan(k-l, x[i], lambda_der);
    bspline::basisfun(fs, x[i], k-l, lambda_der, N);
    for (unsigned int j = 0; j < G-l; ++j)
    {
      for (unsigned int y = 0; y < G-l; ++y)
        M(j, y) += w[i] * N(j) * N(y);
    }
  }
}

void
densityEstimator::fill_DK
()
{
  using Trip = Eigen::Triplet<double>;
  std::vector<Trip> temp;
  DK.resize(G,G);
//  DK.reserve(Eigen::VectorXi::Constant(G,2));
  temp.emplace_back(0,0,(double)(k+1)/(lambda[k+1] - lambda[0]));
  temp.emplace_back(0,G-1,-(double)(k+1)/(lambda[k+1] - lambda[0]));

  for (std::size_t i = 1; i < G; i++)
  {
    temp.emplace_back(i,i-1,-(double)(k+1)/(lambda[k+1+i] - lambda[i]));
    temp.emplace_back(i,i,(double)(k+1)/(lambda[k+1+i] - lambda[i]));
  }

  DK.setFromTriplets(temp.begin(),temp.end());
}

void
densityEstimator::fill_S
()
{
  using Trip = Eigen::Triplet<double>;
  std::vector<Trip> temp;
  // Compute the S_l matrix for the penalization term
  for (std::size_t j = l; j >= 1; j--)
  {
    temp.clear();
    Eigen::SparseMatrix<double> DL(G - j, G + 1 - j);
    /*
    There is a delay between the indexing of lambda in the reference paper
    [J. Machalova et al] and the indexing in the code. Precisely:
    index_code = index_ref + k
    This is why the following loop start from j instead of j - k.
    */
    for (std::size_t i = j; i <= (G - 1) ; i++)
    {
      temp.emplace_back(i-j,i-j,-(double)(k + 1 - j)/(lambda[i+k+1-j] - lambda[i]));
      temp.emplace_back(i-j,i-j+1,(double)(k + 1 - j)/(lambda[i+k+1-j] - lambda[i]));
    }
    DL.setFromTriplets(temp.begin(),temp.end());
    if( j == l )
    {
      S.resize(G - l, G + 1 - l);
      S = DL;
    }
    else
    {
      S = S*DL;
    }
  }
  S.makeCompressed();
}


void
densityEstimator::set_lambda
(const std::vector<double> & knots)
{
  lambda.clear();
  lambda.assign(k, knots[0]);
  lambda.insert(lambda.begin() + k, knots.begin(), knots.end());
  lambda.insert(lambda.end(), k ,knots.back());
}


void
densityEstimator::set_lambda_der
(const std::vector<double> & knots)
{
  lambda_der.clear();
  lambda_der.assign(k-l, knots[0]);
  lambda_der.insert(lambda_der.begin() + k-l, knots.begin(), knots.end());
  lambda_der.insert(lambda_der.end(), k-l ,knots.back());
}

void
densityEstimator::set_matrix
()
{
  //weights = Eigen::VectorXd::Constant(n,1.0);
  set_lambda(knots);
  set_lambda_der(knots);
  fill_C(xcp);
  fill_M();
  fill_DK();
  fill_S();
}

void
densityEstimator::set_weights(const Eigen::Block<Eigen::Map<Eigen::Matrix<double, -1, -1>, 0, Eigen::Stride<0, 0> >, 1, -1, false> & row)
{
  weights.resize(row.cols());
  for(std::size_t i=0; i < row.cols(); i++)
    weights(i) = row(i);
}

double
densityEstimator::eval_J
(const std::vector<double>& ycp)
{
  Eigen::VectorXd newycp(Eigen::VectorXd::Map(ycp.data(),ycp.size()));
  double eval = 0.0;

  eval = (DK*c).transpose() * S.transpose() * M * S * DK*c;
  eval += alpha*(newycp - C*DK*c).transpose()*weights.asDiagonal()*(newycp - C*DK*c);

  return eval;
}

void
densityEstimator::solve
(Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> bspline, const std::vector<double>& ycp)
{
  Eigen::VectorXd newycp(Eigen::VectorXd::Map(ycp.data(),ycp.size()));
  P.noalias() = 1.0 / alpha * (DK).transpose() * S.transpose() * M * S * (DK) +
                (C * DK).transpose() * weights.asDiagonal() * C * DK;
  p.noalias() = DK.transpose()* C.transpose() * weights.asDiagonal() * newycp;

  Eigen::FullPivHouseholderQR<Eigen::MatrixXd> solverQR;
  solverQR.compute(P);

  unsigned int dimKer = solverQR.dimensionOfKernel();
  double relative_error = 0.0;

  switch(dimKer)
  {
    case 0:
    {
      c = solverQR.solve(p);
      break;
    }
    case 1:
    /*
    we have to find a vector of the kernel and find minimal norm solution,
    in order to do this we exploit property of matrix Q in QR decomposition: last n-r column
    of Q are basis for Ker(P) where r=rank(P)
    */
    {
      c = solverQR.solve(p);
      Eigen::VectorXd kernel = solverQR.matrixQ().col(G-1);
      double scale = c.dot(kernel)/kernel.dot(kernel);
      c = c - scale*kernel;
      break;
    }
    case 2:
    {
      c = solverQR.solve(p);
      Eigen::VectorXd ker1= solverQR.matrixQ().col(G-1);
      Eigen::VectorXd ker2 = solverQR.matrixQ().col(G-2);
      double scale1 = c.dot(ker1 - ker2*(ker1.dot(ker2)/ker2.dot(ker2)))/(ker1.dot(ker1)
                          - ker1.dot(ker2)*ker1.dot(ker2)/ker2.dot(ker2));
      double scale2 = -ker1.dot(ker2)/ker2.dot(ker2)*scale1 + c.dot(ker2)/ker2.dot(ker2);
      c = c - scale1*ker1 - scale2*ker2;
      break;
    }
    default:   // case > 2
    {
      Rcpp::Rcout << "\n WARNING: kernel dimension of the problem exceeds 2," << '\n';
      Rcpp::Rcout << " using Andrey Tychonoff regularization.." << '\n';
      double tychlambda2 = 0.01;
      solverQR.compute(P.transpose()*P + tychlambda2*Eigen::MatrixXd::Identity(G,G));
      c = solverQR.solve(P.transpose()*p);
      break;
    }
  }

  relative_error = (P*c - p).norm() / p.norm(); // norm() is L2 norm

  if(relative_error > tol)
    Rcpp::Rcout << "\n WARNING: found least-square solution..." << std::endl;
  /*
    Least-square solution: we look for a solution in the col space projecting the b (in Ax=b)
  */

  bspline.noalias() = DK*c;
  return;
};

void
densityEstimator::print_all
() const
{
    Rcpp::Rcout << "MATRIX C:" << '\n' << C << std::endl;
    Rcpp::Rcout << "MATRIX M:" << '\n' << M << std::endl;
    Rcpp::Rcout << "MATRIX DK:" << '\n' << Eigen::MatrixXd(DK) << std::endl;
    Rcpp::Rcout << "MATRIX W:" << '\n' << Eigen::MatrixXd(weights.asDiagonal()) << std::endl;
    Rcpp::Rcout << "\n Matrix P: " << '\n' << P << '\n';
}

void
densityEstimator::print_sol
() const
{
  Rcpp::Rcout << "\n Matrix P: " << '\n' << P << '\n';
  Rcpp::Rcout << "SOLUTION c = P^(-)p:" << '\n' << c << '\n';
};
