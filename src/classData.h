#ifndef STORE_DATA_1909_HPP
#define STORE_DATA_1909_HPP

#include <math.h>
#include <Eigen/Dense>
#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>
#include <functional>
#include "density_estimation.h"
#include "zeros.h"
/*! \file
  @brief Data container class
*/

/*!
@brief Data manager
@note Data are assumed to be already without extreme observations
*/

class dataManager
{
private:
  /** vector where data are stored (one row at a time)*/
  std::vector<double> numbers;
  /** mesh grid - where to evaluate output density */
  std::vector<double> grid;
  /** numbers' size*/
  unsigned int howmanyclasses;

public:
  /*!
	@brief	Read one row of data
  @details Read one row and apply Bayesian-multiplicative treatment of count zeros if necessary.
  @see BM()
	@param row Input row of data
	@param prior Prior for BM treatment
  @see PRIOR
	@param cancel If cancel = j, it does not consider j-th column.
        Useful for cross-validation. Disabled by default.
  */
  void
  readData
  (const Eigen::Block<Eigen::Map<Eigen::Matrix<double, -1, -1>, 0, Eigen::Stride<0, 0> >, 1, -1, false> & row,
         PRIOR prior, const int & cancel = -1);

  /*!
	@brief	Transform data using centered log-ratio (clr) function
  @details Apply clr transformation to data:
            \f$ y_i = \frac{z_i}{g(z_1,...,z_n)} \f$
            where g() is the geometric mean, \f$z_i\f$ are the elements in
            the row of data and \f$y_i\f$ are the transformed elements.
  */
  void
  transfData
  ();

  /*!
	@brief	Return processed data row
  */
  std::vector<double>
  getNumbers
  ();

  /*!
  @brief It's the unique and final solution of the problem.
  @details Call the solve method of the densityEstimator object.
  @param dens (Input) densityEstimator object where parameters for the method are stored.
  @see densityEstimator
  @param bspline (Output) Row of output matrix where coefficients of the bspline are saved.
  */
  void
  pacs
  (densityEstimator & dens, Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> bspline);

  /*!
	@brief	Anti-transform data using the inverse of centered log-ratio (clr) function
  @details Apply \f$ clr^{-1} \f$ transformation:
            \f$ z_i = \frac{exp(y_i)}{n \int_{k=1}^{n}exp(y_k)} \f$
  @param x Vector to anti-transform
  */
  void
  antitData
  (Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> x);

  /*!
  @brief	Generate equispaced grid in [start,end]
  @param start Leftend point of the interval
  @param end Rightend point of the interval
  @param numPoints Number of points to generate in the interval
  */
  void
  fillGrid
  (double start, double end, unsigned int numPoints);

  /*!
  @brief	Generate points to plot
  @details Compute and store the anti-transformated values of the density in yplot matrix.
  @param numPoints (input) Number of points to plot
  @param bspline (input) Coefficients of bspline
  @param yplot (output) Anti-transformed values of the bspline evaluated in the the points generated for plot
  */
  void
  plotData
  (const densityEstimator & dens, unsigned long int numPoints,
    Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> bspline,
    Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> yplot);

  /*!
  @brief	Generate points to plot
  @details Compute and store the clr-values of the density in yplot matrix.
  @param numPoints (input) Number of points to plot
  @param bspline (input) Coefficients of bspline
  @param yplot (output) clr transformed values of the bspline evaluated in the the points generated for plot
  */
  void
  plotData_Clr
  (const densityEstimator & dens, unsigned long int numPoints,
    Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> bspline,
    Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> yplot);

  /*!
  @brief Compute the value of the bspline in a point
  @param vec1 Coefficients of the bspline
  @param vec2 basis splines evaluated in the point
  @return Value of the bspline evaluated in the point point
  */
  long double
  compute_fvalue
  (Eigen::Block<Eigen::Matrix<double, -1, -1>, 1, -1, false> vec1, Eigen::ArrayXd vec2);

};

#endif //STORE_DATA_1909_HPP
