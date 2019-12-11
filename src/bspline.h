#ifndef HH_BSPLINE_HH
#define HH_BSPLINE_HH
#include <vector>
#include <Eigen/Dense>
/*! \file
  @brief Compute bsplines functions
*/
namespace bspline{
  /*!
	@brief	Find the knot span of the parametric point t.

	@note	This is NOT	Algorithm A2.1 from 'The NURBS BOOK' pg68
			as that algorithm only works for nonperiodic
			knot vectors, nonetheless the results should
			be EXACTLY the same if U is nonperiodic

	@param p Spline degree
	@param t Parametric point
	@param U Knot sequence
	@return	Knot span

  */
  unsigned int
  findspan
  (int p, double t, const std::vector<double> &U);

  /*!
  	@brief	Compute the functions of the basis in the parametric point t

  	@note	Algorithm A2.2 from 'The NURBS BOOK' pg70.

  	@param i (Input) Knot span (from findspan())
  	@param t (Input) Parametric point
  	@param p (Input) Spline degree
  	@param U (Input) Knot sequence
  	@param N (Output) Vector of the functions of the basis (p+1 dimensional)
  */
  void
  basisfun
  (unsigned int i, double t, int p, const std::vector<double> &U, Eigen::ArrayXd &N);
}

#endif
