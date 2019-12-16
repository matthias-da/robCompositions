#include "bspline.h"
#include <iostream>
#include <vector>
#include <cassert>
#include <Rcpp.h>

// Find the knot span of the parametric point t.
//
// INPUT:
//   p - spline degree
//   t - parametric point
//   U - knot sequence
//
// RETURN:
//
//   ret - knot span
//
// Note: This is NOT
// Algorithm A2.1 from 'The NURBS BOOK' pg68
// as that algorithm only works for nonperiodic
// knot vectors, nonetheless the results should
// be EXACTLY the same if U is nonperiodic


unsigned int
bspline::findspan
(int p, double t, const std::vector<double>& U)
// ret is the index of the last knot at the left of the point t
{
	unsigned int n = U.size();
	unsigned int ret = 0;
	if (t > U[U.size () - 1] || t < U[0])
	{
		Rcpp::Rcerr << "Value " << t
	            << " of t is outside the knot span by "
	            << U[U.size () - 1] - t << "\n";
			Rcpp::stop("Error in the C++ execution");
			// exit(EXIT_FAILURE);
	}
	else
	{
		while ((ret < n) && (U[ret] <= t))
		 	ret++;
	}
	if(ret>n-p-2) return n-p-2;
	return ret-1;
};	//findspan



void
bspline::basisfun
(unsigned int i, double t, int p, const std::vector<double>& U, Eigen::ArrayXd& N)
/*
Evaluates basis splines which include x=t in the span and save values in N
*/
{

	double saved, temp;

	// work space
	/* BERNHARD: 
	 * the following two lines results in (only in Linux, gcc) 
	 * Found the following significant warnings:
	 bspline.cpp:62:9: warning: ISO C++ forbids variable length array ‘left’ [-Wvla]
	 bspline.cpp:63:9: warning: ISO C++ forbids variable length array ‘right’ [-Wvla]
	 */
	/*double left[p+1];
	double right[p+1];*/ 
	
	double *left = (double*) calloc(p + 1, sizeof(double));
	double *right = (double*) calloc(p + 1, sizeof(double));
	
	/* statt dessen probiert:
	 * (um dynamisch left und right zu erstellen?)
	 * Alles Errors

	double *left = new double[p+1];
	double *right = new double[p+1];
	left = Rcpp::IntegerVector(p+1);
	right = Rcpp::IntegerVector(p+1);

  Rcpp::NumericVector left(p+1);
  Rcpp::NumericVector right(p+1);
	 */

	if(i == p && t == U[i]){
		N[0] = 1.0;
	}
	else if(i == U.size() ){
		N[U.size()-p-2]= 1.0;
	}
	else{
		std::vector<double> P(p + 1,1.0);
		for (unsigned int j = 1; j <= p; j++) {
			left[j] = t - U[i + 1 - j];
			right[j] = U[i + j] - t;
			saved = 0.0;
			for (unsigned int r = 0; r < j; r++) {
				temp = P[r] / (right[r + 1] + left[j - r]);
				P[r] = saved + right[r + 1] * temp;
				saved = left[j - r] * temp;
			}
			P[j] = saved;
		}
		for (unsigned int k = 0; k <= p; k++) {
			N[i - p + k] = P[k];
		}
	}
	
	free(left);
	free(right);

};	//basisfun
