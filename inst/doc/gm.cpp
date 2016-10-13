//Includes/namespaces
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' gmean
//' @description
//' This function calculates the geometric mean.
//' 
//' @param data a vector 
//' 
//' @details
//' \code{gm} calculates the geometric mean for all positive entries of a vector. It is implemented
//' in C++ and accounts for over- and underflows.
//' @export
//' @author Matthias Templ
//' @examples
//' gm(c(3,5,3,6,7))
// [[Rcpp::export]]
double gm(std::vector<double> const&data)
{
  const double too_large = 1.e64;
  const double too_small = 1.e-64;
  double sum_log = 0.0;
  double product = 1.0;
  for (int i=0; i < data.size(); i++) {
     if (data[i] <= 0)
       throw std::range_error("Positive values required");
  }
  for(auto x:data) { 
    product *= x;
    if(product > too_large || product < too_small) {
      sum_log+= std::log(product);
      product = 1;      
    }
  }
  return std::exp((sum_log + std::log(product))/data.size());
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
gm(c(42,44,46))
*/
