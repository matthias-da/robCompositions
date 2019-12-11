#include <vector>
#include <cmath>
#include <algorithm>
#include <iostream>
#include <Rcpp.h>
#include "gauss_points_weights.h"
// constexpr double pi() { return std::atan(1)*4; } //only GCC defines the mathfunctions like atan as constexpr.

constexpr double pi() { return 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651e+00; }

std::vector<double> operator*(const std::vector<double>& v1, const std::vector<double>& v2){
  unsigned int n = v1.size();
  std::vector<double> res;
  for (size_t i = 0; i < n; i++) {
    res.push_back(v1[i]*v2[i]);
  }
  return res;
}; // pointwise product

std::vector<double> operator*(double k, const std::vector<double>& v){
  unsigned int n = v.size();
  std::vector<double> res;
  for (size_t i = 0; i < n; i++) {
    res.push_back(k*v[i]);
  }
  return res;
};

std::vector<double> operator-(std::vector<double> v1, const std::vector<double>& v2){
  unsigned int n = v1.size();
  std::vector<double> res;
  for (size_t i = 0; i < n; i++) {
    res.push_back(v1[i]-v2[i]);
  }
  return res;
};

std::vector<int> operator-(std::vector<int> v1, const std::vector<int>& v2){
  unsigned int n = v1.size();
  std::vector<int> res;
  for (size_t i = 0; i < n; i++) {
    res.push_back(v1[i]-v2[i]);
  }
  return res;
};

std::vector<double> operator/(std::vector<double> v, double k){
  unsigned int n = v.size();
  std::vector<double> res;
  for (size_t i = 0; i < n; i++) {
    res.push_back(v[i]/k);
  }
  return res;
};
std::vector<double> operator/(std::vector<double> v1, std::vector<double> v2){
  unsigned int n = v1.size();
  std::vector<double> res;
  for (size_t i = 0; i < n; i++) {
    res.push_back(v1[i]/v2[i]);
  }
  return res;
};
std::vector<double> operator+(std::vector<double> v1, const std::vector<double>& v2){
  unsigned int n = v1.size();
  std::vector<double> res;
  for (size_t i = 0; i < n; i++) {
    res.push_back(v1[i]+v2[i]);
  }
  return res;
};

std::vector<double> as_vec(double c, unsigned int dim){
  std::vector<double> v(dim,c);
  return v;
}
std::vector<int> as_vec(int c, unsigned int dim){
  std::vector<int> v(dim,c);
  return v;
}
void integral::grule(std::vector<double> &bp, std::vector<double> &wf) {

   /* [bp,wf]=grule(n)
      This function computes Gauss base points and weight factors
      using the algorithm given by Davis and Rabinowitz in 'Methods
      of Numerical Integration', page 365, Academic Press, 1975.
      https://sourceforge.net/p/octave/integration/ci/default/tree/inst/grule.m
   */
  unsigned int n = bp.size();
  if(wf.size()!= n){
    // gestisci errore
    Rcpp::stop("Error in the C++ execution");
    // exit(EXIT_FAILURE);
  }
  int iter=2;
 unsigned int m=std::trunc(double(n+1)/2); //Example: -2.3->-2; 2.3 -> 2
  int e1=n*(n+1);
  int mm=4*m-1;
    std::vector<double> t;
  for (auto i = 3; i < mm + 1; i=i+4) {
    double k= i* pi()/(4*n+2);
    t.push_back(k);
  };

  double nn = double(1-(1-1.0/n)/(8*n*n));
  std::vector<double> xo(t.size());
  auto f=[nn](double x){return std::cos(x)*nn;};
  std::transform(t.begin(), t.end(), xo.begin(), f);

  std::vector<double> h;
  std::vector<double> d1;
  std::vector<double> pk;
  std::vector<double> dpn;
  std::vector<double> d2pn;
  std::vector<double> d3pn;
  std::vector<double> d4pn;
  std::vector<double> pkm1;
  std::vector<double> pkp1;
  std::vector<double> t1;
  std::vector<double> den;
  std::vector<double> u;
  std::vector<double> v;
  std::vector<double> p;
  std::vector<double> dp;
  for( int j=0; j<iter; j++){
     unsigned int xo_size = xo.size();
     pkm1 = as_vec(1.0, xo_size);
     pk=xo;
     for (unsigned int k=2; k< n+1; k++){
        t1 = xo*pk; //pointwise product
        pkp1 = t1-pkm1-(t1-pkm1)/k+t1;
        pkm1=pk;
        pk=pkp1;
     }
     den = as_vec(1.0, xo_size) - (xo*xo);
     d1=as_vec(double(n), xo_size)*(pkm1-xo*pk);
     dpn=d1/den;
     d2pn=(as_vec(2.0, xo_size)*xo*dpn-as_vec(double(e1), xo_size)*pk)/den;
     d3pn=(as_vec(4.0, xo_size)*xo*d2pn+as_vec(double(2-e1), xo_size)*dpn)/den;
     d4pn=(as_vec(6.0, xo_size)*xo*d3pn+as_vec(double(6-e1), xo_size)*d2pn)/den;
     u=pk/dpn;
     v=d2pn/dpn;
     h= as_vec(0.0, xo_size)-u*(as_vec(1.0, xo_size)+(as_vec(5.0, xo_size)*u)*(v+u*(v*v-u*d3pn/(as_vec(3.0, xo_size)*dpn))));
     p= pk+h*(dpn+(as_vec(5.0, xo_size)*h)*(d2pn+(h/as_vec(3.0, xo_size)*(d3pn+as_vec(25.0, xo_size)*h*d4pn))));
     dp=dpn+h*(d2pn+(as_vec(0.5, xo_size)*h)*(d3pn+h*d4pn/as_vec(3.0, xo_size)));
     h=h-p/dp;
     xo=xo+h;
  }

  unsigned int xo_size = xo.size();
  std::vector<double> bp_temp= as_vec(0.0, xo_size)-xo-h;
  std::vector<double> fx=d1-h*as_vec(double(e1), xo_size)*(pk+(h/as_vec(2.0, xo_size))*(dpn+(h/as_vec(3.0, xo_size))*(d2pn+(h/as_vec(4.0, xo_size))*(d3pn+(as_vec(0.2, xo_size)*h)*d4pn))));
  std::vector<double> wf_temp=as_vec(2.0, xo_size)*(as_vec(1.0, xo_size)-bp_temp*bp_temp)/(fx*fx);
  if ( (m+m) > n ){
  	bp_temp[m-1]=0;
  }
  if ( !((m+m) == n) ){
  	m=m-1;
  }
  std::vector<int> jj;
  for (unsigned int i = 1; i <= m; i++) {
    jj.push_back(i);
  };

  std::vector<int> n1j= as_vec(int(n+1), jj.size())-jj;
  for (unsigned int i=0; i < jj.size(); i++) {
    bp[i]= bp_temp[i];
    wf[i]= wf_temp[i];
    bp[n1j[i]-1]=-bp_temp[i];
    wf[n1j[i]-1]=wf_temp[i];
  };

// % Linear map from[-1,1] to [a,b]
// x=(a*(1-y)+b*(1+y))/2;
//
// % Compute the weights
// w=(b-a)./((1-y.^2).*Lp.^2)*(N2/N1)^2;

};
