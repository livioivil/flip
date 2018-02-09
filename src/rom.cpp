// [[Rcpp::depends(RcppArmadillo)]]
#define SGN(x) (((x)<0)?-1:1)
#include <RcppArmadillo.h>
//using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::mat rom(int n) {
  vec d(n);
  arma::mat A=eye(n,n);
  double s=0;
  double beta=0;
  int sgn=0;
  mat B;
  Rcpp::NumericVector x;
  d[n-1] = SGN(Rcpp::rnorm(1)[1]);
  for(int i = (n-1); i > 0; i--) {
    x=Rcpp::rnorm(n-i+1);
    arma::colvec y(x.begin(), x.size(), false);
    s = (sqrt(y.st()*y))[0];
    sgn = SGN(y[0]);
    s = sgn*s;
    d[i-1] = -sgn;
    y[0] = y[0] + s;
    beta = s*y[0];
    B = A(span((i-1),(n-1)),span(0,(n-1)));
    A(span((i-1),(n-1)),span(0,(n-1)))=B-y*((y.st()*B)/beta);
  }
  return diagmat(d)*A;
}
