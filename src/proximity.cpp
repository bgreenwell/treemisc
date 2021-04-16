#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix proximity_cpp(IntegerMatrix x) {
  int nrow = x.nrow();
  NumericMatrix out(nrow, nrow);
  for (int i = 0; i < nrow; i++) {
    for (int j = i + 1; j < nrow; j++) {
      out(i, j) = sum(x(i, _) == x(j, _));
    }
  }
  return out / x.ncol();
}
