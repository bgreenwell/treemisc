// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// proximity_cpp
NumericMatrix proximity_cpp(IntegerMatrix x);
RcppExport SEXP _treemisc_proximity_cpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(proximity_cpp(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_treemisc_proximity_cpp", (DL_FUNC) &_treemisc_proximity_cpp, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_treemisc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}