// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// str2strvec
std::vector<std::string> str2strvec(std::string str, int len);
RcppExport SEXP _wameicheckr_str2strvec(SEXP strSEXP, SEXP lenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type str(strSEXP);
    Rcpp::traits::input_parameter< int >::type len(lenSEXP);
    rcpp_result_gen = Rcpp::wrap(str2strvec(str, len));
    return rcpp_result_gen;
END_RCPP
}
// editdist
int editdist(std::string s1, std::string s2, int len);
RcppExport SEXP _wameicheckr_editdist(SEXP s1SEXP, SEXP s2SEXP, SEXP lenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s1(s1SEXP);
    Rcpp::traits::input_parameter< std::string >::type s2(s2SEXP);
    Rcpp::traits::input_parameter< int >::type len(lenSEXP);
    rcpp_result_gen = Rcpp::wrap(editdist(s1, s2, len));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_wameicheckr_str2strvec", (DL_FUNC) &_wameicheckr_str2strvec, 2},
    {"_wameicheckr_editdist", (DL_FUNC) &_wameicheckr_editdist, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_wameicheckr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}