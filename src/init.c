#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP blackbox_CcovFocal(SEXP, SEXP);
extern SEXP blackbox_deleteCSmooth();
extern SEXP blackbox_flushCSmoothTable();
extern SEXP blackbox_GCV_lamVar_covFix_Wrapper(SEXP, SEXP, SEXP);
extern SEXP blackbox_getFnEvalCount();
extern SEXP blackbox_Krig_coef_Wrapper(SEXP, SEXP);
extern SEXP blackbox_newCSmooth(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"blackbox_CcovFocal",                 (DL_FUNC) &blackbox_CcovFocal,                 2},
  {"blackbox_deleteCSmooth",             (DL_FUNC) &blackbox_deleteCSmooth,             0},
  {"blackbox_flushCSmoothTable",         (DL_FUNC) &blackbox_flushCSmoothTable,         0},
  {"blackbox_GCV_lamVar_covFix_Wrapper", (DL_FUNC) &blackbox_GCV_lamVar_covFix_Wrapper, 3},
  {"blackbox_getFnEvalCount",            (DL_FUNC) &blackbox_getFnEvalCount,            0},
  {"blackbox_Krig_coef_Wrapper",         (DL_FUNC) &blackbox_Krig_coef_Wrapper,         2},
  {"blackbox_newCSmooth",                (DL_FUNC) &blackbox_newCSmooth,                7},
  {NULL, NULL, 0}
};

void R_init_blackbox(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
