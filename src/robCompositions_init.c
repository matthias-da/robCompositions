#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
 Check these declarations against the C/Fortran source code.
 */

/* .C calls */
extern void da(void *, void *, void *, void *, void *, void *);

/* .Call calls */
extern SEXP smoothingSplines_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP smoothingSplinesValidation_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
  {"da", (DL_FUNC) &da, 6},
  {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
  {"smoothingSplines_",           (DL_FUNC) &smoothingSplines_,           11},
  {"smoothingSplinesValidation_", (DL_FUNC) &smoothingSplinesValidation_,  9},
  {NULL, NULL, 0}
};

void R_init_robCompositions(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}