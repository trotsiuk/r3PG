#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>

void F77_NAME(s_3PG_f)(double *siteInputs, double *speciesInputs, double *forcingInputs,
    double *parameterInputs, double *biasInputs, int *n_sp, int *n_m, int *settings, 
    double *output);

extern SEXP s_3PG_c(SEXP siteInputs, SEXP speciesInputs, SEXP forcingInputs, SEXP parameterInputs,
    SEXP biasInputs, SEXP n_sp, SEXP n_m, SEXP settings){

    int n;

    const int n_m_c = INTEGER(n_m)[0];
    const int n_sp_c = INTEGER(n_sp)[0];
    

    n = n_m_c * n_sp_c * 11 * 15;

    SEXP output = PROTECT( allocVector(REALSXP, n) );
    SEXP dims = PROTECT( allocVector(INTSXP, 4) );

    INTEGER(dims)[0] = n_m_c;
    INTEGER(dims)[1] = n_sp_c;
    INTEGER(dims)[2] = 11;
    INTEGER(dims)[3] = 15;

    setAttrib( output, R_DimSymbol, dims);

    F77_CALL(s_3PG_f)(REAL(siteInputs), REAL(speciesInputs), REAL(forcingInputs), REAL(parameterInputs),
        REAL(biasInputs), INTEGER(n_sp), INTEGER(n_m), INTEGER(settings), REAL(output));

    UNPROTECT(2);

    return output;
}

static const R_CallMethodDef CallEntries[] = {
  {"s_3PG_c",   (DL_FUNC) &s_3PG_c,   8},
  {NULL,         NULL,                0}
};

void R_init_r3PGmix(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("r3PGmix", "s_3PG_c",  (DL_FUNC) &s_3PG_c);
}
