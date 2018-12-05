#include "BlockSim.h"

/* set dim of array */

void SetDim2(SEXP array, int x1, int x2)
{
  SEXP _dim;
  PROTECT(_dim = NEW_INTEGER(2));
  INTEGER_POINTER(_dim)[0] = x1;
  INTEGER_POINTER(_dim)[1] = x2;
  SET_DIM(array, _dim);
  UNPROTECT_PTR(_dim);
}

