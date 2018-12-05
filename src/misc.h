#include <R.h>
#include <Rdefines.h>

/* initialize the list */

template <class T>
inline void SetValues(SEXP r, T *c, T v)
{
  for (int i = 0; i < length(r); i++)
    c[i] = v;
};

/* set dim of array */

void SetDim2(SEXP array, int x1, int x2);
