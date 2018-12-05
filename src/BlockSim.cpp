#include "BlockSim.h"

const R_CallMethodDef callMethods[] = {
  {"BS_ShortestDistances", (DL_FUNC) &BS_ShortestDistances, 3},
  {NULL, NULL, 0}
};

void R_init_BlockSim(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
