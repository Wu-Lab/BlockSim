#include "BlockSim.h"

const R_CallMethodDef callMethods[] = {
  {"BS_ShortestDistances", (DL_FUNC) &BS_ShortestDistances, 3},
  {"BS_BlockTips", (DL_FUNC) &BS_BlockTips, 2},
  {NULL, NULL, 0}
};

void R_init_BlockSim(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
