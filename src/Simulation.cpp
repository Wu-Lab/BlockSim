#include "BlockSim.h"

SEXP BS_BlockTips(SEXP _VisableNodes, SEXP _Matrix)
{
  PROTECT(_VisableNodes = AS_INTEGER(_VisableNodes));
  int *VisableNodes = INTEGER_POINTER(_VisableNodes);
  PROTECT(_Matrix = AS_INTEGER(_Matrix));
  int *Matrix = INTEGER_POINTER(_Matrix);

  SEXP _nNodes;
  PROTECT(_nNodes = GET_DIM(_Matrix));
  int nNodes = INTEGER_POINTER(AS_INTEGER(_nNodes))[0];
  int nVisableNodes = length(_VisableNodes);

  int *tip_record = (int *) R_alloc(nVisableNodes, sizeof(int));
  int nTips = 0;
  int *m;

  for (int i = 0; i < nVisableNodes; i++)
  {
    m = Matrix + (VisableNodes[i] - 1) * nNodes;
    int j;
    for (j = 0; j < nVisableNodes; j++)
    {
      if (m[VisableNodes[j] - 1] != 0) break;
    }
    if (j == nVisableNodes)
    {
      tip_record[i] = 1;
      nTips++;
    }
    else
    {
      tip_record[i] = 0;
    }
  }

  SEXP _Tips;
  PROTECT(_Tips = NEW_INTEGER(nTips));
  int *Tips = INTEGER_POINTER(_Tips);
  int n = 0;
  for (int i = 0; i < nVisableNodes; i++)
  {
    if (tip_record[i]) Tips[n++] = VisableNodes[i];
  }

  UNPROTECT(4);
  return (_Tips);
}
