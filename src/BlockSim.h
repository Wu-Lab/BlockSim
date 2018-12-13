#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include "misc.h"
using namespace std;

/* Interfaces to R */

extern "C" {
  /* DLL Init */
  void R_init_BlockSim(DllInfo *info);
  
  /* Utils */
  SEXP BS_ShortestDistances(SEXP _Edges, SEXP _Index, SEXP _SourceNodes);

  SEXP BS_BlockTips(SEXP _VisableNodes, SEXP _Matrix);

}
