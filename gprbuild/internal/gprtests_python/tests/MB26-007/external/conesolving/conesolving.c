#include "DSDP5.8/include/dsdp5.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

//Code by P. Lecorguiller
//

  void dsdp_dense(const int n, //problem dimension
		  const double c[],     // c'x is the cost function
		  double sol[], // solution vector size n
		  int *status,  // convergence status(DSDPTerminationReason)
		  const int ml, // number of inequalities
		  const double hl[],    // flatten single column dense matrix (ml, 1)
		  const double Gl[],    // flatten dense matrix (ml, n) : model {11,21,12,22}
		  const int ms, // number of linear matrix inequality constraints
		  const double hs[],    // flatten single column dense or sparse matrix (ms, ms)
		  const double Gs[]     // flatten s dense or dense or sparse matrix (squaresum blocks_ms, n)
  ) {

    /* declaration */
    bool has_worked = true;
    DSDP sdp;
    LPCone lpcone;
    SDPCone sdpcone;
    DSDPTerminationReason info;

    //DSDPSolutionType status;

    /* create */
    DSDPCreate(n, &sdp);
    DSDPCreateLPCone(sdp, &lpcone);
    const int nblocks = 1;
    DSDPCreateSDPCone(sdp, nblocks, &sdpcone);

    /* cost function */
    for (int k = 0; k < n; k++) {
      DSDPSetDualObjective(sdp, k + 1, -c[k]);
    }
    //linear inequalities: store [Gl, hl] */
    int nnz = ml * (n + 1);
    int *lp_colptr = (int *)calloc(n + 2, sizeof(int));
    int *lp_rowind = (int *)calloc(nnz, sizeof(int));
    double *lp_values = (double *)calloc(nnz, sizeof(double));
    lp_colptr[0] = 0;
    if (ml) {
      for (int k = 0; k < nnz; k++) {
	lp_values[k] = Gl[k];
      } for (int k = 0; k < n; k++) {
	for (int j = 0; j < ml; j++)
	  lp_rowind[ml * k + j] = j;
	lp_colptr[k + 1] = lp_colptr[k] + ml;
      } for (int k = 0; k < ml; k++) {
	lp_values[k + lp_colptr[n]] = hl[k];
      } for (int k = 0; k < ml; k++)
	lp_rowind[lp_colptr[n] + k] = k;
      lp_colptr[n + 1] = lp_colptr[n] + ml;
    }
    LPConeSetData2(lpcone, ml, lp_colptr, lp_rowind, lp_values);

    /* LPConeView(lpcone); */

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    /* linear matrix inequalities: store hs Gs */
    for (int nbs = 0; nbs < nblocks; nbs++) {       //
      int mnz = ms * (ms + 1) / 2;
      int mk2 = ms * ms;
      for (int i = 0; i < n + 1; i++) {
	double *val;
	val =
	(double *)calloc(ms * (ms + 1) / 2,
			 sizeof(double));
	for (int ii = 0; ii < ms; ii++) {
	  for (int jj = 0; jj < ii + 1; jj++) {
	    if (i == 0) {
	      val[ii * (ii + 1) / 2 +
	      jj] =
	      hs[ii * ms + jj];
	    }

	    else {
	      val[ii * (ii + 1) / 2 +
	      jj] =
	      Gs[(i - 1) * mk2 +
	      ii * ms + jj];
	    }
	  }
	}
	SDPConeSetADenseVecMat(sdpcone, nbs, i, ms,
			       1.0, val, mnz);

	/*       SDPConeViewDataMatrix(sdpcone, nbs, i); */
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    /* solve */
    DSDPSetup(sdp);
    has_worked = DSDPSolve(sdp);
    DSDPStopReason(sdp, &info);
    *status = (int) info;


    //printf( info);
    DSDPGetY(sdp, sol, n);

    //DSDPComputeX(sdp);
    free(lp_colptr);
    free(lp_rowind);
    free(lp_values);
    DSDPDestroy(sdp);
  }

   void dsdp_sparse(const int n,     //problem dimension
                        const double c[], // c'x is the cost function
                        double sol[],     // solution vector size n
                        int *status,      // convergence status(DSDPTerminationReason)
                        const int ml,     // number of inequalities
                        const double hl[],        // flatten single column dense matrix (ml, 1)
                        const double Gl[],        // flatten dense matrix (ml, n) : model {11,21,12,22}
                        const int ms,     // number of linear matrix inequality constraints
                        const double hs[],        // flatten single column dense or sparse matrix (ms, ms)
                        const double Gs[],        // flatten s dense or dense or sparse matrix (squaresum blocks_ms, n)
                        const int sparse_dim_ms[],        // size (n+1)
                        const int ind_hs[],       // flatten single column dense or sparse matrix (squaresum blocks_ms, 1)
                        const int ind_Gs[]        // flatten dense or dense or sparse matrix (squaresum blocks_ms, n)
      ) {

          /* declaration */
          bool has_worked = true;
          DSDP sdp;
          LPCone lpcone;
          SDPCone sdpcone;
          DSDPTerminationReason info;

          //DSDPSolutionType status;

          /* create */
          DSDPCreate(n, &sdp);
          DSDPCreateLPCone(sdp, &lpcone);
          const int nblocks = 1;
          DSDPCreateSDPCone(sdp, nblocks, &sdpcone);

          /* cost function */
          for (int k = 0; k < n; k++) {
                  DSDPSetDualObjective(sdp, k + 1, -c[k]);
          }
          //////////////////////////////////////////////////////////////////////////////////////////////////////
          /* linear inequalities: store [Gl, hl] */
          int nnz = ml * (n + 1);
          int *lp_colptr = (int *)calloc(n + 2, sizeof(int));
          int *lp_rowind = (int *)calloc(nnz, sizeof(int));
          double *lp_values = (double *)calloc(nnz, sizeof(double));
          lp_colptr[0] = 0;
          if (ml) {
                  for (int k = 0; k < nnz; k++) {
                          lp_values[k] = Gl[k];
                  } for (int k = 0; k < n; k++) {
                          for (int j = 0; j < ml; j++)
                                  lp_rowind[ml * k + j] = j;
                          lp_colptr[k + 1] = lp_colptr[k] + ml;
                  } for (int k = 0; k < ml; k++) {
                          lp_values[k + lp_colptr[n]] = hl[k];
                  } for (int k = 0; k < ml; k++)
                          lp_rowind[lp_colptr[n] + k] = k;
                  lp_colptr[n + 1] = lp_colptr[n] + ml;
          }
          LPConeSetData2(lpcone, ml, lp_colptr, lp_rowind, lp_values);

          /* LPConeView(lpcone); */

          //////////////////////////////////////////////////////////////////////////////////////////////////////
          /* linear matrix inequalities: store hs Gs */
          for (int nbs = 0; nbs < nblocks; nbs++) {       //
                  int mnz = ms * (ms + 1) / 2;
                  int mk2 = ms * ms;
                   int sparse_idx = 0;
                  for (int i = 0; i < n + 1; i++) {
                          double *val;
                          int *ind;
                          int sparse_dim = sparse_dim_ms[i];
                          if (sparse_dim == 0) {
                                  val =
                                      (double *)calloc(ms * (ms + 1) / 2,
                                                       sizeof(double));
                                  for (int ii = 0; ii < ms; ii++) {
                                          for (int jj = 0; jj < ii + 1;
                                                jj++) {
                                                  if (i == 0) {
                                                          val[ii *
                                                               (ii +
                                                                1) / 2 +
                                                               jj] =
                                                              hs[ii * ms +
                                                                 jj];
                                                  }

                                                  else {
                                                          val[ii *
                                                               (ii +
                                                                1) / 2 +
                                                               jj] =
                                                              Gs[(i -
                                                                  1) *
                                                                 mk2 +
                                                                 ii * ms +
                                                                 jj];
                                                  }
                                          }
                                  }
                                  sparse_idx += (i == 0) ? 0 : mk2;
                                  SDPConeSetADenseVecMat(sdpcone, nbs, i,
                                                          ms, 1.0, val,
                                                          mnz);
                          } else {
                                  ind =
                                      (int *)calloc(sparse_dim,
                                                    sizeof(int));
                                  val =
                                      (double *)calloc(sparse_dim,
                                                       sizeof(double));
                                  for (int idx = 0; idx < sparse_dim;
                                        idx++) {
                                          if (i == 0) {
                                                  val[idx] = hs[idx];
                                                  ind[idx] = ind_hs[idx];
                                          }

                                          else {
                                                  val[idx] =
                                                      Gs[idx +
                                                         sparse_idx];
                                                  ind[idx] =
                                                      ind_Gs[idx +
                                                             sparse_idx];
                                          }
                                  }
                                  sparse_idx +=
                                      (i == 0) ? 0 : sparse_dim;
                                  SDPConeSetASparseVecMat(sdpcone, nbs,
                                                           i, ms, 1.0,
                                                           0.0, ind, val,
                                                           sparse_dim);
                          }

                          /*SDPConeViewDataMatrix(sdpcone, nbs, i); */
                  }
          }

              //////////////////////////////////////////////////////////////////////////////////////////////////////
              /* solve */
              DSDPSetup(sdp);
          has_worked = DSDPSolve(sdp);
           DSDPStopReason(sdp, &info);

          //printf( info);
          DSDPGetY(sdp, sol, n);

              //DSDPComputeX(sdp);
              free(lp_colptr);
          free(lp_rowind);
          free(lp_values);
          DSDPDestroy(sdp);
   }

