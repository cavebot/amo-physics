*     F11JCF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LA, LIWORK, LWORK
      PARAMETER        (NMAX=1000,LA=10000,LIWORK=2*LA+7*NMAX+1,
     +                 LWORK=6*NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION DSCALE, DTOL, RNORM, TOL
      INTEGER          I, IFAIL, ITN, LFILL, MAXITN, N, NNZ, NNZC, NPIVM
      CHARACTER        MIC, PSTRAT
      CHARACTER*6      METHOD
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA), B(NMAX), WORK(LWORK), X(NMAX)
      INTEGER          ICOL(LA), IPIV(NMAX), IROW(LA), ISTR(NMAX+1),
     +                 IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         F11JAF, F11JCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F11JCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*
*     Read algorithmic parameters
*
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) NNZ
         READ (NIN,*) METHOD
         READ (NIN,*) LFILL, DTOL
         READ (NIN,*) MIC, DSCALE
         READ (NIN,*) PSTRAT
         READ (NIN,*) TOL, MAXITN
*
*     Read the matrix A
*
         DO 20 I = 1, NNZ
            READ (NIN,*) A(I), IROW(I), ICOL(I)
   20    CONTINUE
*
*     Read right-hand side vector b and initial approximate solution x
*
         READ (NIN,*) (B(I),I=1,N)
         READ (NIN,*) (X(I),I=1,N)
*
*     Calculate incomplete Cholesky factorization
*
         IFAIL = 0
         CALL F11JAF(N,NNZ,A,LA,IROW,ICOL,LFILL,DTOL,MIC,DSCALE,PSTRAT,
     +               IPIV,ISTR,NNZC,NPIVM,IWORK,LIWORK,IFAIL)
*
*     Solve Ax = b using F11JCF
*
         CALL F11JCF(METHOD,N,NNZ,A,LA,IROW,ICOL,IPIV,ISTR,B,TOL,MAXITN,
     +               X,RNORM,ITN,WORK,LWORK,IFAIL)
*
         WRITE (NOUT,99999) 'Converged in', ITN, ' iterations'
         WRITE (NOUT,99998) 'Final residual norm =', RNORM
*
*     Output x
*
         DO 40 I = 1, N
            WRITE (NOUT,99997) X(I)
   40    CONTINUE
      END IF
      STOP
99999 FORMAT (1X,A,I10,A)
99998 FORMAT (1X,A,1P,D16.3)
99997 FORMAT (1X,1P,D16.4)
      END
