*     F11JEF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LA, LWORK
      PARAMETER        (NMAX=1000,LA=10000,LWORK=6*NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION OMEGA, RNORM, TOL
      INTEGER          I, IFAIL, ITN, MAXITN, N, NNZ
      CHARACTER        PRECON
      CHARACTER*6      METHOD
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA), B(NMAX), WORK(LWORK), X(NMAX)
      INTEGER          ICOL(LA), IROW(LA), IWORK(NMAX+1)
*     .. External Subroutines ..
      EXTERNAL         F11JEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F11JEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*
*     Read algorithmic parameters
*
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) NNZ
         READ (NIN,*) METHOD, PRECON
         READ (NIN,*) OMEGA
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
*     Solve Ax = b using F11JEF
*
         IFAIL = 0
         CALL F11JEF(METHOD,PRECON,N,NNZ,A,IROW,ICOL,OMEGA,B,TOL,MAXITN,
     +               X,RNORM,ITN,WORK,LWORK,IWORK,IFAIL)
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
99999 FORMAT (1X,A,I10,A)
99998 FORMAT (1X,A,1P,D16.3)
99997 FORMAT (1X,1P,D16.4)
      STOP
      END
