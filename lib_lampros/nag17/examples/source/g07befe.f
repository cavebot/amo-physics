*     G07BEF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=20)
*     .. Local Scalars ..
      DOUBLE PRECISION BETA, CORR, DEV, GAMMA, SEBETA, SEGAM, TOL
      INTEGER          I, IFAIL, MAXIT, N, NIT
*     .. Local Arrays ..
      DOUBLE PRECISION WK(NMAX), X(NMAX)
      INTEGER          IC(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G07BEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G07BEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (X(I),I=1,N)
*        If data were censored then IC would also be read in.
*        Leave G07BEF to calculate initial values
         GAMMA = 0.0D0
*        Use default values for TOL and MAXIT
         TOL = 0.0D0
         MAXIT = 0
         IFAIL = 0
*
         CALL G07BEF('No censor',N,X,IC,BETA,GAMMA,TOL,MAXIT,SEBETA,
     +               SEGAM,CORR,DEV,NIT,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' BETA  = ', BETA, ' Standard error = ',
     +     SEBETA
         WRITE (NOUT,99999) ' GAMMA = ', GAMMA,
     +     ' Standard error = ', SEGAM
      END IF
      STOP
*
99999 FORMAT (1X,2(A,F10.4))
      END
