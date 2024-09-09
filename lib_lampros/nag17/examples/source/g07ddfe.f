*     G07DDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=1000)
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, PROPN, TMEAN, TVAR, WMEAN, WVAR
      INTEGER          I, IFAIL, K, N
*     .. Local Arrays ..
      DOUBLE PRECISION SX(NMAX), X(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G07DDF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G07DDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, (X(I),I=1,N), ALPHA
      IFAIL = 0
*
      CALL G07DDF(N,X,ALPHA,TMEAN,WMEAN,TVAR,WVAR,K,SX,IFAIL)
*
      PROPN = DBLE(K)/N
      PROPN = 100.0D0 - 200.0D0*PROPN
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Statistics from middle ', PROPN, '% of data'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) '               Trimmed-mean = ', TMEAN
      WRITE (NOUT,99998) '   Variance of Trimmed-mean = ', TVAR
      WRITE (NOUT,*)
      WRITE (NOUT,99998) '            Winsorized-mean = ', WMEAN
      WRITE (NOUT,99998) 'Variance of Winsorized-mean = ', WVAR
      STOP
*
99999 FORMAT (1X,A,F6.2,A)
99998 FORMAT (1X,A,F11.4)
      END
