*     G07DCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=25)
*     .. Local Scalars ..
      DOUBLE PRECISION BETA, SIGMA, SIGSAV, THESAV, THETA, TOL
      INTEGER          I, IFAIL, ISIGMA, MAXIT, N, NIT
*     .. Local Arrays ..
      DOUBLE PRECISION RS(NMAX), WRK(NMAX), X(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION CHI, PSI
      EXTERNAL         CHI, PSI
*     .. External Subroutines ..
      EXTERNAL         G07DCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G07DCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (X(I),I=1,N)
         READ (NIN,*) BETA, MAXIT
         WRITE (NOUT,*)
     +     '          Input parameters     Output parameters'
         WRITE (NOUT,*) 'ISIGMA   SIGMA   THETA   TOL    SIGMA  THETA'
   20    READ (NIN,*,END=40) ISIGMA, SIGMA, THETA, TOL
         SIGSAV = SIGMA
         THESAV = THETA
         IFAIL = 0
*
         CALL G07DCF(CHI,PSI,ISIGMA,N,X,BETA,THETA,SIGMA,MAXIT,TOL,RS,
     +               NIT,WRK,IFAIL)
*
         WRITE (NOUT,99999) ISIGMA, SIGSAV, THESAV, TOL, SIGMA, THETA
         GO TO 20
      ELSE
         WRITE (NOUT,99998) 'N is out of range: N =', N
      END IF
   40 STOP
*
99999 FORMAT (1X,I3,3X,2F8.4,F7.4,1X,2F8.4)
99998 FORMAT (1X,A,I5)
      END
*
      DOUBLE PRECISION FUNCTION PSI(T)
*     Hampel's Piecewise Linear Function.
*     .. Parameters ..
      DOUBLE PRECISION              ZERO
      PARAMETER                     (ZERO=0.0D+0)
      DOUBLE PRECISION              H1, H2, H3
      PARAMETER                     (H1=1.5D0,H2=3.0D0,H3=4.5D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Local Scalars ..
      DOUBLE PRECISION              ABST
*     .. Intrinsic Functions ..
      INTRINSIC                     ABS, MIN
*     .. Executable Statements ..
      ABST = ABS(T)
      IF (ABST.LT.H3) THEN
         IF (ABST.LE.H2) THEN
            PSI = MIN(H1,ABST)
         ELSE
            PSI = H1*(H3-ABST)/(H3-H2)
         END IF
         IF (T.LT.ZERO) PSI = -PSI
      ELSE
         PSI = ZERO
      END IF
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION CHI(T)
*     Huber's CHI function.
*     .. Parameters ..
      DOUBLE PRECISION              DCHI
      PARAMETER                     (DCHI=1.5D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Local Scalars ..
      DOUBLE PRECISION              ABST, PS
*     .. Intrinsic Functions ..
      INTRINSIC                     ABS, MIN
*     .. Executable Statements ..
      ABST = ABS(T)
      PS = MIN(DCHI,ABST)
      CHI = PS*PS/2
      RETURN
      END
