*     D01AUF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          LW, LIW
      PARAMETER        (LW=800,LIW=LW/4)
*     .. Scalars in Common ..
      INTEGER          KOUNT
*     .. Local Scalars ..
      DOUBLE PRECISION A, ABSERR, B, EPSABS, EPSREL, PI, RESULT
      INTEGER          IFAIL, KEY
*     .. Local Arrays ..
      DOUBLE PRECISION W(LW)
      INTEGER          IW(LIW)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D01AUF, FST
*     .. Common blocks ..
      COMMON           /TELNUM/KOUNT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01AUF Example Program Results'
      PI = X01AAF(0.0D0)
      EPSABS = 0.0D0
      EPSREL = 1.0D-03
      A = 0.0D0
      B = 2.0D0*PI
      KEY = 6
      KOUNT = 0
      IFAIL = -1
*
      CALL D01AUF(FST,A,B,KEY,EPSABS,EPSREL,RESULT,ABSERR,W,LW,IW,LIW,
     +            IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'A      - lower limit of integration = ', A
      WRITE (NOUT,99999) 'B      - upper limit of integration = ', B
      WRITE (NOUT,99998) 'EPSABS - absolute accuracy requested = ',
     +  EPSABS
      WRITE (NOUT,99998) 'EPSREL - relative accuracy requested = ',
     +  EPSREL
      WRITE (NOUT,*)
      IF (IFAIL.NE.0) WRITE (NOUT,99996) 'IFAIL = ', IFAIL
      IF (IFAIL.LE.3) THEN
         WRITE (NOUT,99997) 'RESULT - approximation to the integral = ',
     +     RESULT
         WRITE (NOUT,99998) 'ABSERR - estimate of the absolute error = '
     +     , ABSERR
         WRITE (NOUT,99996) 'KOUNT  - number of function evaluations = '
     +     , KOUNT
         WRITE (NOUT,99996) 'IW(1)  - number of subintervals used = ',
     +     IW(1)
      END IF
      STOP
*
99999 FORMAT (1X,A,F10.4)
99998 FORMAT (1X,A,D9.2)
99997 FORMAT (1X,A,F9.5)
99996 FORMAT (1X,A,I4)
      END
*
      SUBROUTINE FST(X,FV,N)
*     .. Scalar Arguments ..
      INTEGER        N
*     .. Array Arguments ..
      DOUBLE PRECISION FV(N), X(N)
*     .. Scalars in Common ..
      INTEGER        KOUNT
*     .. Local Scalars ..
      INTEGER        I
*     .. Intrinsic Functions ..
      INTRINSIC      COS, SIN
*     .. Common blocks ..
      COMMON         /TELNUM/KOUNT
*     .. Executable Statements ..
      KOUNT = KOUNT + N
      DO 20 I = 1, N
         FV(I) = X(I)*(SIN(30.0D0*X(I)))*COS(X(I))
   20 CONTINUE
      RETURN
      END
