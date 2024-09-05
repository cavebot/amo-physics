*     D01APF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          LW, LIW
      PARAMETER        (LW=800,LIW=LW/4)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          KOUNT, NOF
*     .. Local Scalars ..
      DOUBLE PRECISION A, ABSERR, B, EPSABS, EPSREL, RESULT
      INTEGER          IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION ALFA(2), BETA(2), W(LW)
      INTEGER          INTEGR(2), IW(LIW)
*     .. External Functions ..
      DOUBLE PRECISION FST
      EXTERNAL         FST
*     .. External Subroutines ..
      EXTERNAL         D01APF
*     .. Common blocks ..
      COMMON           /TELNUM/KOUNT, NOF
*     .. Data statements ..
      DATA             ALFA/0.0D0, -0.5D0/
      DATA             BETA/0.0D0, -0.5D0/
      DATA             INTEGR/2, 1/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01APF Example Program Results'
      EPSABS = 0.0D0
      EPSREL = 1.0D-04
      A = 0.0D0
      B = 1.0D0
      DO 20 NOF = 1, 2
         KOUNT = 0
         IFAIL = -1
*
         CALL D01APF(FST,A,B,ALFA(NOF),BETA(NOF),INTEGR(NOF),EPSABS,
     +               EPSREL,RESULT,ABSERR,W,LW,IW,LIW,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'A      - lower limit of integration = ', A
         WRITE (NOUT,99999) 'B      - upper limit of integration = ', B
         WRITE (NOUT,99998) 'EPSABS - absolute accuracy requested = ',
     +     EPSABS
         WRITE (NOUT,99998) 'EPSREL - relative accuracy requested = ',
     +     EPSREL
         WRITE (NOUT,*)
         WRITE (NOUT,99998)
     +     'ALFA   - parameter in the weight function = ', ALFA(NOF)
         WRITE (NOUT,99998)
     +     'BETA   - parameter in the weight function = ', BETA(NOF)
         WRITE (NOUT,99997)
     +     'INTEGR - denotes which weight function is to be used = ',
     +     INTEGR(NOF)
         WRITE (NOUT,*)
         IF (IFAIL.NE.0) WRITE (NOUT,99997) 'IFAIL = ', IFAIL
         IF (IFAIL.LE.3) THEN
            WRITE (NOUT,99996)
     +        'RESULT - approximation to the integral = ', RESULT
            WRITE (NOUT,99998)
     +        'ABSERR - estimate of the absolute error = ', ABSERR
            WRITE (NOUT,99997)
     +        'KOUNT  - number of function evaluations = ', KOUNT
            WRITE (NOUT,99997) 'IW(1)  - number of subintervals used = '
     +        , IW(1)
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,F10.4)
99998 FORMAT (1X,A,D9.2)
99997 FORMAT (1X,A,I4)
99996 FORMAT (1X,A,F9.5)
      END
*
      DOUBLE PRECISION FUNCTION FST(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              X
*     .. Scalars in Common ..
      INTEGER                       KOUNT, NOF
*     .. Local Scalars ..
      DOUBLE PRECISION              A, OMEGA, PI
*     .. External Functions ..
      DOUBLE PRECISION              X01AAF
      EXTERNAL                      X01AAF
*     .. Intrinsic Functions ..
      INTRINSIC                     COS, SIN
*     .. Common blocks ..
      COMMON                        /TELNUM/KOUNT, NOF
*     .. Executable Statements ..
      PI = X01AAF(PI)
      KOUNT = KOUNT + 1
      IF (NOF.EQ.1) THEN
         A = 10.0D0*PI
         FST = COS(A*X)
      ELSE
         OMEGA = 10.0D0
         FST = SIN(OMEGA*X)
      END IF
      RETURN
      END
