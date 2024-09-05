*     D01ALF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NPTS, LW, LIW
      PARAMETER        (NPTS=1,LW=800,LIW=LW/2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          KOUNT
*     .. Local Scalars ..
      DOUBLE PRECISION A, ABSERR, B, EPSABS, EPSREL, RESULT
      INTEGER          IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION POINTS(NPTS), W(LW)
      INTEGER          IW(LIW)
*     .. External Functions ..
      DOUBLE PRECISION FST
      EXTERNAL         FST
*     .. External Subroutines ..
      EXTERNAL         D01ALF
*     .. Common blocks ..
      COMMON           /TELNUM/KOUNT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01ALF Example Program Results'
      EPSABS = 0.0D0
      EPSREL = 1.0D-03
      A = 0.0D0
      B = 1.0D0
      POINTS(1) = 1.0D0/7.0D0
      KOUNT = 0
      IFAIL = -1
*
      CALL D01ALF(FST,A,B,NPTS,POINTS,EPSABS,EPSREL,RESULT,ABSERR,W,LW,
     +            IW,LIW,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'A      - lower limit of integration = ', A
      WRITE (NOUT,99999) 'B      - upper limit of integration = ', B
      WRITE (NOUT,99998) 'EPSABS - absolute accuracy requested = ',
     +  EPSABS
      WRITE (NOUT,99998) 'EPSREL - relative accuracy requested = ',
     +  EPSREL
      WRITE (NOUT,99999) 'POINTS(1) - given break-point = ', POINTS(1)
      WRITE (NOUT,*)
      IF (IFAIL.NE.0) WRITE (NOUT,99996) 'IFAIL = ', IFAIL
      IF (IFAIL.LE.5) THEN
         WRITE (NOUT,99997)
     +     '  RESULT - approximation to the integral = ', RESULT
         WRITE (NOUT,99998)
     +     '  ABSERR - estimate of the absolute error = ', ABSERR
         WRITE (NOUT,99996)
     +     '  KOUNT  - number of function evaluations = ', KOUNT
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
      DOUBLE PRECISION FUNCTION FST(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              X
*     .. Scalars in Common ..
      INTEGER                       KOUNT
*     .. Local Scalars ..
      DOUBLE PRECISION              A
*     .. Intrinsic Functions ..
      INTRINSIC                     ABS
*     .. Common blocks ..
      COMMON                        /TELNUM/KOUNT
*     .. Executable Statements ..
      KOUNT = KOUNT + 1
      A = ABS(X-1.0D0/7.0D0)
      FST = 0.0D0
      IF (A.NE.0.0D0) FST = A**(-0.5D0)
      RETURN
      END
