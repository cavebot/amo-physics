*     D01BDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      DOUBLE PRECISION PI
      INTEGER          KOUNT
*     .. Local Scalars ..
      DOUBLE PRECISION A, ABSERR, B, EPSABS, EPSREL, RESULT
*     .. External Functions ..
      DOUBLE PRECISION FST, X01AAF
      EXTERNAL         FST, X01AAF
*     .. External Subroutines ..
      EXTERNAL         D01BDF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX
*     .. Common blocks ..
      COMMON           /TELNUM/PI, KOUNT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01BDF Example Program Results'
      PI = X01AAF(0.0D0)
      EPSABS = 0.0D0
      EPSREL = 1.0D-04
      A = 0.0D0
      B = 1.0D0
      KOUNT = 0
*
      CALL D01BDF(FST,A,B,EPSABS,EPSREL,RESULT,ABSERR)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'A      - lower limit of integration = ', A
      WRITE (NOUT,99999) 'B      - upper limit of integration = ', B
      WRITE (NOUT,99998) 'EPSABS - absolute accuracy requested = ',
     +  EPSABS
      WRITE (NOUT,99998) 'EPSREL - relative accuracy requested = ',
     +  EPSREL
      WRITE (NOUT,*)
      WRITE (NOUT,99997) 'RESULT - approximation to the integral = ',
     +  RESULT
      WRITE (NOUT,99998) 'ABSERR - estimate to the absolute error = ',
     +  ABSERR
      WRITE (NOUT,99996) 'KOUNT  - number of function evaluations = ',
     +  KOUNT
      WRITE (NOUT,*)
      IF (KOUNT.GT.87 .OR. ABSERR.GT.MAX(EPSABS,EPSREL*ABS(RESULT)))
     +    THEN
         WRITE (NOUT,*)
     +     'Warning - requested accuracy may not have been achieved'
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
      DOUBLE PRECISION              PI
      INTEGER                       KOUNT
*     .. Intrinsic Functions ..
      INTRINSIC                     SIN
*     .. Common blocks ..
      COMMON                        /TELNUM/PI, KOUNT
*     .. Executable Statements ..
      KOUNT = KOUNT + 1
      FST = (X**2)*SIN(10.0D0*PI*X)
      RETURN
      END
