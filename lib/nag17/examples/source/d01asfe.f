*     D01ASF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          LW, LIW, LIMLST
      PARAMETER        (LW=800,LIW=LW/2,LIMLST=50)
*     .. Scalars in Common ..
      INTEGER          KOUNT
*     .. Local Scalars ..
      DOUBLE PRECISION A, ABSERR, EPSABS, OMEGA, RESULT
      INTEGER          IFAIL, INTEGR, LST
*     .. Local Arrays ..
      DOUBLE PRECISION ERLST(LIMLST), RSLST(LIMLST), W(LW)
      INTEGER          IERLST(LIMLST), IW(LIW)
*     .. External Functions ..
      DOUBLE PRECISION FST, X01AAF
      EXTERNAL         FST, X01AAF
*     .. External Subroutines ..
      EXTERNAL         D01ASF
*     .. Common blocks ..
      COMMON           /TELNUM/KOUNT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01ASF Example Program Results'
      EPSABS = 1.0D-03
      A = 0.0D0
      KOUNT = 0
      OMEGA = 0.5D0*X01AAF(0.0D0)
      INTEGR = 1
      IFAIL = -1
*
      CALL D01ASF(FST,A,OMEGA,INTEGR,EPSABS,RESULT,ABSERR,LIMLST,LST,
     +            ERLST,RSLST,IERLST,W,LW,IW,LIW,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'A      - lower limit of integration = ', A
      WRITE (NOUT,*) 'B      - upper limit of integration =  infinity'
      WRITE (NOUT,99998) 'EPSABS - absolute accuracy requested = ',
     +  EPSABS
      WRITE (NOUT,*)
      IF (IFAIL.NE.0) WRITE (NOUT,99996) 'IFAIL = ', IFAIL
      IF (IFAIL.NE.6 .AND. IFAIL.NE.10) THEN
         WRITE (NOUT,99997) 'RESULT - approximation to the integral = ',
     +     RESULT
         WRITE (NOUT,99998) 'ABSERR - estimate of the absolute error = '
     +     , ABSERR
         WRITE (NOUT,99996) 'KOUNT  - number of function evaluations = '
     +     , KOUNT
         WRITE (NOUT,99996) 'LST    - number of intervals used = ', LST
         WRITE (NOUT,99996)
     +   'IW(1)  - max. no. of subintervals used in any one interval = '
     +     , IW(1)
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
*     .. Intrinsic Functions ..
      INTRINSIC                     SQRT
*     .. Common blocks ..
      COMMON                        /TELNUM/KOUNT
*     .. Executable Statements ..
      KOUNT = KOUNT + 1
      FST = 0.0D0
      IF (X.GT.0.0D0) FST = 1.0D0/SQRT(X)
      RETURN
      END
