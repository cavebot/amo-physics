*     D02XBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, IW
      PARAMETER        (N=3,IW=7)
*     .. Local Scalars ..
      DOUBLE PRECISION PI, SOL, TOL, X, XEND
      INTEGER          I, IFAIL, M
*     .. Local Arrays ..
      DOUBLE PRECISION CIN(6), COMM(5), CON(3), COUT(14), W(N,IW), Y(N)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02PAF, D02XBF, FCN
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02XBF Example Program Results'
      PI = X01AAF(X)
      M = 1
      TOL = 1.0D-5
      X = 0.0D0
      XEND = 8.0D0
      Y(1) = 0.0D0
      Y(2) = 0.5D0
      Y(3) = PI/5.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '      X          Y(', M, ')'
      WRITE (NOUT,99998) X, Y(M)
      CIN(1) = 1.0D0
      DO 20 I = 2, 5
         CIN(I) = 0.0D0
   20 CONTINUE
      DO 40 I = 1, 3
         COMM(I) = 0.0D0
   40 CONTINUE
      DO 60 I = 1, 3
         CON(I) = 0.0D0
   60 CONTINUE
*     Interrupt D02PAF for output using D02XBF
      DO 80 I = 1, 8
         COMM(4) = -1.0D0
         COMM(5) = DBLE(I)
         IFAIL = 1
*
         CALL D02PAF(X,XEND,N,Y,CIN,TOL,FCN,COMM,CON,COUT,W,N,IW,IFAIL)
*
         IF (IFAIL.GT.0) THEN
            IFAIL = 1
            WRITE (NOUT,99997) 'Error in D02PAF, IFAIL =', IFAIL,
     +        ' CIN(1)=', CIN(1)
            STOP
         END IF
*
         CALL D02XBF(COMM(5),X,COUT,N,Y,W,N,M,SOL,IFAIL)
*
         IF (IFAIL.GT.0) THEN
            WRITE (NOUT,99997) 'Error in D02XBF, IFAIL =', IFAIL
            STOP
         END IF
         WRITE (NOUT,99998) COMM(5), SOL
   80 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I1,A)
99998 FORMAT (1X,F9.2,F13.5)
99997 FORMAT (1X,A,I2,A,F8.3)
      END
*
      SUBROUTINE FCN(T,Y,F)
*     .. Parameters ..
      INTEGER        N
      PARAMETER      (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), Y(N)
*     .. Intrinsic Functions ..
      INTRINSIC      COS, TAN
*     .. Executable Statements ..
      F(1) = TAN(Y(3))
      F(2) = -0.032D0*TAN(Y(3))/Y(2) - 0.02D0*Y(2)/COS(Y(3))
      F(3) = -0.032D0/Y(2)**2
      RETURN
      END
