*     D02PAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, IW1
      PARAMETER        (N=3,IW1=9)
*     .. Local Scalars ..
      DOUBLE PRECISION H, PI, TOL, X, XEND
      INTEGER          CASE, I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION CIN(6), COMM(5), CON(3), COUT(14), SOL(N),
     +                 W(N,IW1), Y(N)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02PAF, D02XAF, FCN
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02PAF Example Program Results'
      PI = X01AAF(X)
      TOL = 1.0D-5
      XEND = 8.0D0
*     First case. Simple call
      CASE = 1
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Case', CASE
      CIN(1) = 0.0D0
      X = 0.0D0
      Y(1) = 0.0D0
      Y(2) = 0.5D0
      Y(3) = PI/5.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     X           Y(1)         Y(2)         Y(3)'
      WRITE (NOUT,99998) X, (Y(I),I=1,N)
      IFAIL = 1
*
      CALL D02PAF(X,XEND,N,Y,CIN,TOL,FCN,COMM,CON,COUT,W,N,IW1,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99998) X, (Y(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'CIN ', (CIN(I),I=1,6)
         WRITE (NOUT,99997) 'COMM', (COMM(I),I=1,4)
         WRITE (NOUT,99997) 'CON ', (CON(I),I=1,3)
         WRITE (NOUT,99997) 'COUT', (COUT(I),I=1,6)
         WRITE (NOUT,99997) '    ', (COUT(I),I=7,12)
         WRITE (NOUT,99996) '    ', (COUT(I),I=13,14), ' at X = ', X
      END IF
*     Second case. Relative error test componentwise with floor
      CASE = 2
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Case', CASE
      W(1,7) = 1.0D0
      W(2,7) = 1.0D0
      W(3,7) = 1.0D-1
      W(1,8) = 1.0D-8
      W(2,8) = 1.0D-8
      W(3,8) = 1.0D-8
      X = 0.0D0
      CIN(1) = 1.0D0
      CIN(2) = 3.0D0
      CIN(5) = 0.0D0
      Y(1) = 0.0D0
      Y(2) = 0.5D0
      Y(3) = PI/5.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     X           Y(1)         Y(2)         Y(3)'
      WRITE (NOUT,99998) X, (Y(I),I=1,N)
      IFAIL = 1
*
      CALL D02PAF(X,XEND,N,Y,CIN,TOL,FCN,COMM,CON,COUT,W,N,IW1,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99998) X, (Y(I),I=1,N)
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,99995) 'In D02PAF IFAIL =', IFAIL, ' at X = ', X
      END IF
*     Third case. Comparison of output from D02PAF called repeatedly
*     over a short interval and D02PAF called on a long interval
*     with interpolation using D02XAF.
      CASE = 3
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Case', CASE, ' making successive calls'
      CIN(1) = 0.0D0
      X = 0.0D0
      Y(1) = 0.0D0
      Y(2) = 0.5D0
      Y(3) = PI/5.0D0
      H = 1.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     X           Y(1)         Y(2)         Y(3)'
      WRITE (NOUT,99998) X, (Y(I),I=1,N)
      DO 20 J = 1, 8
         IFAIL = 1
*
         CALL D02PAF(X,X+H,N,Y,CIN,TOL,FCN,COMM,CON,COUT,W,N,IW1,IFAIL)
*
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,99998) X, (Y(I),I=1,N)
         ELSE
            WRITE (NOUT,*)
            WRITE (NOUT,99995) 'In D02PAF IFAIL =', IFAIL, ' at X = ', X
            GO TO 40
         END IF
   20 CONTINUE
   40 WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Case', CASE, ' interpolating'
      CIN(1) = 1.0D0
      CIN(5) = 0.0D0
      X = 0.0D0
      Y(1) = 0.0D0
      Y(2) = 0.5D0
      Y(3) = PI/5.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     X           Y(1)         Y(2)         Y(3)'
      WRITE (NOUT,99998) X, (Y(I),I=1,N)
      DO 60 J = 1, 8
         COMM(4) = -1.0D0
         COMM(5) = DBLE(J)
         IFAIL = 1
*
         CALL D02PAF(X,XEND,N,Y,CIN,TOL,FCN,COMM,CON,COUT,W,N,IW1,IFAIL)
*
         IF (IFAIL.GT.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99995) 'In D02PAF IFAIL =', IFAIL, ' at X = ', X
            STOP
         END IF
         IFAIL = 1
*
         CALL D02XAF(COMM(5),X,COUT,N,Y,W,N,SOL,IFAIL)
*
         IF (IFAIL.GT.0) THEN
            WRITE (NOUT,99995) 'In D02XAF IFAIL = ', IFAIL
            STOP
         END IF
         WRITE (NOUT,99998) COMM(5), (SOL(I),I=1,N)
   60 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I2,A)
99998 FORMAT (1X,F9.4,3F13.5)
99997 FORMAT (1X,A,6D11.4)
99996 FORMAT (1X,A,2D11.4,A,F9.4)
99995 FORMAT (1X,A,I2,A,F9.4)
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
