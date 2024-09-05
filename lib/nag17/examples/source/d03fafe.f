*     D03FAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          L, M, N, MAXLM, LDIMF, MDIMF, LWRK
      PARAMETER        (L=16,M=32,N=20,MAXLM=32,LDIMF=L+1,MDIMF=M+1,
     +                 LWRK=2*(N+1)*MAXLM+3*L+3*M+4*N+6000)
*     .. Local Scalars ..
      DOUBLE PRECISION DX, DY, DZ, ERROR, LAMBDA, PERTRB, PI, T, XF, XS,
     +                 YF, YS, ZF, ZS
      INTEGER          I, IFAIL, J, K, LBDCND, MBDCND, NBDCND
*     .. Local Arrays ..
      DOUBLE PRECISION BDXF(MDIMF,N+1), BDXS(MDIMF,N+1),
     +                 BDYF(LDIMF,N+1), BDYS(LDIMF,N+1),
     +                 BDZF(LDIMF,M+1), BDZS(LDIMF,M+1),
     +                 F(LDIMF,MDIMF,N+1), W(LWRK), X(L+1), Y(M+1),
     +                 Z(N+1)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D03FAF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, COS, DBLE, SIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03FAF Example Program Results'
      LAMBDA = -2.0D0
      XS = 0.0D0
      XF = 1.0D0
      LBDCND = 1
      YS = 0.0D0
      PI = X01AAF(PI)
      YF = 2.0D0*PI
      MBDCND = 0
      ZS = 0.0D0
      ZF = PI/2.0D0
      NBDCND = 2
*
*     Define the grid points for later use.
*
      DX = (XF-XS)/DBLE(L)
      DO 20 I = 1, L + 1
         X(I) = XS + DBLE(I-1)*DX
   20 CONTINUE
      DY = (YF-YS)/DBLE(M)
      DO 40 J = 1, M + 1
         Y(J) = YS + DBLE(J-1)*DY
   40 CONTINUE
      DZ = (ZF-ZS)/DBLE(N)
      DO 60 K = 1, N + 1
         Z(K) = ZS + DBLE(K-1)*DZ
   60 CONTINUE
*
*     Define the array of derivative boundary values.
*
      DO 100 J = 1, M + 1
         DO 80 I = 1, L + 1
            BDZF(I,J) = -X(I)**4*SIN(Y(J))
   80    CONTINUE
  100 CONTINUE
*
*     Note that for this example all other boundary arrays are
*     dummy variables.
*
*     We define the function boundary values in the F array.
*
      DO 140 K = 1, N + 1
         DO 120 J = 1, M + 1
            F(1,J,K) = 0.0D0
            F(L+1,J,K) = SIN(Y(J))*COS(Z(K))
  120    CONTINUE
  140 CONTINUE
      DO 180 J = 1, M + 1
         DO 160 I = 1, L + 1
            F(I,J,1) = X(I)**4*SIN(Y(J))
  160    CONTINUE
  180 CONTINUE
*
*     Define the values of the right hand side of the Helmholtz
*     equation.
*
      DO 240 K = 2, N + 1
         DO 220 J = 1, M + 1
            DO 200 I = 2, L
               F(I,J,K) = 4.0D0*X(I)**2*(3.0D0-X(I)**2)*SIN(Y(J))
     +                    *COS(Z(K))
  200       CONTINUE
  220    CONTINUE
  240 CONTINUE
*
*     Call D03FAF to generate and solve the finite difference equation.
*
      IFAIL = 0
*
      CALL D03FAF(XS,XF,L,LBDCND,BDXS,BDXF,YS,YF,M,MBDCND,BDYS,BDYF,ZS,
     +            ZF,N,NBDCND,BDZS,BDZF,LAMBDA,LDIMF,MDIMF,F,PERTRB,W,
     +            LWRK,IFAIL)
*
*     Compute discretization error.  The exact solution to the
*     problem is
*
*        U(X,Y,Z) = X**4*SIN(Y)*COS(Z)
*
      ERROR = 0.0D0
      DO 300 K = 1, N + 1
         DO 280 J = 1, M + 1
            DO 260 I = 1, L + 1
               T = ABS(F(I,J,K)-X(I)**4*SIN(Y(J))*COS(Z(K)))
               IF (T.GT.ERROR) ERROR = T
  260       CONTINUE
  280    CONTINUE
  300 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Maximum component of discretization error =',
     +  ERROR
      STOP
*
99999 FORMAT (1X,A,1P,D13.6)
      END
