*     D03EEF Example Program Text
*     Mark 16 Revised. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          LEVELS, NGX, NGY, LDA
      PARAMETER        (LEVELS=3,NGX=2**LEVELS+1,NGY=NGX,LDA=4*(NGX+1)
     +                 *(NGY+1)/3)
*     .. Arrays in Common ..
      DOUBLE PRECISION USER(6)
*     .. Local Scalars ..
      DOUBLE PRECISION ACC, HX, HY, PI, RMSERR, XMAX, XMIN, YMAX, YMIN
      INTEGER          I, IFAIL, IOUT, J, MAXIT, NUMIT
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,7), RHS(LDA), U(LDA), UB(NGX*NGY), US(LDA),
     +                 X(NGX*NGY), Y(NGX*NGY)
*     .. External Functions ..
      DOUBLE PRECISION FEXACT, X01AAF
      EXTERNAL         FEXACT, X01AAF
*     .. External Subroutines ..
      EXTERNAL         BNDY, D03EDF, D03EEF, PDEF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE, SQRT
*     .. Common blocks ..
      COMMON           /BLOCK1/USER
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03EEF Example Program Results'
      WRITE (NOUT,*)
      PI = X01AAF(0.0D0)
*
*     USER(1) .. USER(6) contain the coefficients ALPHA, BETA, GAMMA,
*     DELTA, EPSLON and PHI appearing in the example partial
*     differential equation. They are stored in COMMON for use in PDEF.
*
      USER(1) = 1.0D0
      USER(2) = 0.0D0
      USER(3) = 1.0D0
      USER(4) = 50.0D0
      USER(5) = 50.0D0
      USER(6) = 0.0D0
*
      XMIN = 0.0D0
      XMAX = 1.0D0
      YMIN = 0.0D0
      YMAX = 1.0D0
      HX = (XMAX-XMIN)/DBLE(NGX-1)
      HY = (YMAX-YMIN)/DBLE(NGY-1)
      DO 40 I = 1, NGX
         DO 20 J = 1, NGY
            X(I+(J-1)*NGX) = XMIN + DBLE(I-1)*HX
            Y(I+(J-1)*NGX) = YMIN + DBLE(J-1)*HY
   20    CONTINUE
   40 CONTINUE
*
*     Discretize the equations
*
      IFAIL = -1
*
      CALL D03EEF(XMIN,XMAX,YMIN,YMAX,PDEF,BNDY,NGX,NGY,LDA,A,RHS,
     +            'Central',IFAIL)
*
*     Set the initial guess to zero
*
      DO 60 I = 1, NGX*NGY
         UB(I) = 0.0D0
   60 CONTINUE
*
*     Solve the equations
*
*     ** set IOUT.GE.2 to obtain intermediate output from D03EDF **
*
      IOUT = 0
      ACC = 1.0D-6
      MAXIT = 50
      IFAIL = -1
*
      CALL D03EDF(NGX,NGY,LDA,A,RHS,UB,MAXIT,ACC,US,U,IOUT,NUMIT,IFAIL)
*
*     Print out the solution
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Exact solution above computed solution'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) '  I/J', (I,I=1,NGX)
      RMSERR = 0.0D0
      DO 100 J = NGY, 1, -1
         WRITE (NOUT,*)
         WRITE (NOUT,99999) J, (FEXACT(X(I+(J-1)*NGX),Y(I+(J-1)*NGX)
     +     ),I=1,NGX)
         WRITE (NOUT,99999) J, (U(I+(J-1)*NGX),I=1,NGX)
         DO 80 I = 1, NGX
            RMSERR = RMSERR + (FEXACT(X(I+(J-1)*NGX),Y(I+(J-1)*NGX))
     +               -U(I+(J-1)*NGX))**2
   80    CONTINUE
  100 CONTINUE
      RMSERR = SQRT(RMSERR/DBLE(NGX*NGY))
      WRITE (NOUT,*)
      WRITE (NOUT,99997) 'Number of Iterations = ', NUMIT
      WRITE (NOUT,99996) 'RMS Error = ', RMSERR
*
*     Now discretize and solve the equations using upwind differences
*
      IFAIL = -1
*
      CALL D03EEF(XMIN,XMAX,YMIN,YMAX,PDEF,BNDY,NGX,NGY,LDA,A,RHS,
     +            'Upwind',IFAIL)
*
      IFAIL = -1
*
*     Set the initial guess to zero
*
      DO 120 I = 1, NGX*NGY
         UB(I) = 0.0D0
  120 CONTINUE
*
      CALL D03EDF(NGX,NGY,LDA,A,RHS,UB,MAXIT,ACC,US,U,IOUT,NUMIT,IFAIL)
*
*     Print the solution
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Exact solution above computed solution'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) '  I/J', (I,I=1,NGX)
      RMSERR = 0.0D0
      DO 160 J = NGY, 1, -1
         WRITE (NOUT,*)
         WRITE (NOUT,99999) J, (FEXACT(X(I+(J-1)*NGX),Y(I+(J-1)*NGX)
     +     ),I=1,NGX)
         WRITE (NOUT,99999) J, (U(I+(J-1)*NGX),I=1,NGX)
         DO 140 I = 1, NGX
            RMSERR = RMSERR + (FEXACT(X(I+(J-1)*NGX),Y(I+(J-1)*NGX))
     +               -U(I+(J-1)*NGX))**2
  140    CONTINUE
  160 CONTINUE
      RMSERR = SQRT(RMSERR/DBLE(NGX*NGY))
      WRITE (NOUT,*)
      WRITE (NOUT,99997) 'Number of Iterations = ', NUMIT
      WRITE (NOUT,99996) 'RMS Error = ', RMSERR
      STOP
*
99999 FORMAT (1X,I3,2X,10F7.3,:/(6X,10F7.3))
99998 FORMAT (1X,A,10I7,:/(6X,10I7))
99997 FORMAT (1X,A,I3)
99996 FORMAT (1X,A,1P,D10.2)
      END
*
      SUBROUTINE PDEF(X,Y,ALPHA,BETA,GAMMA,DELTA,EPSLON,PHI,PSI)
*     .. Scalar Arguments ..
      DOUBLE PRECISION ALPHA, BETA, DELTA, EPSLON, GAMMA, PHI, PSI, X, Y
*     .. Arrays in Common ..
      DOUBLE PRECISION USER(6)
*     .. Intrinsic Functions ..
      INTRINSIC       COS, SIN
*     .. Common blocks ..
      COMMON          /BLOCK1/USER
*     .. Executable Statements ..
      ALPHA = USER(1)
      BETA = USER(2)
      GAMMA = USER(3)
      DELTA = USER(4)
      EPSLON = USER(5)
      PHI = USER(6)
*
      PSI = (-ALPHA-GAMMA+PHI)*SIN(X)*SIN(Y) + BETA*COS(X)*COS(Y) +
     +      DELTA*COS(X)*SIN(Y) + EPSLON*SIN(X)*COS(Y)
*
      RETURN
      END
*
      SUBROUTINE BNDY(X,Y,A,B,C,IBND)
*     .. Parameters ..
      INTEGER         BOTTOM, RIGHT, TOP, LEFT
      PARAMETER       (BOTTOM=0,RIGHT=1,TOP=2,LEFT=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION A, B, C, X, Y
      INTEGER         IBND
*     .. Intrinsic Functions ..
      INTRINSIC       SIN
*     .. Executable Statements ..
      IF (IBND.EQ.TOP .OR. IBND.EQ.RIGHT) THEN
*
*        Solution prescribed
*
         A = 1.0D0
         B = 0.0D0
         C = SIN(X)*SIN(Y)
      ELSE IF (IBND.EQ.BOTTOM) THEN
*
*        Derivative prescribed
*
         A = 0.0D0
         B = 1.0D0
         C = -SIN(X)
      ELSE IF (IBND.EQ.LEFT) THEN
*
*        Derivative prescribed
*
         A = 0.0D0
         B = 1.0D0
         C = -SIN(Y)
      END IF
*
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION FEXACT(X,Y)
*     .. Scalar Arguments ..
      DOUBLE PRECISION                 X, Y
*     .. Intrinsic Functions ..
      INTRINSIC                        SIN
*     .. Executable Statements ..
      FEXACT = SIN(X)*SIN(Y)
      RETURN
      END
