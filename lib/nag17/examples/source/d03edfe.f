*     D03EDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      DOUBLE PRECISION ALPHA
      PARAMETER        (ALPHA=1.7D0)
      INTEGER          LEVELS, NGX, NGY, LDA
      PARAMETER        (LEVELS=3,NGX=2**LEVELS+1,NGY=NGX,LDA=4*(NGX+1)
     +                 *(NGY+1)/3)
*     .. Local Scalars ..
      DOUBLE PRECISION ACC, HX, HY
      INTEGER          I, IFAIL, IOUT, IX, IY, J, K, MAXIT, NUMIT
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,7), RHS(LDA), U(LDA), UB(NGX*NGY), US(LDA)
*     .. External Subroutines ..
      EXTERNAL         D03EDF, X04ABF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03EDF Example Program Results'
      WRITE (NOUT,*)
      ACC = 1.0D-4
      CALL X04ABF(1,NOUT)
      MAXIT = 15
*     ** Set IOUT.GE.2 to obtain intermediate output **
      IOUT = 0
      HX = 1.0D0/DBLE(NGX+1)
      HY = 1.0D0/DBLE(NGY+1)
      WRITE (NOUT,99999) 'NGX = ', NGX, '  NGY = ', NGY, '  ACC =', ACC,
     +  '  MAXIT = ', MAXIT
*     Set up operator, right-hand side and initial guess for
*     step-lengths HX and HY
      DO 40 J = 1, NGY
         DO 20 I = 1, NGX
            K = (J-1)*NGX + I
            A(K,1) = 1.0D0 - 0.5D0*ALPHA
            A(K,2) = 0.5D0*ALPHA
            A(K,3) = 1.0D0 - 0.5D0*ALPHA
            A(K,4) = -4.0D0 + ALPHA
            A(K,5) = 1.0D0 - 0.5D0*ALPHA
            A(K,6) = 0.5D0*ALPHA
            A(K,7) = 1.0D0 - 0.5D0*ALPHA
            RHS(K) = -4.0D0*HX*HY
            UB(K) = 0.0D0
   20    CONTINUE
   40 CONTINUE
*     Correction for the boundary conditions
*     Horizontal boundaries --
      DO 60 I = 2, NGX - 1
*        Boundary condition on Y=0 -- U=0
         IX = I
         A(IX,1) = 0.0D0
         A(IX,2) = 0.0D0
*        Boundary condition on Y=1 -- U=0
         IX = I + (NGY-1)*NGX
         A(IX,6) = 0.0D0
         A(IX,7) = 0.0D0
   60 CONTINUE
*     Vertical boundaries --
      DO 80 J = 2, NGY - 1
*        Boundary condition on X=0 -- U=0
         IY = (J-1)*NGX + 1
         A(IY,3) = 0.0D0
         A(IY,6) = 0.0D0
*        Boundary condition on X=1 -- U=1
         IY = J*NGX
         RHS(IY) = RHS(IY) - A(IY,5) - A(IY,2)
         A(IY,2) = 0.0D0
         A(IY,5) = 0.0D0
   80 CONTINUE
*     Now the four corners --
*     Bottom left corner
      K = 1
      A(K,1) = 0.0D0
      A(K,2) = 0.0D0
      A(K,3) = 0.0D0
      A(K,6) = 0.0D0
*     Top left corner
      K = 1 + (NGY-1)*NGX
      A(K,3) = 0.0D0
      A(K,6) = 0.0D0
      A(K,7) = 0.0D0
*     Bottom right corner
*     Use average value at discontinuity ( = 0.5 )
      K = NGX
      RHS(K) = RHS(K) - A(K,2)*0.5D0 - A(K,5)
      A(K,1) = 0.0D0
      A(K,2) = 0.0D0
      A(K,5) = 0.0D0
*     Top right corner
      K = NGX*NGY
      RHS(K) = RHS(K) - A(K,2) - A(K,5)
      A(K,2) = 0.0D0
      A(K,5) = 0.0D0
      A(K,6) = 0.0D0
      A(K,7) = 0.0D0
*     Solve the equations
      IFAIL = 0
*
      CALL D03EDF(NGX,NGY,LDA,A,RHS,UB,MAXIT,ACC,US,U,IOUT,NUMIT,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99998) 'Residual norm =', US(1)
      WRITE (NOUT,99997) 'Number of iterations =', NUMIT
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Solution'
      WRITE (NOUT,*)
      WRITE (NOUT,99996) '  I/J', (I,I=1,NGX)
      DO 100 J = 1, NGY
         WRITE (NOUT,99995) J, (U(I+(J-1)*NGX),I=1,NGX)
  100 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I3,A,I3,A,1P,D10.2,A,I3)
99998 FORMAT (1X,A,1P,D12.2)
99997 FORMAT (1X,A,I5)
99996 FORMAT (1X,A,10I7,:)
99995 FORMAT (1X,I3,2X,10F7.3,:)
      END
