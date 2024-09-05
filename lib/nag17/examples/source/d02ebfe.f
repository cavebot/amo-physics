*     D02EBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, IW
      PARAMETER        (N=3,IW=(12+N)*N+50)
*     .. Scalars in Common ..
      DOUBLE PRECISION H, XEND
      INTEGER          I
*     .. Local Scalars ..
      DOUBLE PRECISION TOL, X
      INTEGER          IFAIL, IR, J, MPED
*     .. Local Arrays ..
      DOUBLE PRECISION W(IW), Y(N)
*     .. External Subroutines ..
      EXTERNAL         D02EBF, D02EJY, FCN, OUT, PEDERV
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Common blocks ..
      COMMON           XEND, H, I
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02EBF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Calculating Jacobian internally'
      MPED = 0
      IR = 2
      XEND = 10.0D0
      DO 20 J = 3, 4
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         WRITE (NOUT,*) '     X         Y(1)         Y(2)         Y(3)'
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         I = 4
         H = (XEND-X)/DBLE(I+1)
         IFAIL = 0
*
         CALL D02EBF(X,XEND,N,Y,TOL,IR,FCN,MPED,D02EJY,OUT,W,IW,IFAIL)
*
         IF (TOL.LT.0.0D0) WRITE (NOUT,*) '  Range too short for TOL'
   20 CONTINUE
      MPED = 1
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Calculating Jacobian by PEDERV'
      DO 40 J = 3, 4
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         WRITE (NOUT,*) '     X         Y(1)         Y(2)         Y(3)'
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         I = 4
         H = (XEND-X)/DBLE(I+1)
         IFAIL = 0
*
         CALL D02EBF(X,XEND,N,Y,TOL,IR,FCN,MPED,PEDERV,OUT,W,IW,IFAIL)
*
         IF (TOL.LT.0.0D0) WRITE (NOUT,*) '  Range too short for TOL'
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,A,D8.1)
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
*     .. Executable Statements ..
      F(1) = -0.04D0*Y(1) + 1.0D4*Y(2)*Y(3)
      F(2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)*Y(2)
      F(3) = 3.0D7*Y(2)*Y(2)
      RETURN
      END
*
      SUBROUTINE PEDERV(X,Y,PW)
*     .. Parameters ..
      INTEGER           N
      PARAMETER         (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  X
*     .. Array Arguments ..
      DOUBLE PRECISION  PW(N,N), Y(N)
*     .. Executable Statements ..
      PW(1,1) = -0.04D0
      PW(1,2) = 1.0D4*Y(3)
      PW(1,3) = 1.0D4*Y(2)
      PW(2,1) = 0.04D0
      PW(2,2) = -1.0D4*Y(3) - 6.0D7*Y(2)
      PW(2,3) = -1.0D4*Y(2)
      PW(3,1) = 0.0D0
      PW(3,2) = 6.0D7*Y(2)
      PW(3,3) = 0.0D0
      RETURN
      END
*
      SUBROUTINE OUT(X,Y)
*     .. Parameters ..
      INTEGER        N
      PARAMETER      (N=3)
      INTEGER        NOUT
      PARAMETER      (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
*     .. Array Arguments ..
      DOUBLE PRECISION Y(N)
*     .. Scalars in Common ..
      DOUBLE PRECISION H, XEND
      INTEGER        I
*     .. Local Scalars ..
      INTEGER        J
*     .. Intrinsic Functions ..
      INTRINSIC      DBLE
*     .. Common blocks ..
      COMMON         XEND, H, I
*     .. Executable Statements ..
      WRITE (NOUT,99999) X, (Y(J),J=1,N)
      X = XEND - DBLE(I)*H
      I = I - 1
      RETURN
*
99999 FORMAT (1X,F8.2,3F13.5)
      END
