*     D02EJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, IW
      PARAMETER        (N=3,IW=(12+N)*N+50)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      DOUBLE PRECISION H, XEND
      INTEGER          K
*     .. Local Scalars ..
      DOUBLE PRECISION TOL, X
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION W(IW), Y(N)
*     .. External Functions ..
      DOUBLE PRECISION D02EJW, G
      EXTERNAL         D02EJW, G
*     .. External Subroutines ..
      EXTERNAL         D02EJF, D02EJX, D02EJY, FCN, OUT, PEDERV
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Common blocks ..
      COMMON           XEND, H, K
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02EJF Example Program Results'
      XEND = 10.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 1: calculating Jacobian internally,'
      WRITE (NOUT,*) ' intermediate output, root-finding'
      DO 20 J = 3, 4
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         K = 4
         H = (XEND-X)/DBLE(K+1)
         WRITE (NOUT,*) '     X         Y(1)         Y(2)         Y(3)'
         IFAIL = 0
*
         CALL D02EJF(X,XEND,N,Y,FCN,D02EJY,TOL,'Default',OUT,G,W,IW,
     +               IFAIL)
*
         WRITE (NOUT,99998) '  Root of Y(1)-0.9 at', X
         WRITE (NOUT,99997) '  Solution is', (Y(I),I=1,N)
         IF (TOL.LT.0.0D0) WRITE (NOUT,*) '  Range too short for TOL'
   20 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 2: calculating Jacobian by PEDERV,'
      WRITE (NOUT,*) ' intermediate output, root-finding'
      DO 40 J = 3, 4
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         K = 4
         H = (XEND-X)/DBLE(K+1)
         WRITE (NOUT,*) '     X         Y(1)         Y(2)         Y(3)'
         IFAIL = 0
*
         CALL D02EJF(X,XEND,N,Y,FCN,PEDERV,TOL,'Default',OUT,G,W,IW,
     +               IFAIL)
*
         WRITE (NOUT,99998) '  Root of Y(1)-0.9 at', X
         WRITE (NOUT,99997) '  Solution is', (Y(I),I=1,N)
         IF (TOL.LT.0.0D0) WRITE (NOUT,*) '  Range too short for TOL'
   40 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 3: calculating Jacobian internally,'
      WRITE (NOUT,*) ' no intermediate output, root-finding'
      DO 60 J = 3, 4
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         IFAIL = 0
*
         CALL D02EJF(X,XEND,N,Y,FCN,D02EJY,TOL,'Default',D02EJX,G,W,IW,
     +               IFAIL)
*
         WRITE (NOUT,99998) '  Root of Y(1)-0.9 at', X
         WRITE (NOUT,99997) '  Solution is', (Y(I),I=1,N)
         IF (TOL.LT.0.0D0) WRITE (NOUT,*) '  Range too short for TOL'
   60 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 4: calculating Jacobian internally,'
      WRITE (NOUT,*) ' intermediate output, no root-finding'
      DO 80 J = 3, 4
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         K = 4
         H = (XEND-X)/DBLE(K+1)
         WRITE (NOUT,*) '     X         Y(1)         Y(2)         Y(3)'
         IFAIL = 0
*
         CALL D02EJF(X,XEND,N,Y,FCN,D02EJY,TOL,'Default',OUT,D02EJW,W,
     +               IW,IFAIL)
*
         IF (TOL.LT.0.0D0) WRITE (NOUT,*) '  Range too short for TOL'
   80 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 5: calculating Jacobian internally,'
      WRITE (NOUT,*)
     +  ' no intermediate output, no root-finding (integrate to XEND)'
      DO 100 J = 3, 4
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         WRITE (NOUT,*) '     X         Y(1)         Y(2)         Y(3)'
         WRITE (NOUT,99996) X, (Y(I),I=1,N)
         IFAIL = 0
*
         CALL D02EJF(X,XEND,N,Y,FCN,D02EJY,TOL,'Default',D02EJX,D02EJW,
     +               W,IW,IFAIL)
*
         WRITE (NOUT,99996) X, (Y(I),I=1,N)
         IF (TOL.LT.0.0D0) WRITE (NOUT,*) '  Range too short for TOL'
  100 CONTINUE
      STOP
*
99999 FORMAT (1X,A,D8.1)
99998 FORMAT (1X,A,F7.3)
99997 FORMAT (1X,A,3F13.5)
99996 FORMAT (1X,F8.2,3F13.5)
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
      DOUBLE PRECISION FUNCTION G(T,Y)
*     .. Parameters ..
      INTEGER                     N
      PARAMETER                   (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION            T
*     .. Array Arguments ..
      DOUBLE PRECISION            Y(N)
*     .. Executable Statements ..
      G = Y(1) - 0.9D0
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
