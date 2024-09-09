*     D02EHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, IW
      PARAMETER        (N=3,IW=(12+N)*N+50)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION HMAX, TOL, X, XEND
      INTEGER          I, IFAIL, IRELAB, J, MPED
*     .. Local Arrays ..
      DOUBLE PRECISION W(IW), Y(N)
*     .. External Functions ..
      DOUBLE PRECISION G
      EXTERNAL         G
*     .. External Subroutines ..
      EXTERNAL         D02EHF, D02EJY, FCN, PEDERV
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02EHF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Calculating Jacobian internally'
      XEND = 10.0D0
      HMAX = 0.0D0
      IRELAB = 2
      MPED = 0
      DO 20 J = 3, 4
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99997) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         IFAIL = 0
*
         CALL D02EHF(X,XEND,N,Y,TOL,IRELAB,HMAX,FCN,MPED,D02EJY,G,W,IW,
     +               IFAIL)
*
         WRITE (NOUT,99999) '  Root of Y(1)-0.9 at', X
         WRITE (NOUT,99998) '  Solution is', (Y(I),I=1,N)
         IF (TOL.LT.0.0D0) WRITE (NOUT,*)
     +       '  Over one-third steps controlled by HMAX'
   20 CONTINUE
      MPED = 1
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Calculating Jacobian by PEDERV'
      DO 40 J = 3, 4
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99997) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         IFAIL = 0
*
         CALL D02EHF(X,XEND,N,Y,TOL,IRELAB,HMAX,FCN,MPED,PEDERV,G,W,IW,
     +               IFAIL)
*
         WRITE (NOUT,99999) '  Root of Y(1)-0.9 at', X
         WRITE (NOUT,99998) '  Solution is', (Y(I),I=1,3)
         IF (TOL.LT.0.0D0) WRITE (NOUT,*)
     +       '  Over one-third steps controlled by HMAX'
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,A,F7.3)
99998 FORMAT (1X,A,3F13.5)
99997 FORMAT (1X,A,D8.1)
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
