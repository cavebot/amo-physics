*     D02HAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
*     N.B the definition of IW must be changed for N.GT.11
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, IW, M1
      PARAMETER        (N=3,IW=3*N+17+11,M1=6)
*     .. Local Scalars ..
      DOUBLE PRECISION TOL, X, X1
      INTEGER          I, IFAIL, J, L
*     .. Local Arrays ..
      DOUBLE PRECISION U(N,2), V(N,2), W(N,IW), Y(N,M1)
*     .. External Subroutines ..
      EXTERNAL         D02HAF, DERIV, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02HAF Example Program Results'
      CALL X04ABF(1,NOUT)
      DO 40 L = 3, 4
         TOL = 5.0D0*10.0D0**(-L)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Results with TOL = ', TOL
         U(1,1) = 0.0D0
         V(1,1) = 0.0D0
         U(1,2) = 0.0D0
         V(1,2) = 0.0D0
         U(2,1) = 0.5D0
         V(2,1) = 0.0D0
         U(2,2) = 0.46D0
         V(2,2) = 1.0D0
         U(3,1) = 1.15D0
         V(3,1) = 1.0D0
         U(3,2) = -1.2D0
         V(3,2) = 1.0D0
         X = 0.0D0
         X1 = 5.0D0
*        * Set IFAIL to 111 to obtain monitoring information *
         IFAIL = 11
*
         CALL D02HAF(U,V,N,X,X1,TOL,DERIV,Y,M1,W,IW,IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,*) ' X-value and final solution'
            DO 20 I = 1, M1
               WRITE (NOUT,99998) I - 1, (Y(J,I),J=1,N)
   20       CONTINUE
         ELSE
            WRITE (NOUT,99997) ' IFAIL =', IFAIL
         END IF
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,A,D10.3)
99998 FORMAT (1X,I3,3F10.4)
99997 FORMAT (1X,A,I4)
      END
*
      SUBROUTINE DERIV(X,Z,G)
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
*     .. Array Arguments ..
      DOUBLE PRECISION G(N), Z(N)
*     .. Intrinsic Functions ..
      INTRINSIC        COS, TAN
*     .. Executable Statements ..
      G(1) = TAN(Z(3))
      G(2) = -0.032D0*TAN(Z(3))/Z(2) - 0.02D0*Z(2)/COS(Z(3))
      G(3) = -0.032D0/Z(2)**2
      RETURN
      END
