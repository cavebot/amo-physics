*     D02GAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, MNP, LW, LIW
      PARAMETER        (N=3,MNP=40,LW=MNP*(3*N*N+6*N+2)+4*N*N+4*N,
     +                 LIW=MNP*(2*N+1)+N*N+4*N+2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      DOUBLE PRECISION BETA
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, TOL
      INTEGER          I, IFAIL, J, K, NP
*     .. Local Arrays ..
      DOUBLE PRECISION U(N,2), V(N,2), W(LW), X(MNP), Y(N,MNP)
      INTEGER          IW(LIW)
*     .. External Subroutines ..
      EXTERNAL         D02GAF, FCN, X04ABF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Common blocks ..
      COMMON           BETA
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02GAF Example Program Results'
      TOL = 1.0D-3
      NP = 26
      A = 0.0D0
      B = 10.0D0
      CALL X04ABF(1,NOUT)
      BETA = 0.0D0
      DO 40 I = 1, N
         DO 20 J = 1, 2
            U(I,J) = 0.0D0
            V(I,J) = 0.0D0
   20    CONTINUE
   40 CONTINUE
      V(3,1) = 1.0D0
      V(1,2) = 1.0D0
      V(3,2) = 1.0D0
      U(2,2) = 1.0D0
      U(1,2) = B
      X(1) = A
      DO 60 I = 2, NP - 1
         X(I) = (DBLE(NP-I)*A+DBLE(I-1)*B)/DBLE(NP-1)
   60 CONTINUE
      X(NP) = B
      DO 80 K = 1, 2
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Problem with BETA = ', BETA
*        * Set IFAIL to 111 to obtain monitoring information *
         IFAIL = 11
*
         CALL D02GAF(U,V,N,A,B,TOL,FCN,MNP,X,Y,NP,W,LW,IW,LIW,IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.EQ.3) THEN
            WRITE (NOUT,*)
            IF (IFAIL.EQ.3) WRITE (NOUT,99996) ' IFAIL = ', IFAIL
            WRITE (NOUT,99998) 'Solution on final mesh of ', NP,
     +        ' points'
            WRITE (NOUT,*)
     +        '       X(I)        Y1(I)        Y2(I)        Y3(I)'
            WRITE (NOUT,99997) (X(I),(Y(J,I),J=1,N),I=1,NP)
            BETA = BETA + 0.2D0
         ELSE
            STOP
         END IF
   80 CONTINUE
      STOP
*
99999 FORMAT (1X,A,F7.2)
99998 FORMAT (1X,A,I2,A)
99997 FORMAT (1X,F11.3,3F13.4)
99996 FORMAT (1X,A,I3)
      END
*
      SUBROUTINE FCN(X,Y,F)
*     .. Parameters ..
      INTEGER        N
      PARAMETER      (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), Y(N)
*     .. Scalars in Common ..
      DOUBLE PRECISION BETA
*     .. Common blocks ..
      COMMON         BETA
*     .. Executable Statements ..
      F(1) = Y(2)
      F(2) = Y(3)
      F(3) = -Y(1)*Y(3) - BETA*(1.0D0-Y(2)*Y(2))
      RETURN
      END
