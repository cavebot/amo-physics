*     D02GBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, MNP, LW, LIW
      PARAMETER        (N=2,MNP=70,LW=MNP*(3*N*N+5*N+2)+3*N*N+5*N,
     +                 LIW=MNP*(2*N+1)+N)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      DOUBLE PRECISION EPS
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, TOL
      INTEGER          I, IFAIL, J, NP
*     .. Local Arrays ..
      DOUBLE PRECISION C(N,N), D(N,N), GAM(N), W(LW), X(MNP), Y(N,MNP)
      INTEGER          IW(LIW)
*     .. External Subroutines ..
      EXTERNAL         D02GBF, FCNF, FCNG, X04ABF
*     .. Common blocks ..
      COMMON           EPS
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02GBF Example Program Results'
      TOL = 1.0D-3
      NP = 0
      A = 0.0D0
      B = 1.0D0
      CALL X04ABF(1,NOUT)
      DO 40 I = 1, N
         GAM(I) = 0.0D0
         DO 20 J = 1, N
            C(I,J) = 0.0D0
            D(I,J) = 0.0D0
   20    CONTINUE
   40 CONTINUE
      C(1,1) = 1.0D0
      D(2,1) = 1.0D0
      GAM(2) = 1.0D0
      DO 60 I = 1, 2
         EPS = 10.0D0**(-I)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Problem with epsilon = ', EPS
*        * Set IFAIL to 111 to obtain monitoring information *
         IFAIL = 11
*
         CALL D02GBF(A,B,N,TOL,FCNF,FCNG,C,D,GAM,MNP,X,Y,NP,W,LW,IW,LIW,
     +               IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,99998) 'Approximate solution on final mesh of ',
     +        NP, ' points'
            WRITE (NOUT,*) '       X(I)     Y(1,I)'
            WRITE (NOUT,99997) (X(J),Y(1,J),J=1,NP)
         ELSE
            WRITE (NOUT,99996) ' IFAIL = ', IFAIL
            STOP
         END IF
   60 CONTINUE
   80 STOP
*
99999 FORMAT (1X,A,D10.2)
99998 FORMAT (1X,A,I2,A)
99997 FORMAT (1X,2F11.4)
99996 FORMAT (1X,A,I3)
      END
*
      SUBROUTINE FCNF(X,F)
*     .. Parameters ..
      INTEGER         N
      PARAMETER       (N=2)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
*     .. Array Arguments ..
      DOUBLE PRECISION F(N,N)
*     .. Scalars in Common ..
      DOUBLE PRECISION EPS
*     .. Common blocks ..
      COMMON          EPS
*     .. Executable Statements ..
      F(1,1) = 0.0D0
      F(1,2) = 1
      F(2,1) = 0.0D0
      F(2,2) = -1.0D0/EPS
      RETURN
      END
*
      SUBROUTINE FCNG(X,G)
*     .. Parameters ..
      INTEGER         N
      PARAMETER       (N=2)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
*     .. Array Arguments ..
      DOUBLE PRECISION G(N)
*     .. Executable Statements ..
      G(1) = 0.0D0
      G(2) = 0.0D0
      RETURN
      END
