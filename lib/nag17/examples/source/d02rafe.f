*     D02RAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, MNP, IY, LWORK, LIWORK
      PARAMETER        (N=3,MNP=40,IY=N,LWORK=MNP*(3*N*N+6*N+2)
     +                 +4*N*N+3*N,LIWORK=MNP*(2*N+1)+N)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DELEPS, TOL
      INTEGER          I, IFAIL, IJAC, INIT, J, NP, NUMBEG, NUMMIX
*     .. Local Arrays ..
      DOUBLE PRECISION ABT(N), WORK(LWORK), X(MNP), Y(IY,MNP)
      INTEGER          IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         D02RAF, FCN, G, JACEPS, JACGEP, JACOBF, JACOBG,
     +                 X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02RAF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Calculation using analytic Jacobians'
      CALL X04ABF(1,NOUT)
      TOL = 1.0D-4
      NP = 17
      NUMBEG = 2
      NUMMIX = 0
      X(1) = 0.0D0
      X(NP) = 10.0D0
      INIT = 0
      DELEPS = 0.1D0
      IJAC = 1
*     * Set IFAIL to 111 to obtain monitoring information *
      IFAIL = 11
*
      CALL D02RAF(N,MNP,NP,NUMBEG,NUMMIX,TOL,INIT,X,Y,N,ABT,FCN,G,IJAC,
     +            JACOBF,JACOBG,DELEPS,JACEPS,JACGEP,WORK,LWORK,IWORK,
     +            LIWORK,IFAIL)
*
      IF (IFAIL.EQ.0 .OR. IFAIL.EQ.4) THEN
         IF (IFAIL.EQ.4) WRITE (NOUT,99996)
     +       'On exit from D02RAF IFAIL = ', IFAIL
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Solution on final mesh of ', NP, ' points'
         WRITE (NOUT,*)
     +     '      X(I)        Y1(I)        Y2(I)        Y3(I)'
         WRITE (NOUT,99998) (X(J),(Y(I,J),I=1,N),J=1,NP)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Maximum estimated error by components'
         WRITE (NOUT,99997) (ABT(I),I=1,N)
      ELSE
         WRITE (NOUT,99996) 'On exit from D02RAF IFAIL = ', IFAIL
      END IF
   20 STOP
*
99999 FORMAT (1X,A,I2,A)
99998 FORMAT (1X,F10.3,3F13.4)
99997 FORMAT (11X,1P,3D13.2)
99996 FORMAT (1X,A,I3)
      END
*
      SUBROUTINE FCN(X,EPS,Y,F,M)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, X
      INTEGER        M
*     .. Array Arguments ..
      DOUBLE PRECISION F(M), Y(M)
*     .. Executable Statements ..
      F(1) = Y(2)
      F(2) = Y(3)
      F(3) = -Y(1)*Y(3) - 2.0D0*(1.0D0-Y(2)*Y(2))*EPS
      RETURN
      END
*
      SUBROUTINE G(EPS,Y,Z,AL,M)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS
      INTEGER      M
*     .. Array Arguments ..
      DOUBLE PRECISION AL(M), Y(M), Z(M)
*     .. Executable Statements ..
      AL(1) = Y(1)
      AL(2) = Y(2)
      AL(3) = Z(2) - 1.0D0
      RETURN
      END
*
      SUBROUTINE JACEPS(X,EPS,Y,F,M)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  EPS, X
      INTEGER           M
*     .. Array Arguments ..
      DOUBLE PRECISION  F(M), Y(M)
*     .. Executable Statements ..
      F(1) = 0.0D0
      F(2) = 0.0D0
      F(3) = -2.0D0*(1.0D0-Y(2)*Y(2))
      RETURN
      END
*
      SUBROUTINE JACGEP(EPS,Y,Z,AL,M)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  EPS
      INTEGER           M
*     .. Array Arguments ..
      DOUBLE PRECISION  AL(M), Y(M), Z(M)
*     .. Local Scalars ..
      INTEGER           I
*     .. Executable Statements ..
      DO 20 I = 1, M
         AL(I) = 0.0D0
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE JACOBF(X,EPS,Y,F,M)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  EPS, X
      INTEGER           M
*     .. Array Arguments ..
      DOUBLE PRECISION  F(M,M), Y(M)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
      DO 40 I = 1, M
         DO 20 J = 1, M
            F(I,J) = 0.0D0
   20    CONTINUE
   40 CONTINUE
      F(1,2) = 1.0D0
      F(2,3) = 1.0D0
      F(3,1) = -Y(3)
      F(3,2) = 4.0D0*Y(2)*EPS
      F(3,3) = -Y(1)
      RETURN
      END
*
      SUBROUTINE JACOBG(EPS,Y,Z,A,B,M)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  EPS
      INTEGER           M
*     .. Array Arguments ..
      DOUBLE PRECISION  A(M,M), B(M,M), Y(M), Z(M)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
      DO 40 I = 1, M
         DO 20 J = 1, M
            A(I,J) = 0.0D0
            B(I,J) = 0.0D0
   20    CONTINUE
   40 CONTINUE
      A(1,1) = 1.0D0
      A(2,2) = 1.0D0
      B(3,2) = 1.0D0
      RETURN
      END
