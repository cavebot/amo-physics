*     D02JAF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          N, K1MAX, KPMAX, LW
      PARAMETER        (N=2,K1MAX=8,KPMAX=15,LW=2*(KPMAX+N)*(K1MAX+1)
     +                 +7*K1MAX)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION X, X0, X1, Y
      INTEGER          I, IA1, IFAIL, K1, KP, M
*     .. Local Arrays ..
      DOUBLE PRECISION C(K1MAX), W(LW)
      INTEGER          IW(K1MAX)
*     .. External Functions ..
      DOUBLE PRECISION CF
      EXTERNAL         CF
*     .. External Subroutines ..
      EXTERNAL         BC, D02JAF, E02AKF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02JAF Example Program Results'
      X0 = -1.0D0
      X1 = 1.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' KP  K1   Chebyshev coefficients'
      DO 40 KP = 10, KPMAX, 5
         DO 20 K1 = 4, K1MAX, 2
            IFAIL = 1
*
            CALL D02JAF(N,CF,BC,X0,X1,K1,KP,C,W,LW,IW,IFAIL)
*
            IF (IFAIL.NE.0) THEN
               WRITE (NOUT,99999) KP, K1, '  D02JAF fails with IFAIL =',
     +           IFAIL
               STOP
            ELSE
               WRITE (NOUT,99998) KP, K1, (C(I),I=1,K1)
            END IF
   20    CONTINUE
   40 CONTINUE
      K1 = 8
      M = 9
      IA1 = 1
      WRITE (NOUT,*)
      WRITE (NOUT,99997) 'Last computed solution evaluated at', M,
     +  ' equally spaced points'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '      X         Y'
      DO 60 I = 1, M
         X = (X0*DBLE(M-I)+X1*DBLE(I-1))/DBLE(M-1)
         IFAIL = 0
*
         CALL E02AKF(K1,X0,X1,C,IA1,K1MAX,X,Y,IFAIL)
*
         WRITE (NOUT,99996) X, Y
   60 CONTINUE
      STOP
*
99999 FORMAT (1X,2(I3,1X),A,I4)
99998 FORMAT (1X,2(I3,1X),8F8.4)
99997 FORMAT (1X,A,I3,A)
99996 FORMAT (1X,2F10.4)
      END
*
      DOUBLE PRECISION FUNCTION CF(J,X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             X
      INTEGER                      J
*     .. Executable Statements ..
      IF (J.EQ.2) THEN
         CF = 0.0D0
      ELSE
         CF = 1.0D0
      END IF
      RETURN
      END
*
      SUBROUTINE BC(I,J,RHS)
*     .. Scalar Arguments ..
      DOUBLE PRECISION RHS
      INTEGER       I, J
*     .. Executable Statements ..
      RHS = 0.0D0
      IF (I.EQ.1) THEN
         J = 1
      ELSE
         J = -1
      END IF
      RETURN
      END
