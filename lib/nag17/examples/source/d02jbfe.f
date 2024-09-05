*     D02JBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, K1MAX, KPMAX, IC, LW, LIW
      PARAMETER        (N=2,K1MAX=8,KPMAX=15,IC=K1MAX,LW=2*N*(KPMAX+1)
     +                 *(N*K1MAX+1)+7*N*K1MAX,LIW=N*(K1MAX+2))
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION X, X0, X1
      INTEGER          I, IA1, IFAIL, J, K1, KP, M
*     .. Local Arrays ..
      DOUBLE PRECISION C(IC,N), W(LW), Y(N)
      INTEGER          IW(LIW)
*     .. External Functions ..
      DOUBLE PRECISION CF
      EXTERNAL         CF
*     .. External Subroutines ..
      EXTERNAL         BC, D02JBF, E02AKF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02JBF Example Program Results'
      X0 = -1.0D0
      X1 = 1.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' KP  K1   Chebyshev coefficients'
      DO 60 KP = 10, KPMAX, 5
         DO 40 K1 = 4, K1MAX, 2
            IFAIL = 1
*
            CALL D02JBF(N,CF,BC,X0,X1,K1,KP,C,IC,W,LW,IW,LIW,IFAIL)
*
            IF (IFAIL.NE.0) THEN
               WRITE (NOUT,99999) KP, K1, '  D02JBF fails with IFAIL =',
     +           IFAIL
               STOP
            ELSE
               WRITE (NOUT,99998) KP, K1, (C(I,1),I=1,K1)
               DO 20 J = 2, N
                  WRITE (NOUT,99997) (C(I,J),I=1,K1)
   20          CONTINUE
               WRITE (NOUT,*)
            END IF
   40    CONTINUE
   60 CONTINUE
      K1 = 8
      M = 9
      IA1 = 1
      WRITE (NOUT,99996) 'Last computed solution evaluated at', M,
     +  '  equally spaced points'
      WRITE (NOUT,*)
      WRITE (NOUT,99995) '      X ', (J,J=1,N)
      DO 100 I = 1, M
         X = (X0*DBLE(M-I)+X1*DBLE(I-1))/DBLE(M-1)
         DO 80 J = 1, N
            IFAIL = 0
*
            CALL E02AKF(K1,X0,X1,C(1,J),IA1,IC,X,Y(J),IFAIL)
*
   80    CONTINUE
         WRITE (NOUT,99994) X, (Y(J),J=1,N)
  100 CONTINUE
      STOP
*
99999 FORMAT (1X,2(I3,1X),A,I4)
99998 FORMAT (1X,2(I3,1X),8F8.4)
99997 FORMAT (9X,8F8.4)
99996 FORMAT (1X,A,I3,A)
99995 FORMAT (1X,A,2('      Y(',I1,')'))
99994 FORMAT (1X,3F10.4)
      END
*
      DOUBLE PRECISION FUNCTION CF(I,J,X)
*     .. Parameters ..
      INTEGER                      N
      PARAMETER                    (N=2)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             X
      INTEGER                      I, J
*     .. Local Arrays ..
      DOUBLE PRECISION             A(N,N), R(N)
*     .. Data statements ..
      DATA                         A(1,1), A(2,1), A(1,2), A(2,2)/0.0D0,
     +                             -1.0D0, 1.0D0, 0.0D0/
      DATA                         R(1), R(2)/0.0D0, 1.0D0/
*     .. Executable Statements ..
      IF (J.GT.0) CF = A(I,J)
      IF (J.EQ.0) CF = R(I)
      RETURN
      END
*
      SUBROUTINE BC(I,J,RHS)
*     .. Scalar Arguments ..
      DOUBLE PRECISION RHS
      INTEGER       I, J
*     .. Executable Statements ..
      RHS = 0.0D0
      IF (I.GT.1) THEN
         J = -1
      ELSE
         J = 1
      END IF
      RETURN
      END
