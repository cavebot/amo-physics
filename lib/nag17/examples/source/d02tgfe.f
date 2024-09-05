*     D02TGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, MIMAX, K1, IC, KP, LSUM, LW, LIW
      PARAMETER        (N=2,MIMAX=8,K1=MIMAX+1,IC=K1,KP=15,LSUM=3,
     +                 LW=2*(N*KP+LSUM)*(N*K1+1)+7*N*K1,LIW=N*K1)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      DOUBLE PRECISION X0, X1
*     .. Arrays in Common ..
      DOUBLE PRECISION B(K1,N)
*     .. Local Scalars ..
      DOUBLE PRECISION EMAX, X
      INTEGER          I, IA1, IFAIL, ITER, J, K
*     .. Local Arrays ..
      DOUBLE PRECISION C(IC,N), W(LW), Y(N)
      INTEGER          IW(LIW), L(N), M(N)
*     .. External Subroutines ..
      EXTERNAL         BDYC, COEFF, D02TGF, E02AKF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX, DBLE
*     .. Common blocks ..
      COMMON           /ABC/B, X0, X1
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02TGF Example Program Results'
      X0 = -1.0D0
      X1 = 1.0D0
      M(1) = 1
      M(2) = 2
      L(1) = 1
      L(2) = 2
      DO 40 J = 1, N
         DO 20 I = 1, K1
            B(I,J) = 0.0D0
   20    CONTINUE
   40 CONTINUE
      B(1,2) = 6.0D0
      ITER = 0
   60 ITER = ITER + 1
      WRITE (NOUT,*)
      WRITE (NOUT,99999) ' Iteration', ITER,
     +  ' Chebyshev coefficients are'
      IFAIL = 1
*
      CALL D02TGF(N,M,L,X0,X1,K1,KP,C,IC,COEFF,BDYC,W,LW,IW,LIW,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         DO 80 J = 1, N
            WRITE (NOUT,99998) (C(I,J),I=1,K1)
   80    CONTINUE
         EMAX = 0.0D0
         DO 120 J = 1, N
            DO 100 I = 1, K1
               EMAX = MAX(EMAX,ABS(C(I,J)-B(I,J)))
               B(I,J) = C(I,J)
  100       CONTINUE
  120    CONTINUE
         IF (EMAX.LT.1.0D-5) THEN
            K = 9
            IA1 = 1
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Solution evaluated at', K,
     +        '  equally spaced points'
            WRITE (NOUT,*)
            WRITE (NOUT,99997) '      X ', (J,J=1,N)
            DO 160 I = 1, K
               X = (X0*DBLE(K-I)+X1*DBLE(I-1))/DBLE(K-1)
               DO 140 J = 1, N
                  IFAIL = 0
*
                  CALL E02AKF(K1,X0,X1,C(1,J),IA1,K1,X,Y(J),IFAIL)
*
  140          CONTINUE
               WRITE (NOUT,99996) X, (Y(J),J=1,N)
  160       CONTINUE
         ELSE
            GO TO 60
         END IF
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'D02TGF fails with IFAIL =', IFAIL
      END IF
      STOP
*
99999 FORMAT (1X,A,I3,A)
99998 FORMAT (1X,9F8.4)
99997 FORMAT (1X,A,2('      Y(',I1,')'))
99996 FORMAT (1X,3F10.4)
      END
*
      SUBROUTINE COEFF(X,I,A,IA,IA1,RHS)
*     .. Parameters ..
      INTEGER          N, MIMAX, K1
      PARAMETER        (N=2,MIMAX=8,K1=MIMAX+1)
*     .. Scalar Arguments ..
      DOUBLE PRECISION RHS, X
      INTEGER          I, IA, IA1
*     .. Array Arguments ..
      DOUBLE PRECISION A(IA,IA1)
*     .. Scalars in Common ..
      DOUBLE PRECISION X0, X1
*     .. Arrays in Common ..
      DOUBLE PRECISION B(K1,N)
*     .. Local Scalars ..
      DOUBLE PRECISION Z1, Z2
      INTEGER          IFAIL
*     .. External Subroutines ..
      EXTERNAL         E02AKF
*     .. Common blocks ..
      COMMON           /ABC/B, X0, X1
*     .. Executable Statements ..
      IF (I.LE.1) THEN
         IA1 = 1
         IFAIL = 0
*
         CALL E02AKF(K1,X0,X1,B(1,1),IA1,K1,X,Z1,IFAIL)
         CALL E02AKF(K1,X0,X1,B(1,2),IA1,K1,X,Z2,IFAIL)
*
         A(1,1) = Z2*Z2 - 1.0D0
         A(1,2) = 2.0D0
         A(2,1) = 2.0D0*Z1*Z2 + 1.0D0
         RHS = 2.0D0*Z1*Z2*Z2
      ELSE
         A(1,2) = -1.0D0
         A(2,3) = 2.0D0
      END IF
      RETURN
      END
*
      SUBROUTINE BDYC(X,I,J,A,IA,IA1,RHS)
*     .. Scalar Arguments ..
      DOUBLE PRECISION RHS, X
      INTEGER         I, IA, IA1, J
*     .. Array Arguments ..
      DOUBLE PRECISION A(IA,IA1)
*     .. Executable Statements ..
      X = -1.0D0
      A(I,J) = 1.0D0
      IF (I.EQ.2 .AND. J.EQ.1) RHS = 3.0D0
      RETURN
      END
