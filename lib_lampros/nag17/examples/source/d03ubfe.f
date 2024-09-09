*     D03UBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N1, N2, N3, N1M, N2M, NITS
      PARAMETER        (N1=4,N2=5,N3=6,N1M=N1,N2M=N2,NITS=10)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ADEL, APARAM, ARES, DELMAX, DELMN, RESMAX, RESMN,
     +                 ROOT2
      INTEGER          I, IFAIL, IT, J, K
*     .. Local Arrays ..
      DOUBLE PRECISION A(N1M,N2M,N3), B(N1M,N2M,N3), C(N1M,N2M,N3),
     +                 D(N1M,N2M,N3), E(N1M,N2M,N3), F(N1M,N2M,N3),
     +                 G(N1M,N2M,N3), Q(N1M,N2M,N3), R(N1M,N2M,N3),
     +                 T(N1M,N2M,N3), WRKSP1(N1M,N2M,N3),
     +                 WRKSP2(N1M,N2M,N3), WRKSP3(N1M,N2M,N3), X(N1),
     +                 Y(N2), Z(N3)
*     .. External Subroutines ..
      EXTERNAL         D03UBF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, COS, EXP, MAX, DBLE, SQRT
*     .. Data statements ..
      DATA             X(1), X(2), X(3), X(4)/0.0D0, 1.0D0, 3.0D0,
     +                 6.0D0/
      DATA             Y(1), Y(2), Y(3), Y(4), Y(5)/0.0D0, 1.0D0, 3.0D0,
     +                 6.0D0, 10.0D0/
      DATA             Z(1), Z(2), Z(3), Z(4), Z(5), Z(6)/0.0D0, 1.0D0,
     +                 3.0D0, 6.0D0, 10.0D0, 15.0D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03UBF Example Program Results'
      WRITE (NOUT,*)
      ROOT2 = SQRT(2.0D0)
      APARAM = 1.0D0
*     Set up difference equation coefficients, source terms and
*     initial approximation
      DO 60 K = 1, N3
         DO 40 J = 1, N2
            DO 20 I = 1, N1
               IF ((I.NE.1) .AND. (I.NE.N1) .AND. (J.NE.1)
     +             .AND. (J.NE.N2) .AND. (K.NE.1) .AND. (K.NE.N3)) THEN
*                 Specification for internal nodes
                  A(I,J,K) = 2.0D0/((Z(K)-Z(K-1))*(Z(K+1)-Z(K-1)))
                  G(I,J,K) = 2.0D0/((Z(K+1)-Z(K))*(Z(K+1)-Z(K-1)))
                  B(I,J,K) = 2.0D0/((Y(J)-Y(J-1))*(Y(J+1)-Y(J-1)))
                  F(I,J,K) = 2.0D0/((Y(J+1)-Y(J))*(Y(J+1)-Y(J-1)))
                  C(I,J,K) = 2.0D0/((X(I)-X(I-1))*(X(I+1)-X(I-1)))
                  E(I,J,K) = 2.0D0/((X(I+1)-X(I))*(X(I+1)-X(I-1)))
                  D(I,J,K) = -A(I,J,K) - B(I,J,K) - C(I,J,K) - E(I,J,K)
     +                        - F(I,J,K) - G(I,J,K)
                  Q(I,J,K) = 0.0D0
                  T(I,J,K) = 0.0D0
               ELSE
*                 Specification for boundary nodes
                  A(I,J,K) = 0.0D0
                  B(I,J,K) = 0.0D0
                  C(I,J,K) = 0.0D0
                  E(I,J,K) = 0.0D0
                  F(I,J,K) = 0.0D0
                  G(I,J,K) = 0.0D0
                  D(I,J,K) = 0.0D0
                  Q(I,J,K) = EXP((X(I)+1.0D0)/Y(N2))*COS(ROOT2*Y(J)
     +                       /Y(N2))*EXP((-Z(K)-1.0D0)/Y(N2))
                  T(I,J,K) = 0.0D0
               END IF
   20       CONTINUE
   40    CONTINUE
   60 CONTINUE
*     Iterative loop
      WRITE (NOUT,*) 'Iteration      Residual                   Change'
      WRITE (NOUT,*)
     +  '   No      Max.        Mean           Max.      Mean'
      WRITE (NOUT,*)
      DO 200 IT = 1, NITS
         RESMAX = 0.0D0
         RESMN = 0.0D0
         DO 120 K = 1, N3
            DO 100 J = 1, N2
               DO 80 I = 1, N1
                  IF (D(I,J,K).NE.0.0D0) THEN
*                    Seven point molecule formula
                     R(I,J,K) = Q(I,J,K) - A(I,J,K)*T(I,J,K-1) - B(I,J,
     +                          K)*T(I,J-1,K) - C(I,J,K)*T(I-1,J,K) -
     +                          D(I,J,K)*T(I,J,K) - E(I,J,K)*T(I+1,J,K)
     +                           - F(I,J,K)*T(I,J+1,K) - G(I,J,K)*T(I,J,
     +                          K+1)
                  ELSE
*                    Explicit equation
                     R(I,J,K) = Q(I,J,K) - T(I,J,K)
                  END IF
                  ARES = ABS(R(I,J,K))
                  RESMAX = MAX(RESMAX,ARES)
                  RESMN = RESMN + ARES
   80          CONTINUE
  100       CONTINUE
  120    CONTINUE
         RESMN = RESMN/(DBLE(N1*N2*N3))
         IFAIL = 0
*
         CALL D03UBF(N1,N2,N3,N1M,N2M,A,B,C,D,E,F,G,APARAM,IT,R,WRKSP1,
     +               WRKSP2,WRKSP3,IFAIL)
*
*        Update the dependent variable
         DELMAX = 0.0D0
         DELMN = 0.0D0
         DO 180 K = 1, N3
            DO 160 J = 1, N2
               DO 140 I = 1, N1
                  T(I,J,K) = T(I,J,K) + R(I,J,K)
                  ADEL = ABS(R(I,J,K))
                  DELMAX = MAX(DELMAX,ADEL)
                  DELMN = DELMN + ADEL
  140          CONTINUE
  160       CONTINUE
  180    CONTINUE
         DELMN = DELMN/(DBLE(N1*N2*N3))
         WRITE (NOUT,99999) IT, RESMAX, RESMN, DELMAX, DELMN
*        Convergence tests here if required
  200 CONTINUE
*     End of iterative loop
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Table of calculated function values'
      WRITE (NOUT,*)
     +'K  J   (I       T   ) (I       T   ) (I       T    ) (I       T
     + )'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) ((K,J,(I,T(I,J,K),I=1,N1),J=1,N2),K=1,N3)
      STOP
*
99999 FORMAT (1X,I5,4(2X,D11.4))
99998 FORMAT ((1X,I1,I3,1X,4(1X,I3,2X,F9.3)))
      END
