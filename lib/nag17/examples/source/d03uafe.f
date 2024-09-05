*     D03UAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N1, N2, N1M, NITS
      PARAMETER        (N1=6,N2=10,N1M=N1,NITS=10)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ADEL, APARAM, ARES, DELMAX, DELMN, RESMAX, RESMN
      INTEGER          I, IFAIL, IT, J
*     .. Local Arrays ..
      DOUBLE PRECISION A(N1M,N2), B(N1M,N2), C(N1M,N2), D(N1M,N2),
     +                 E(N1M,N2), Q(N1M,N2), R(N1M,N2), T(N1M,N2),
     +                 WRKSP1(N1M,N2), WRKSP2(N1M,N2), X(N1), Y(N2)
*     .. External Subroutines ..
      EXTERNAL         D03UAF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, COS, EXP, MAX, DBLE
*     .. Data statements ..
      DATA             X(1), X(2), X(3), X(4), X(5), X(6)/0.0D0, 1.0D0,
     +                 3.0D0, 6.0D0, 10.0D0, 15.0D0/
      DATA             Y(1), Y(2), Y(3), Y(4), Y(5), Y(6), Y(7), Y(8),
     +                 Y(9), Y(10)/0.0D0, 1.0D0, 3.0D0, 6.0D0, 10.0D0,
     +                 15.0D0, 21.0D0, 28.0D0, 36.0D0, 45.0D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03UAF Example Program Results'
      WRITE (NOUT,*)
      APARAM = 1.0D0
*     Set up difference equation coefficients, source terms and
*     initial S
      DO 40 J = 1, N2
         DO 20 I = 1, N1
            IF ((I.NE.1) .AND. (I.NE.N1) .AND. (J.NE.1) .AND. (J.NE.N2))
     +          THEN
*              Specification for internal nodes
               A(I,J) = 2.0D0/((Y(J)-Y(J-1))*(Y(J+1)-Y(J-1)))
               E(I,J) = 2.0D0/((Y(J+1)-Y(J))*(Y(J+1)-Y(J-1)))
               B(I,J) = 2.0D0/((X(I)-X(I-1))*(X(I+1)-X(I-1)))
               D(I,J) = 2.0D0/((X(I+1)-X(I))*(X(I+1)-X(I-1)))
               C(I,J) = -A(I,J) - B(I,J) - D(I,J) - E(I,J)
               Q(I,J) = 0.0D0
               T(I,J) = 0.0D0
            ELSE
*              Specification for boundary nodes
               A(I,J) = 0.0D0
               B(I,J) = 0.0D0
               C(I,J) = 0.0D0
               D(I,J) = 0.0D0
               E(I,J) = 0.0D0
               Q(I,J) = EXP((X(I)+1.0D0)/Y(N2))*COS(Y(J)/Y(N2))
               T(I,J) = 0.0D0
            END IF
   20    CONTINUE
   40 CONTINUE
*     Iterative loop
      WRITE (NOUT,*) 'Iteration      Residual                   Change'
      WRITE (NOUT,*)
     +  '  No       Max.        Mean           Max.      Mean'
      WRITE (NOUT,*)
      DO 140 IT = 1, NITS
*        Calculate the residuals
         RESMAX = 0.0D0
         RESMN = 0.0D0
         DO 80 J = 1, N2
            DO 60 I = 1, N1
               IF (C(I,J).NE.0.0D0) THEN
*                 Five point molecule formula
                  R(I,J) = Q(I,J) - A(I,J)*T(I,J-1) - B(I,J)*T(I-1,J) -
     +                     C(I,J)*T(I,J) - D(I,J)*T(I+1,J) - E(I,J)*T(I,
     +                     J+1)
               ELSE
*                 Explicit equation
                  R(I,J) = Q(I,J) - T(I,J)
               END IF
               ARES = ABS(R(I,J))
               RESMAX = MAX(RESMAX,ARES)
               RESMN = RESMN + ARES
   60       CONTINUE
   80    CONTINUE
         RESMN = RESMN/(DBLE(N1*N2))
         IFAIL = 0
*
         CALL D03UAF(N1,N2,N1M,A,B,C,D,E,APARAM,IT,R,WRKSP1,WRKSP2,
     +               IFAIL)
*
*        Update the dependent variable
         DELMAX = 0.0D0
         DELMN = 0.0D0
         DO 120 J = 1, N2
            DO 100 I = 1, N1
               T(I,J) = T(I,J) + R(I,J)
               ADEL = ABS(R(I,J))
               DELMAX = MAX(DELMAX,ADEL)
               DELMN = DELMN + ADEL
  100       CONTINUE
  120    CONTINUE
         DELMN = DELMN/(DBLE(N1*N2))
         WRITE (NOUT,99999) IT, RESMAX, RESMN, DELMAX, DELMN
*        Convergence tests here if required
  140 CONTINUE
*     End of iterative loop
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Table of calculated function values'
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +'   I    1          2          3          4          5          6'
      WRITE (NOUT,*) ' J'
      DO 160 J = 1, N2
         WRITE (NOUT,99998) J, (T(I,J),I=1,N1)
  160 CONTINUE
      STOP
*
99999 FORMAT (1X,I3,4(2X,D11.4))
99998 FORMAT (1X,I2,1X,6(F9.3,2X))
      END
