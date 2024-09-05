*     D03EBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N1, N2, N1M, ITMAX
      PARAMETER        (N1=6,N2=10,N1M=N1,ITMAX=18)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION APARAM, CONCHN, CONRES
      INTEGER          I, IFAIL, ITCOUN, ITUSED, IXN, IYN, J, NDIR
*     .. Local Arrays ..
      DOUBLE PRECISION A(N1M,N2), B(N1M,N2), C(N1M,N2), CHNGS(ITMAX),
     +                 D(N1M,N2), E(N1M,N2), Q(N1M,N2), RESIDS(ITMAX),
     +                 T(N1M,N2), WRKSP1(N1M,N2), WRKSP2(N1M,N2),
     +                 WRKSP3(N1M,N2), X(N1M), Y(N2)
*     .. External Subroutines ..
      EXTERNAL         D03EBF
*     .. Intrinsic Functions ..
      INTRINSIC        COS, EXP
*     .. Data statements ..
      DATA             X(1), X(2), X(3), X(4), X(5), X(6)/0.0D0, 1.0D0,
     +                 3.0D0, 6.0D0, 10.0D0, 15.0D0/
      DATA             Y(1), Y(2), Y(3), Y(4), Y(5), Y(6), Y(7), Y(8),
     +                 Y(9), Y(10)/0.0D0, 1.0D0, 3.0D0, 6.0D0, 10.0D0,
     +                 15.0D0, 21.0D0, 28.0D0, 36.0D0, 45.0D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03EBF Example Program Results'
      WRITE (NOUT,*)
      APARAM = 1.0D0
      ITCOUN = 0
      NDIR = 1
      CONRES = 0.1D-5
      CONCHN = 0.1D-5
*     Set up difference equation coefficients, source terms and
*     initial conditions.
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
      WRITE (NOUT,*) 'Iteration      Maximum        Maximum'
      WRITE (NOUT,*) ' number        residual        change'
      IFAIL = 1
*
      CALL D03EBF(N1,N2,N1M,A,B,C,D,E,Q,T,APARAM,ITMAX,ITCOUN,ITUSED,
     +            NDIR,IXN,IYN,CONRES,CONCHN,RESIDS,CHNGS,WRKSP1,WRKSP2,
     +            WRKSP3,IFAIL)
*
      WRITE (NOUT,99999) (I,RESIDS(I),CHNGS(I),I=1,ITUSED)
*     Check error flag
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Table of calculated function values'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     + '  I    1          2          3          4          5          6'
         WRITE (NOUT,*) ' J'
         DO 60 J = 1, N2
            WRITE (NOUT,99998) J, (T(I,J),I=1,N1)
   60    CONTINUE
      ELSE
   80    WRITE (NOUT,99997) 'Error in D03EBF  IFAIL =', IFAIL
      END IF
      STOP
*
99999 FORMAT (2X,I2,10X,D11.4,4X,D11.4)
99998 FORMAT (1X,I2,1X,6(F9.3,2X))
99997 FORMAT (1X,A,I4)
      END
