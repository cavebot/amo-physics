*     E04YBF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MDEC, NDEC, LJ, LB, LIW, LW
      PARAMETER        (MDEC=15,NDEC=3,LJ=MDEC,LB=NDEC*(NDEC+1)/2,LIW=1,
     +                 LW=5*NDEC+MDEC+MDEC*NDEC+NDEC*(NDEC-1)/2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Arrays in Common ..
      DOUBLE PRECISION T(MDEC,NDEC), Y(MDEC)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION B(LB), FJAC(LJ,NDEC), FVEC(MDEC), W(LW), X(NDEC)
      INTEGER          IW(LIW)
*     .. External Subroutines ..
      EXTERNAL         E04YAF, E04YBF, LSQFUN, LSQHES
*     .. Common blocks ..
      COMMON           Y, T
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04YBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      M = MDEC
      N = NDEC
*     Observations of TJ (J = 1, 2, 3) are held in T(I, J)
*     (I = 1, 2, . . . , 15)
      DO 20 I = 1, M
         READ (NIN,*) Y(I), (T(I,J),J=1,N)
   20 CONTINUE
*     Set up an arbitrary point at which to check the derivatives
      X(1) = 0.19D0
      X(2) = -1.34D0
      X(3) = 0.88D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The test point is'
      WRITE (NOUT,99999) (X(J),J=1,N)
*     Check the 1st derivatives
      IFAIL = 0
*
      CALL E04YAF(M,N,LSQFUN,X,FVEC,FJAC,LJ,IW,LIW,W,LW,IFAIL)
*
*     Check the evaluation of B
      IFAIL = 1
*
      CALL E04YBF(M,N,LSQFUN,LSQHES,X,FVEC,FJAC,LJ,B,LB,IW,LIW,W,LW,
     +            IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.LT.0) THEN
         WRITE (NOUT,99998) 'IFLAG was set to ', IFAIL,
     +     'in LSQFUN or LSQHES'
      ELSE IF (IFAIL.EQ.1) THEN
         WRITE (NOUT,*) 'A parameter is outside its expected range'
      ELSE
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,*)
     +        'The matrix B is consistent with 1st derivatives'
         ELSE IF (IFAIL.EQ.2) THEN
            WRITE (NOUT,*)
     +        'Probable error in calculation of the matrix B'
         END IF
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'At the test point, LSQFUN gives'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     '      Residuals                   1st derivatives'
         WRITE (NOUT,99997) (FVEC(I),(FJAC(I,J),J=1,N),I=1,M)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'and LSQHES gives the lower triangle of the matrix B'
         WRITE (NOUT,*)
         K = 1
         DO 40 I = 1, N
            WRITE (NOUT,99997) (B(J),J=K,K+I-1)
            K = K + I
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,4F10.5)
99998 FORMAT (1X,A,I3,A)
99997 FORMAT (1X,1P,4D15.3)
      END
*
      SUBROUTINE LSQFUN(IFLAG,M,N,XC,FVECC,FJACC,LJC,IW,LIW,W,LW)
*     Routine to evaluate the residuals and their 1st derivatives
*     .. Parameters ..
      INTEGER           MDEC, NDEC
      PARAMETER         (MDEC=15,NDEC=3)
*     .. Scalar Arguments ..
      INTEGER           IFLAG, LIW, LJC, LW, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION  FJACC(LJC,N), FVECC(M), W(LW), XC(N)
      INTEGER           IW(LIW)
*     .. Arrays in Common ..
      DOUBLE PRECISION  T(MDEC,NDEC), Y(MDEC)
*     .. Local Scalars ..
      DOUBLE PRECISION  DENOM, DUMMY
      INTEGER           I
*     .. Common blocks ..
      COMMON            Y, T
*     .. Executable Statements ..
      DO 20 I = 1, M
         DENOM = XC(2)*T(I,2) + XC(3)*T(I,3)
         FVECC(I) = XC(1) + T(I,1)/DENOM - Y(I)
         FJACC(I,1) = 1.0D0
         DUMMY = -1.0D0/(DENOM*DENOM)
         FJACC(I,2) = T(I,1)*T(I,2)*DUMMY
         FJACC(I,3) = T(I,1)*T(I,3)*DUMMY
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE LSQHES(IFLAG,M,N,FVECC,XC,B,LB,IW,LIW,W,LW)
*     Routine to compute the lower triangle of the matrix B
*     (stored by rows in the array B)
*     .. Parameters ..
      INTEGER           MDEC, NDEC
      PARAMETER         (MDEC=15,NDEC=3)
*     .. Scalar Arguments ..
      INTEGER           IFLAG, LB, LIW, LW, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION  B(LB), FVECC(M), W(LW), XC(N)
      INTEGER           IW(LIW)
*     .. Arrays in Common ..
      DOUBLE PRECISION  T(MDEC,NDEC), Y(MDEC)
*     .. Local Scalars ..
      DOUBLE PRECISION  DUMMY, SUM22, SUM32, SUM33
      INTEGER           I
*     .. Common blocks ..
      COMMON            Y, T
*     .. Executable Statements ..
      B(1) = 0.0D0
      B(2) = 0.0D0
      SUM22 = 0.0D0
      SUM32 = 0.0D0
      SUM33 = 0.0D0
      DO 20 I = 1, M
         DUMMY = 2.0D0*T(I,1)/(XC(2)*T(I,2)+XC(3)*T(I,3))**3
         SUM22 = SUM22 + FVECC(I)*DUMMY*T(I,2)**2
         SUM32 = SUM32 + FVECC(I)*DUMMY*T(I,2)*T(I,3)
         SUM33 = SUM33 + FVECC(I)*DUMMY*T(I,3)**2
   20 CONTINUE
      B(3) = SUM22
      B(4) = 0.0D0
      B(5) = SUM32
      B(6) = SUM33
      RETURN
      END
