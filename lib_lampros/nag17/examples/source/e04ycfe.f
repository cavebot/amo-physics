*     E04YCF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MDEC, NDEC, LIW, LW
      PARAMETER        (MDEC=15,NDEC=3,LIW=1,
     +                 LW=7*NDEC+NDEC*NDEC+2*MDEC*NDEC+3*MDEC+NDEC*
     +                 (NDEC-1)/2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Arrays in Common ..
      DOUBLE PRECISION T(MDEC,NDEC), Y(MDEC)
*     .. Local Scalars ..
      DOUBLE PRECISION FSUMSQ
      INTEGER          I, IFAIL, J, M, N, NS, NV
*     .. Local Arrays ..
      DOUBLE PRECISION CJ(NDEC), W(LW), X(NDEC)
      INTEGER          IW(LIW)
*     .. External Subroutines ..
      EXTERNAL         E04FDF, E04YCF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Common blocks ..
      COMMON           /USER/Y, T
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04YCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      M = MDEC
      N = NDEC
*     Observations of TJ ( J = 1, 2, 3 ) are held in T( I, J )
*     ( I = 1, 2, ... , 15 )
      DO 20 I = 1, M
         READ (NIN,*) Y(I), (T(I,J),J=1,N)
   20 CONTINUE
      X(1) = 0.5D0
      X(2) = 1.0D0
      X(3) = 1.5D0
      IFAIL = 1
*
      CALL E04FDF(M,N,X,FSUMSQ,IW,LIW,W,LW,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Error exit from E04FDF. IFAIL = ', IFAIL
         WRITE (NOUT,*) '- see routine document'
      END IF
      IF (IFAIL.NE.1) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'On exit, the sum of squares is', FSUMSQ
         WRITE (NOUT,*) 'at the point'
         WRITE (NOUT,99997) (X(J),J=1,N)
*
*        Compute estimates of the variances of the sample regression
*        coefficients at the final point.
*        Since NS is greater than N we can use the first N elements
*        of W for the parameter WORK.
*
         NS = 6*N + 2*M + M*N + 1 + MAX(1,(N*(N-1))/2)
         NV = NS + N
         IFAIL = 1
*
         CALL E04YCF(0,M,N,FSUMSQ,W(NS),W(NV),N,CJ,W,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Error exit from E04YCF.  IFAIL = ',
     +        IFAIL
            WRITE (NOUT,*) '- see routine document'
         END IF
         IF ((IFAIL.NE.1) .AND. (IFAIL.NE.2)) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        'and estimates of the variances of the sample'
            WRITE (NOUT,*) 'regression coefficients are'
            WRITE (NOUT,99997) (CJ(J),J=1,N)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,A,F12.4)
99997 FORMAT (1X,3F12.4)
      END
*
      SUBROUTINE LSFUN1(M,N,XC,FVECC)
*     Routine to evaluate the residuals
*     .. Parameters ..
      INTEGER           MDEC, NDEC
      PARAMETER         (MDEC=15,NDEC=3)
*     .. Scalar Arguments ..
      INTEGER           M, N
*     .. Array Arguments ..
      DOUBLE PRECISION  FVECC(M), XC(N)
*     .. Arrays in Common ..
      DOUBLE PRECISION  T(MDEC,NDEC), Y(MDEC)
*     .. Local Scalars ..
      INTEGER           I
*     .. Common blocks ..
      COMMON            /USER/Y, T
*     .. Executable Statements ..
      DO 20 I = 1, M
         FVECC(I) = XC(1) + T(I,1)/(XC(2)*T(I,2)+XC(3)*T(I,3)) - Y(I)
   20 CONTINUE
      RETURN
      END
