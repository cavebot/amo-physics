*     E04HFF Example Program Text.
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          M, N, NT, LIW, LW
      PARAMETER        (M=15,N=3,NT=3,LIW=1,LW=8*N+2*N*N+2*M*N+3*M)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Arrays in Common ..
      DOUBLE PRECISION T(M,NT), Y(M)
*     .. Local Scalars ..
      DOUBLE PRECISION FSUMSQ
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION W(LW), X(N)
      INTEGER          IW(LIW)
*     .. External Subroutines ..
      EXTERNAL         E04HFF
*     .. Common blocks ..
      COMMON           Y, T
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04HFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Observations of TJ (J = 1, 2, 3) are held in T(I, J)
*     (I = 1, 2, . . . , 15)
      DO 20 I = 1, M
         READ (NIN,*) Y(I), (T(I,J),J=1,NT)
   20 CONTINUE
      X(1) = 0.5D0
      X(2) = 1.0D0
      X(3) = 1.5D0
      IFAIL = 1
*
      CALL E04HFF(M,N,X,FSUMSQ,IW,LIW,W,LW,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Error exit type', IFAIL,
     +     ' - see routine document'
      END IF
      IF (IFAIL.NE.1) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'On exit, the sum of squares is', FSUMSQ
         WRITE (NOUT,99998) 'at the point', (X(J),J=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I3,A)
99998 FORMAT (1X,A,3F12.4)
      END
*
      SUBROUTINE LSFUN2(M,N,XC,FVECC,FJACC,LJC)
*     Routine to evaluate the residuals and their 1st derivatives.
*     This routine must be called LSFUN2.
*     .. Parameters ..
      INTEGER           MDEC, NT
      PARAMETER         (MDEC=15,NT=3)
*     .. Scalar Arguments ..
      INTEGER           LJC, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION  FJACC(LJC,N), FVECC(M), XC(N)
*     .. Arrays in Common ..
      DOUBLE PRECISION  T(MDEC,NT), Y(MDEC)
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
      SUBROUTINE LSHES2(M,N,FVECC,XC,B,LB)
*     Routine to compute the lower triangle of the matrix B
*     (stored by rows in the array B).
*     This routine must be called LHES2.
*     .. Parameters ..
      INTEGER           MDEC, NT
      PARAMETER         (MDEC=15,NT=3)
*     .. Scalar Arguments ..
      INTEGER           LB, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION  B(LB), FVECC(M), XC(N)
*     .. Arrays in Common ..
      DOUBLE PRECISION  T(MDEC,NT), Y(MDEC)
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
