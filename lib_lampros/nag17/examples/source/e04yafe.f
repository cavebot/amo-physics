*     E04YAF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MDEC, NDEC, LJ, LIW, LW
      PARAMETER        (MDEC=15,NDEC=3,LJ=MDEC,LIW=1,
     +                 LW=3*NDEC+MDEC+MDEC*NDEC)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Arrays in Common ..
      DOUBLE PRECISION T(MDEC,NDEC), Y(MDEC)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION FJAC(LJ,NDEC), FVEC(MDEC), W(LW), X(NDEC)
      INTEGER          IW(LIW)
*     .. External Subroutines ..
      EXTERNAL         E04YAF, LSQFUN
*     .. Common blocks ..
      COMMON           Y, T
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04YAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      N = NDEC
      M = MDEC
*     Observations of TJ (J = 1, 2, 3) are held in T(I, J)
*     (I = 1, 2, . . . , 15)
      DO 20 I = 1, M
         READ (NIN,*) Y(I), (T(I,J),J=1,N)
   20 CONTINUE
*     Set up an arbitrary point at which to check the 1st
*     derivatives
      X(1) = 0.19D0
      X(2) = -1.34D0
      X(3) = 0.88D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The test point is'
      WRITE (NOUT,99999) (X(J),J=1,N)
      IFAIL = 1
*
      CALL E04YAF(M,N,LSQFUN,X,FVEC,FJAC,LJ,IW,LIW,W,LW,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.LT.0) THEN
         WRITE (NOUT,99998) 'IFLAG was set to ', IFAIL, 'in LSQFUN'
      ELSE IF (IFAIL.EQ.1) THEN
         WRITE (NOUT,*) 'A parameter is outside its expected range'
      ELSE
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,*)
     +        '1st derivatives are consistent with residual values'
         ELSE IF (IFAIL.EQ.2) THEN
            WRITE (NOUT,*)
     +        'Probable error in calculation of 1st derivatives'
         END IF
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'At the test point, LSQFUN gives'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     '      Residuals                   1st derivatives'
         WRITE (NOUT,99997) (FVEC(I),(FJAC(I,J),J=1,N),I=1,M)
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
         IF (IFLAG.NE.1) FVECC(I) = XC(1) + T(I,1)/DENOM - Y(I)
         IF (IFLAG.NE.0) THEN
            FJACC(I,1) = 1.0D0
            DUMMY = -1.0D0/(DENOM*DENOM)
            FJACC(I,2) = T(I,1)*T(I,2)*DUMMY
            FJACC(I,3) = T(I,1)*T(I,3)*DUMMY
         END IF
   20 CONTINUE
      RETURN
      END
