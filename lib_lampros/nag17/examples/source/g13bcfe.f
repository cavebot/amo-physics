*     G13BCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NXYMAX, NLMAX
      PARAMETER        (NXYMAX=20,NLMAX=15)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION R0XY, R0YX, STATXY, STATYX, SXY, SYX
      INTEGER          I, IFAIL, NL, NXY
*     .. Local Arrays ..
      DOUBLE PRECISION RXY(NLMAX), RYX(NLMAX), X(NXYMAX), Y(NXYMAX)
*     .. External Subroutines ..
      EXTERNAL         G13BCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13BCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Read series length and number of lags
      READ (NIN,*) NXY, NL
      IF (NXY.GT.2 .AND. NXY.LE.NXYMAX .AND. NL.GT.0 .AND. NL.LE.NLMAX)
     +    THEN
*        Read series
         READ (NIN,*) (X(I),I=1,NXY)
         READ (NIN,*) (Y(I),I=1,NXY)
*        Call routine to calculate cross correlations between X and Y
         IFAIL = 0
*
         CALL G13BCF(X,Y,NXY,NL,SXY,R0XY,RXY,STATXY,IFAIL)
*
         IFAIL = 0
*
*        Call routine to calculate cross correlations between Y and X
         CALL G13BCF(Y,X,NXY,NL,SYX,R0YX,RYX,STATYX,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     '                           Between        Between'
         WRITE (NOUT,*)
     +     '                           X and Y        Y and X'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Standard deviation ratio', SXY, SYX
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Cross correlation at lag'
         WRITE (NOUT,99999) '                       0', R0XY, R0YX
         WRITE (NOUT,99998) (I,RXY(I),RYX(I),I=1,NL)
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'Test statistic          ', STATXY, STATYX
      END IF
      STOP
*
99999 FORMAT (1X,A,F10.4,F15.4)
99998 FORMAT (21X,I4,F10.4,F15.4)
99997 FORMAT (1X,A,F10.4,F15.4)
      END
