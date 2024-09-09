*     G08EBF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, MSIZE, LAG, LDC
      PARAMETER        (N=1000,MSIZE=10,LAG=1,LDC=10)
*     .. Local Scalars ..
      DOUBLE PRECISION CHI, DF, EX, P
      INTEGER          I, IFAIL, IFAIL2
      CHARACTER*1      CL
*     .. Local Arrays ..
      DOUBLE PRECISION WRK(2*LAG), X(N)
      INTEGER          NCOUNT(LDC,MSIZE)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05FAF, G08EBF, X04EAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08EBF Example Program Results'
      CALL G05CBF(0)
      DO 20 I = 1, 10
         IF (I.EQ.1) THEN
            CL = 'First'
         ELSE IF (I.EQ.10) THEN
            CL = 'Last'
         ELSE
            CL = 'Intermediate'
         END IF
         CALL G05FAF(0.0D0,1.0D0,N,X)
         IFAIL = -1
*
         CALL G08EBF(CL,N,X,MSIZE,LAG,NCOUNT,LDC,EX,CHI,DF,P,WRK,IFAIL)
*
         IF (CL.NE.'L' .AND. CL.NE.'l' .AND. IFAIL.NE.0) GO TO 40
*
   20 CONTINUE
      IF (IFAIL.EQ.0 .OR. IFAIL.EQ.8) THEN
         WRITE (NOUT,*)
         IFAIL2 = 0
*
*        Print the count matrix
         CALL X04EAF('General',' ',MSIZE,MSIZE,NCOUNT,LDC,
     +               'Count matrix',IFAIL2)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Expected value = ', EX
         WRITE (NOUT,99997) 'CHISQ          = ', CHI
         WRITE (NOUT,99998) 'DF             = ', DF
         WRITE (NOUT,99997) 'Probability    = ', P
         IF (IFAIL.EQ.8) WRITE (NOUT,*)
     +   ' ** Note : the chi square approximation may not be very good.'
      END IF
   40 STOP
*
99999 FORMAT (1X,I2,10I7)
99998 FORMAT (1X,A,F8.2)
99997 FORMAT (1X,A,F10.4)
      END
