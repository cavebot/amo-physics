*     G08ECF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, MSIZE, LDC
      PARAMETER        (N=1000,MSIZE=5,LDC=5)
*     .. Local Scalars ..
      DOUBLE PRECISION CHI, DF, EX, P
      INTEGER          I, IFAIL, J, K
      CHARACTER*1      CL
*     .. Local Arrays ..
      DOUBLE PRECISION X(N)
      INTEGER          NCOUNT(LDC,LDC,MSIZE)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05FAF, G08ECF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08ECF Example Program Results'
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
         CALL G08ECF(CL,N,X,MSIZE,NCOUNT,LDC,EX,CHI,DF,P,IFAIL)
*
         IF (CL.NE.'L' .AND. CL.NE.'l' .AND. IFAIL.NE.0) GO TO 80
*
   20 CONTINUE
*
      IF (IFAIL.EQ.0 .OR. IFAIL.EQ.7) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Count matrix'
         DO 60 I = 1, MSIZE
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'I = ', I
            WRITE (NOUT,*) '        1      2      3      4      5'
            WRITE (NOUT,*)
            DO 40 J = 1, MSIZE
               WRITE (NOUT,99998) J, (NCOUNT(I,J,K),K=1,MSIZE)
   40       CONTINUE
   60    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'Expected value = ', EX
         WRITE (NOUT,99996) 'CHISQ          = ', CHI
         WRITE (NOUT,99997) 'DF             = ', DF
         WRITE (NOUT,99996) 'Prob           = ', P
         IF (IFAIL.EQ.7) WRITE (NOUT,*)
     +   ' ** Note : the chi square approximation may not be very good.'
      END IF
   80 STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,I2,5I7)
99997 FORMAT (1X,A,F8.2)
99996 FORMAT (1X,A,F10.4)
      END
