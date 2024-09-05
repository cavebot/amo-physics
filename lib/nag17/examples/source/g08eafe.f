*     G08EAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          M, N, MAXR, LDC, LWRK
      PARAMETER        (M=0,N=1000,MAXR=6,LDC=10,LWRK=34)
*     .. Local Scalars ..
      DOUBLE PRECISION CHI, DF, P
      INTEGER          I, IFAIL, J, NRUNS
      CHARACTER*1      CL
*     .. Local Arrays ..
      DOUBLE PRECISION C(LDC,MAXR), EXPECT(MAXR), WRK(LWRK), X(N)
      INTEGER          NCOUNT(MAXR)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05FAF, G08EAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08EAF Example Program Results'
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
         CALL G08EAF(CL,N,X,M,MAXR,NRUNS,NCOUNT,EXPECT,C,LDC,CHI,DF,P,
     +               WRK,LWRK,IFAIL)
*
         IF (CL.NE.'L' .AND. CL.NE.'l' .AND. IFAIL.NE.0) GO TO 60
*
   20 CONTINUE
*
      IF (IFAIL.EQ.0 .OR. IFAIL.EQ.10) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Total number of runs found = ', NRUNS
         IF (IFAIL.EQ.10) WRITE (NOUT,*)
     +       ' ** Note : the number of runs requested were not found.'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '                             Count'
         WRITE (NOUT,*)
     +     '          1        2        3        4        5       >5'
         WRITE (NOUT,99998) (NCOUNT(J),J=1,MAXR)
         WRITE (NOUT,*)
         WRITE (NOUT,*) '                             Expect'
         WRITE (NOUT,*)
     +     '          1        2        3        4        5       >5'
         WRITE (NOUT,99997) (EXPECT(J),J=1,MAXR)
         WRITE (NOUT,*)
         WRITE (NOUT,*) '                       Covariance matrix'
         WRITE (NOUT,*)
     +     '          1        2        3        4        5       >5'
         DO 40 I = 1, MAXR
            WRITE (NOUT,99996) I, (C(I,J),J=1,MAXR)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,99995) 'Chisq = ', CHI
         WRITE (NOUT,99994) 'DF    = ', DF
         WRITE (NOUT,99995) 'Prob  = ', P
      END IF
   60 STOP
*
99999 FORMAT (1X,A,I10)
99998 FORMAT (3X,6I9)
99997 FORMAT (3X,6F9.2)
99996 FORMAT (1X,I2,6F9.2)
99995 FORMAT (1X,A,F10.4)
99994 FORMAT (1X,A,F7.1)
      END
