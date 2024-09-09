*     G08EDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          M, N, MAXG
      DOUBLE PRECISION RLO, RUP, TOTLEN
      PARAMETER        (M=0,N=1000,MAXG=10,RLO=0.4D0,RUP=0.6D0,
     +                 TOTLEN=1.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION CHI, DF, P
      INTEGER          I, IFAIL, J, NGAP
      CHARACTER*1      CL
*     .. Local Arrays ..
      DOUBLE PRECISION EX(MAXG), X(N)
      INTEGER          NCOUNT(MAXG)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05FAF, G08EDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08EDF Example Program Results'
      CALL G05CBF(0)
      DO 20 I = 1, 5
         IF (I.EQ.1) THEN
            CL = 'First'
         ELSE IF (I.EQ.5) THEN
            CL = 'Last'
         ELSE
            CL = 'Intermediate'
         END IF
         CALL G05FAF(0.0D0,1.0D0,N,X)
         IFAIL = -1
*
         CALL G08EDF(CL,N,X,M,MAXG,RLO,RUP,TOTLEN,NGAP,NCOUNT,EX,CHI,DF,
     +               P,IFAIL)
*
         IF (CL.NE.'L' .AND. CL.NE.'l' .AND. IFAIL.NE.0) GO TO 40
*
   20 CONTINUE
      IF (IFAIL.EQ.0 .OR. IFAIL.GE.8) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Total number of gaps found = ', NGAP
         IF (IFAIL.EQ.8) WRITE (NOUT,*)
     +       ' ** Note : the number of gaps requested were not found.'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Count'
         WRITE (NOUT,*)
     +'      0      1      2      3      4      5      6      7      8
     +   >9'
         WRITE (NOUT,99998) (NCOUNT(J),J=1,MAXG)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Expect'
         WRITE (NOUT,*)
     +'      0      1      2      3      4      5      6      7      8
     +   >9'
         WRITE (NOUT,99997) (EX(J),J=1,MAXG)
         WRITE (NOUT,*)
         WRITE (NOUT,99996) 'Chisq = ', CHI
         WRITE (NOUT,99995) 'DF    = ', DF
         WRITE (NOUT,99996) 'Prob  = ', P
         IF (IFAIL.EQ.9) WRITE (NOUT,*)
     +   ' ** Note : the chi square approximation may not be very good.'
      END IF
   40 STOP
*
99999 FORMAT (1X,A,I10)
99998 FORMAT (1X,10I7)
99997 FORMAT (1X,10F7.1)
99996 FORMAT (1X,A,F10.4)
99995 FORMAT (1X,A,F7.1)
      END
