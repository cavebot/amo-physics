*     G01AEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, K2MAX
      PARAMETER        (NMAX=71,K2MAX=10)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION XMAX, XMIN
      INTEGER          I, ICLASS, IFAIL, J, N, NOC, NPROB
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX), C(K2MAX)
      INTEGER          JFREQ(K2MAX)
*     .. External Subroutines ..
      EXTERNAL         G01AEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01AEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NPROB
      DO 20 I = 1, NPROB
         READ (NIN,*) N, ICLASS, NOC
         IF (N.GE.1 .AND. N.LE.NMAX .AND. NOC.GE.0 .AND. NOC.LE.K2MAX-2)
     +       THEN
            READ (NIN,*) (A(J),J=1,N)
            WRITE (NOUT,*)
            WRITE (NOUT,99997) 'Problem ', I
            WRITE (NOUT,99997) 'Number of cases', N
            WRITE (NOUT,99997) 'Number of classes', NOC
            NOC = NOC + 1
            IF (ICLASS.NE.1) THEN
               WRITE (NOUT,*) 'Routine-supplied class boundaries'
            ELSE
               READ (NIN,*) (C(J),J=1,NOC)
               WRITE (NOUT,*) 'User-supplied class boundaries'
            END IF
            NOC = NOC + 1
            IFAIL = 1
*
            CALL G01AEF(N,NOC,A,ICLASS,C,JFREQ,XMIN,XMAX,IFAIL)
*
            WRITE (NOUT,*)
            IF (IFAIL.EQ.0) THEN
               WRITE (NOUT,*) 'Successful call of G01AEF'
               WRITE (NOUT,*)
               WRITE (NOUT,*) '*** Frequency  distribution ***'
               WRITE (NOUT,*)
               WRITE (NOUT,*) '       Class            Frequency'
               WRITE (NOUT,*)
               WRITE (NOUT,99999) '   Up to    ', C(1), JFREQ(1)
               NOC = NOC - 1
               IF (NOC.GT.1) WRITE (NOUT,99998) (C(J-1),' to ',C(J),
     +             JFREQ(J),J=2,NOC)
               WRITE (NOUT,99996) C(NOC), '    and over  ', JFREQ(NOC+1)
               WRITE (NOUT,*)
               WRITE (NOUT,99995) 'Total frequency = ', N
               WRITE (NOUT,99994) 'Minimum = ', XMIN
               WRITE (NOUT,99994) 'Maximum = ', XMAX
            ELSE
               WRITE (NOUT,99997)
     +           'Unsuccessful call of G01AEF. IFAIL = ', IFAIL
            END IF
         ELSE
            STOP
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,F8.2,I11)
99998 FORMAT (1X,F8.2,A,F8.2,I11)
99997 FORMAT (1X,A,I4)
99996 FORMAT (1X,F8.2,A,I9)
99995 FORMAT (1X,A,I6)
99994 FORMAT (1X,A,F9.2)
      END
