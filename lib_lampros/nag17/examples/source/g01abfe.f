*     G01ABF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=30)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IWT, J, N, NPROB
*     .. Local Arrays ..
      DOUBLE PRECISION ANS(13), WT(NMAX), X1(NMAX), X2(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G01ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01ABF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NPROB
      DO 20 J = 1, NPROB
         READ (NIN,*) N, IWT
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Problem ', J
         WRITE (NOUT,99999) 'Number of cases', N
         IF (N.GE.1 .AND. N.LE.NMAX) THEN
            READ (NIN,*) (X1(I),X2(I),I=1,N)
            WRITE (NOUT,*) 'Data as input -'
            WRITE (NOUT,*)
     +'        Var   1    Var   2    Var   1    Var   2    Var   1    Va
     +r   2'
            WRITE (NOUT,99995) (X1(I),X2(I),I=1,N)
            IF (IWT.EQ.1) THEN
               READ (NIN,*) (WT(I),I=1,N)
               WRITE (NOUT,*) 'Weights as input -'
               WRITE (NOUT,99994) (WT(I),I=1,N)
            END IF
            IFAIL = 1
*
            CALL G01ABF(N,X1,X2,IWT,WT,ANS,IFAIL)
*
            WRITE (NOUT,*)
            IF (IFAIL.EQ.0) THEN
               WRITE (NOUT,*) 'Successful call of G01ABF'
               WRITE (NOUT,99999) 'No. of valid cases', IWT
               WRITE (NOUT,*)
     +           '             Variable 1                    Variable 2'
               WRITE (NOUT,99998) 'Mean    ', ANS(1), ANS(2)
               WRITE (NOUT,99998) 'Std devn', ANS(3), ANS(4)
               WRITE (NOUT,99997) 'Corr SSP', ANS(5), ANS(6), ANS(7)
               WRITE (NOUT,99996) 'Correln ', ANS(8)
               WRITE (NOUT,99998) 'Minimum ', ANS(9), ANS(11)
               WRITE (NOUT,99998) 'Maximum ', ANS(10), ANS(12)
               WRITE (NOUT,99998) 'Sum of weights         ', ANS(13)
            ELSE
               WRITE (NOUT,*) 'Unsuccessful call of G01ABF'
               WRITE (NOUT,99999) 'IFAIL =', IFAIL
               IF (IFAIL.EQ.2) THEN
                  WRITE (NOUT,99999) 'No. of valid cases', IWT
                  WRITE (NOUT,*)
     +           '             Variable 1                    Variable 2'
                  WRITE (NOUT,99998) 'Mean    ', ANS(1), ANS(2)
                  WRITE (NOUT,99997) 'Corr SSP', ANS(5), ANS(6), ANS(7)
                  WRITE (NOUT,99998) 'Minimum ', ANS(9), ANS(11)
                  WRITE (NOUT,99998) 'Maximum ', ANS(10), ANS(12)
                  WRITE (NOUT,99998) 'Sum of weights         ', ANS(13)
               END IF
            END IF
         ELSE
            STOP
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,F15.1,F30.1)
99997 FORMAT (1X,A,3D15.5)
99996 FORMAT (1X,A,F30.4)
99995 FORMAT (5X,6F11.1)
99994 FORMAT (13X,F9.3)
      END
