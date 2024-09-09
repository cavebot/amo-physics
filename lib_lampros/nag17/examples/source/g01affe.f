*     G01AFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX, IA, IO
      PARAMETER        (NMAX=8,MMAX=8,IA=MMAX,IO=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION CHI
      INTEGER          I, IFAIL, J, K, M, M1, M2, N, N1, N2, NDF, NPOS,
     +                 NPROB, NUM
*     .. Local Arrays ..
      DOUBLE PRECISION O(MMAX,NMAX), P(21)
      INTEGER          NA(MMAX,NMAX)
*     .. External Subroutines ..
      EXTERNAL         G01AFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01AFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NPROB
      DO 80 I = 1, NPROB
         READ (NIN,*) M, N, NUM
         IF (M.GE.1 .AND. M.LE.MMAX-1 .AND. N.GE.1 .AND. N.LE.NMAX-1)
     +       THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Problem ', I
            DO 20 J = 1, M
               READ (NIN,*) (NA(J,K),K=1,N)
   20       CONTINUE
            WRITE (NOUT,*) 'Data as input -'
            WRITE (NOUT,99991) 'Number of rows', M
            WRITE (NOUT,99991) 'Number of columns', N
            WRITE (NOUT,99991) 'NUM =', NUM,
     +        ' (NUM = 1 means table reduced in size if necessary)'
            IFAIL = 1
            M = M + 1
            N = N + 1
*
            CALL G01AFF(IA,IO,M,N,NA,NUM,O,CHI,P,NPOS,NDF,M1,N1,IFAIL)
*
            WRITE (NOUT,*)
            IF (IFAIL.NE.0) THEN
               WRITE (NOUT,99999)
     +           'Unsuccessful call of G01AFF. IFAIL = ', IFAIL
            ELSE
               IF (NUM.EQ.0) THEN
                  WRITE (NOUT,*) 'Successful call of G01AFF'
                  M2 = M - 1
                  N2 = N - 1
                  IF (M1.NE.M2) THEN
                     WRITE (NOUT,99991) 'No. of rows reduced from ', M2,
     +                 ' to ', M1
                  END IF
                  IF (N1.NE.N2) THEN
                     WRITE (NOUT,99991) 'No. of cols reduced from ', N2,
     +                 ' to ', N1
                  END IF
                  WRITE (NOUT,*)
                  WRITE (NOUT,*)
     +              'Table of observed and expected frequencies'
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) '              Column'
                  WRITE (NOUT,99990) (K,K=1,N1)
                  WRITE (NOUT,*) 'Row'
                  DO 40 J = 1, M1
                     WRITE (NOUT,99998) J, (NA(J,K),K=1,N1)
                     WRITE (NOUT,99997) (O(J,K),K=1,N1)
                     WRITE (NOUT,99993) 'Row total = ', NA(J,N)
   40             CONTINUE
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) 'Column'
                  WRITE (NOUT,99992) 'totals', (NA(M,K),K=1,N1)
                  WRITE (NOUT,99993) 'Grand total = ', NA(M,N)
                  WRITE (NOUT,*)
                  WRITE (NOUT,99996) 'Chi-squared = ', CHI,
     +              '   D.F. = ', NDF
               ELSE
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) 'Fisher''s exact test for 2*2 table'
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) 'Table of observed frequencies'
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) '          Column'
                  WRITE (NOUT,*) '          1    2'
                  WRITE (NOUT,*) 'Row'
                  DO 60 J = 1, 2
                     WRITE (NOUT,99998) J, (NA(J,K),K=1,2)
                     WRITE (NOUT,99993) 'Row total = ', NA(J,N)
   60             CONTINUE
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) 'Column'
                  WRITE (NOUT,99992) 'totals', (NA(M,K),K=1,2)
                  WRITE (NOUT,99993) 'Grand total = ', NA(M,N)
                  WRITE (NOUT,*)
                  WRITE (NOUT,99995)
     +              'This table corresponds to element ', NPOS,
     +              ' in vector P below'
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) 'Vector P'
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) ' I   P(I)'
                  WRITE (NOUT,99994) (J,P(J),J=1,NUM)
               END IF
            END IF
         ELSE
            STOP
         END IF
   80 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,I2,4X,10I6)
99997 FORMAT (8X,12F6.0)
99996 FORMAT (1X,A,F10.3,A,I3)
99995 FORMAT (1X,A,I4,A)
99994 FORMAT (1X,I2,F9.4)
99993 FORMAT (49X,A,I7)
99992 FORMAT (1X,A,10I6)
99991 FORMAT (1X,A,I3,A,I3)
99990 FORMAT (7X,10I6)
      END
