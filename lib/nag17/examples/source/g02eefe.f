*     G02EEF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=20,MMAX=8)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION CHRSS, F, FIN, RSS
      INTEGER          I, IDF, IFAIL, IFR, IM, ISTEP, J, M, N, NTERM
      LOGICAL          ADDVAR
      CHARACTER        MEAN, WEIGHT
      CHARACTER*3      NEWVAR
*     .. Local Arrays ..
      DOUBLE PRECISION EXSS(MMAX), P(MMAX+1), Q(NMAX,MMAX+2),
     +                 WK(2*MMAX), WT(NMAX), X(NMAX,MMAX), Y(NMAX)
      INTEGER          ISX(MMAX)
      CHARACTER*3      FREE(MMAX), MODEL(MMAX), NAME(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02EEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02EEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, MEAN, WEIGHT
      IF (M.LT.MMAX .AND. N.LE.NMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), Y(I), WT(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), Y(I)
   40       CONTINUE
         END IF
         READ (NIN,*) (ISX(J),J=1,M)
         READ (NIN,*) (NAME(J),J=1,M)
         READ (NIN,*) FIN
         IF (MEAN.EQ.'M' .OR. MEAN.EQ.'m') THEN
            IM = 1
         ELSE
            IM = 0
         END IF
         ISTEP = 0
         DO 60 I = 1, M
            IFAIL = 0
*
            CALL G02EEF(ISTEP,MEAN,WEIGHT,N,M,X,NMAX,NAME,ISX,MMAX,Y,WT,
     +                  FIN,ADDVAR,NEWVAR,CHRSS,F,MODEL,NTERM,RSS,IDF,
     +                  IFR,FREE,EXSS,Q,NMAX,P,WK,IFAIL)
*
            IF (IFAIL.NE.0) GO TO 80
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Step ', ISTEP
            IF ( .NOT. ADDVAR) THEN
               WRITE (NOUT,99998)
     +           'No further variables added maximum F =', F
               WRITE (NOUT,99993) 'Free variables:  ', (FREE(J),J=1,IFR)
               WRITE (NOUT,*)
     +          'Change in residual sums of squares for free variables:'
               WRITE (NOUT,99992) '                 ', (EXSS(J),J=1,IFR)
               GO TO 80
            ELSE
               WRITE (NOUT,99997) 'Added variable is ', NEWVAR
               WRITE (NOUT,99996) 'Change in residual sum of squares =',
     +           CHRSS
               WRITE (NOUT,99998) 'F Statistic = ', F
               WRITE (NOUT,*)
               WRITE (NOUT,99995) 'Variables in model:',
     +           (MODEL(J),J=1,NTERM)
               WRITE (NOUT,*)
               WRITE (NOUT,99994) 'Residual sum of squares = ', RSS
               WRITE (NOUT,99999) 'Degrees of freedom = ', IDF
               WRITE (NOUT,*)
               IF (IFR.EQ.0) THEN
                  WRITE (NOUT,*) 'No free variables remaining'
                  GO TO 80
               END IF
               WRITE (NOUT,99993) 'Free variables:  ', (FREE(J),J=1,IFR)
               WRITE (NOUT,*)
     +          'Change in residual sums of squares for free variables:'
               WRITE (NOUT,99992) '                 ', (EXSS(J),J=1,IFR)
            END IF
   60    CONTINUE
   80    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,F7.2)
99997 FORMAT (1X,2A)
99996 FORMAT (1X,A,D13.4)
99995 FORMAT (1X,A,6(1X,A))
99994 FORMAT (1X,A,D13.4)
99993 FORMAT (1X,A,6(6X,A))
99992 FORMAT (1X,A,6(F9.4))
      END
