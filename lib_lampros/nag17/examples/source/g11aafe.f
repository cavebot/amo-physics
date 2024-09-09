*     G11AAF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          CMAX, RMAX
      PARAMETER        (CMAX=3,RMAX=3)
*     .. Local Scalars ..
      DOUBLE PRECISION CHI, DF, G, PROB
      INTEGER          I, IFAIL, J, NCOL, NROW
*     .. Local Arrays ..
      DOUBLE PRECISION CHIST(RMAX,CMAX), EXPT(RMAX,CMAX)
      INTEGER          NOBST(RMAX,CMAX)
*     .. External Subroutines ..
      EXTERNAL         G11AAF
*     .. Executable Statements ..
      WRITE (NOUT,*) ' G11AAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NROW, NCOL
      IF (NROW.LE.RMAX .AND. NCOL.LE.CMAX) THEN
         DO 20 I = 1, NROW
            READ (NIN,*) (NOBST(I,J),J=1,NCOL)
   20    CONTINUE
         IFAIL = -1
*
         CALL G11AAF(NROW,NCOL,NOBST,RMAX,EXPT,CHIST,PROB,CHI,G,DF,
     +               IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.EQ.3) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) ' Probability = ', PROB
            WRITE (NOUT,99998) ' Pearson Chi-square statistic = ',
     +        CHI
            WRITE (NOUT,99998) ' Likelihood ratio test statistic = '
     +        , G
            WRITE (NOUT,99997) ' Degrees of freedom = ', DF
         END IF
      END IF
      STOP
*
99999 FORMAT (A,F6.4)
99998 FORMAT (A,F8.3)
99997 FORMAT (A,F4.0)
      END
