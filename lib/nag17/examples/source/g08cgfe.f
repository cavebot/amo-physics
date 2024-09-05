*     G08CGF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT, NMAX, NCLMAX
      PARAMETER        (NIN=5,NOUT=6,NMAX=100,NCLMAX=10)
*     .. Local Scalars ..
      DOUBLE PRECISION CHISQ, P, XMAX, XMIN
      INTEGER          I, ICLASS, IFAIL, NPEST, NCLASS, N, NDF
      CHARACTER*1      CDIST
*     .. Local Arrays ..
      DOUBLE PRECISION CHISQI(NCLMAX), CINT(NCLMAX), EVAL(NCLMAX),
     +                 PAR(2), PROB(NCLMAX), X(NMAX)
      INTEGER          IFREQ(NCLMAX)
*     .. External Subroutines ..
      EXTERNAL         G01AEF, G05CBF, G05FAF, G08CGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08CGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NCLASS, CDIST
      READ (NIN,*) (PAR(I),I=1,2)
      NPEST = 0
*
*     Generate random numbers from a uniform distribution
      CALL G05CBF(0)
*
      CALL G05FAF(PAR(1),PAR(2),N,X)
      ICLASS = 0
*
*     Determine suitable intervals
      IF (CDIST.EQ.'U' .OR. CDIST.EQ.'u') THEN
         ICLASS = 1
         CINT(1) = PAR(1) + (PAR(2)-PAR(1))/NCLASS
         DO 20 I = 2, NCLASS - 1
            CINT(I) = CINT(I-1) + (PAR(2)-PAR(1))/NCLASS
   20    CONTINUE
      END IF
      IFAIL = 0
*
      CALL G01AEF(N,NCLASS,X,ICLASS,CINT,IFREQ,XMIN,XMAX,IFAIL)
*
      IFAIL = 0
*
      CALL G08CGF(NCLASS,IFREQ,CINT,CDIST,PAR,NPEST,PROB,CHISQ,P,NDF,
     +            EVAL,CHISQI,IFAIL)
*
      IF (IFAIL.NE.0) WRITE (NOUT,99999) '** IFAIL = ', IFAIL
      WRITE (NOUT,*)
      WRITE (NOUT,99998) 'Chi-squared test statistic   = ', CHISQ
      WRITE (NOUT,99997) 'Degrees of freedom.          = ', NDF
      WRITE (NOUT,99998) 'Significance level           = ', P
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  'The contributions to the test statistic are :-'
      DO 40 I = 1, NCLASS
         WRITE (NOUT,99996) CHISQI(I)
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,F10.4)
99997 FORMAT (1X,A,I5)
99996 FORMAT (1X,F10.4)
      END
