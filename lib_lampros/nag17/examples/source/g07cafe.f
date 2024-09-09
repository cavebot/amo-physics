*     G07CAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION CLEVEL, DF, DL, DU, PROB, T, XMEAN, XSTD, YMEAN,
     +                 YSTD
      INTEGER          IFAIL, NX, NY
*     .. External Subroutines ..
      EXTERNAL         G07CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G07CAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NX, NY
      READ (NIN,*) XMEAN, YMEAN, XSTD, YSTD
      READ (NIN,*) CLEVEL
      IFAIL = 0
*
      CALL G07CAF('Two','Equal',NX,NY,XMEAN,YMEAN,XSTD,YSTD,CLEVEL,T,DF,
     +            PROB,DL,DU,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Assuming population variances are equal.'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 't test statistic = ', T
      WRITE (NOUT,99998) 'Degrees of freedom = ', DF
      WRITE (NOUT,99997) 'Significance level = ', PROB
      WRITE (NOUT,99999)
     +  'Lower confidence limit for difference in means = ', DL
      WRITE (NOUT,99999)
     +  'Upper confidence limit for difference in means = ', DU
      WRITE (NOUT,*)
      IFAIL = 0
*
      CALL G07CAF('Two','Unequal',NX,NY,XMEAN,YMEAN,XSTD,YSTD,CLEVEL,T,
     +            DF,PROB,DL,DU,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'No assumptions about population variances .'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 't test statistic = ', T
      WRITE (NOUT,99997) 'Degrees of freedom = ', DF
      WRITE (NOUT,99997) 'Significance level = ', PROB
      WRITE (NOUT,99999)
     +  'Lower confidence limit for difference in means = ', DL
      WRITE (NOUT,99999)
     +  'Upper confidence limit for difference in means = ', DU
      STOP
*
99999 FORMAT (1X,A,F10.4)
99998 FORMAT (1X,A,F8.1)
99997 FORMAT (1X,A,F8.4)
      END
