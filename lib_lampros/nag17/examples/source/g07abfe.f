*     G07ABF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION CLEVEL, SUM, TL, TU, XMEAN
      INTEGER          I, IFAIL, IFREQ, N, NUM
*     .. External Subroutines ..
      EXTERNAL         G07ABF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G07ABF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*
*     Read in the number of Noxious Seeds in a sub sample and
*     the frequency with which that number occurs.
*
*     Compute the sample mean
*
      SUM = 0.0D0
      N = 0
   20 READ (NIN,*,END=40) NUM, IFREQ
      SUM = SUM + DBLE(NUM)*DBLE(IFREQ)
      N = N + IFREQ
      GO TO 20
   40 XMEAN = SUM/DBLE(N)
      WRITE (NOUT,*)
      WRITE (NOUT,99999)
     +  'The point estimate of the Poisson parameter = ', XMEAN
*
      DO 60 I = 1, 2
         IF (I.EQ.1) THEN
            CLEVEL = 0.95D0
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        '95 percent Confidence Interval for the estimate '
         ELSE
            CLEVEL = 0.99D0
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        '99 percent Confidence Interval for the estimate '
         END IF
         IFAIL = 0
*
         CALL G07ABF(N,XMEAN,CLEVEL,TL,TU,IFAIL)
*
         WRITE (NOUT,99998) '( ', TL, ' , ', TU, ' )'
   60 CONTINUE
      STOP
*
99999 FORMAT (1X,A,F6.4)
99998 FORMAT (6X,A,F6.4,A,F6.4,A)
      END
