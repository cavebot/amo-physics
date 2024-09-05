*     G07AAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION CLEVEL, PHAT, PL, PU
      INTEGER          IFAIL, K, N
*     .. External Subroutines ..
      EXTERNAL         G07AAF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G07AAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' Probability    Confidence Interval '
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) N, K, CLEVEL
      PHAT = DBLE(K)/DBLE(N)
      IFAIL = 0
*
      CALL G07AAF(N,K,CLEVEL,PL,PU,IFAIL)
*
      WRITE (NOUT,99999) PHAT, '( ', PL, ' , ', PU, ' )'
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,F10.4,6X,A,F6.4,A,F6.4,A)
      END
