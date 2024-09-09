*     D01GZF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NDIM
      PARAMETER        (NDIM=4)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, NP1, NP2
*     .. Local Arrays ..
      DOUBLE PRECISION VK(NDIM)
*     .. External Subroutines ..
      EXTERNAL         D01GZF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01GZF Example Program Results'
      NP1 = 89
      NP2 = 11
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'NDIM =', NDIM, ' NP1 =', NP1, ' NP2 =', NP2
      IFAIL = 0
*
      CALL D01GZF(NDIM,NP1,NP2,VK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99998) 'Coefficients =', (VK(I),I=1,NDIM)
      STOP
*
99999 FORMAT (1X,A,I3,A,I6,A,I6)
99998 FORMAT (1X,A,4F6.0)
      END
