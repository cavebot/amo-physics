*     D01GYF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NDIM
      PARAMETER        (NDIM=4)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, NPTS
*     .. Local Arrays ..
      DOUBLE PRECISION VK(20)
*     .. External Subroutines ..
      EXTERNAL         D01GYF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01GYF Example Program Results'
      NPTS = 631
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'NDIM =', NDIM, ' NPTS =', NPTS
      IFAIL = 0
*
      CALL D01GYF(NDIM,NPTS,VK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99998) 'Coefficients =', (VK(I),I=1,NDIM)
      STOP
*
99999 FORMAT (1X,A,I3,A,I6)
99998 FORMAT (1X,A,4F6.0)
      END
