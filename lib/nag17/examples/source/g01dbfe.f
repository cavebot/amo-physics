*     G01DBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=10)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION PP(N)
*     .. External Subroutines ..
      EXTERNAL         G01DBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01DBF Example Program Results'
      IFAIL = 0
*
      CALL G01DBF(N,PP,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Sample size = ', N
      WRITE (NOUT,*) 'Normal scores'
      WRITE (NOUT,99998) (PP(I),I=1,N)
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (10X,5F12.4)
      END
