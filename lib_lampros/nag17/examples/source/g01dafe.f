*     G01DAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IW
      PARAMETER        (NMAX=15,IW=3*NMAX/2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ERREST, ETOL
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION PP(NMAX), WORK(IW)
*     .. External Subroutines ..
      EXTERNAL         G01DAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01DAF Example Program Results'
      ETOL = 0.001D0
      DO 20 J = 5, NMAX, 5
         N = J
         IFAIL = 1
*
         CALL G01DAF(N,PP,ETOL,ERREST,WORK,IW,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Set size = ', N
         WRITE (NOUT,99998) 'Error tolerance (input) = ', ETOL
         WRITE (NOUT,99998) 'Error estimate (output) = ', ERREST
         WRITE (NOUT,*) 'Normal scores'
         WRITE (NOUT,99997) (PP(I),I=1,N)
         IF (IFAIL.NE.0) WRITE (NOUT,99999)
     +       'G01DAF fails with IFAIL = ', IFAIL
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,D13.3)
99997 FORMAT (10X,5F10.3)
      END
