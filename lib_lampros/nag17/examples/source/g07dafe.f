*     G07DAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=25)
*     .. Local Scalars ..
      DOUBLE PRECISION XMD, XME, XSD
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      DOUBLE PRECISION X(NMAX), Y(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G07DAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G07DAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (X(I),I=1,N)
         IFAIL = 0
*
         CALL G07DAF(N,X,Y,XME,XMD,XSD,IFAIL)
*
         WRITE (NOUT,*) 'Output Y:'
         WRITE (NOUT,99999) (Y(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'XME = ', XME, ', XMD = ', XMD, ', XSD = ',
     +     XSD
      ELSE
         WRITE (NOUT,99997) 'N is out of range: N =', N
      END IF
      STOP
*
99999 FORMAT (1X,11F7.3)
99998 FORMAT (1X,A,F6.3,A,F6.3,A,F6.3)
99997 FORMAT (1X,A,I5)
      END
