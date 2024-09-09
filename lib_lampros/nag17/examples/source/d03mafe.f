*     D03MAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          IDIM, LD
      PARAMETER        (IDIM=100,LD=20)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION H
      INTEGER          I, IFAIL, J, M, N, NB, NPTS
*     .. Local Arrays ..
      DOUBLE PRECISION DIST(4,LD), PLACES(2,IDIM)
      INTEGER          INDEX(4,IDIM)
*     .. External Functions ..
      INTEGER          IN1
      EXTERNAL         IN1
*     .. External Subroutines ..
      EXTERNAL         D03MAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03MAF Example Program Results'
      WRITE (NOUT,*)
      H = 4.0D0
      M = 3
      N = 5
      NB = 10
      IFAIL = 0
*
      CALL D03MAF(H,M,N,NB,NPTS,PLACES,INDEX,IDIM,IN1,DIST,LD,IFAIL)
*
      WRITE (NOUT,*) '  I    X(I)      Y(I)'
      DO 20 I = 1, NPTS
         WRITE (NOUT,99999) I, PLACES(1,I), PLACES(2,I)
   20 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Index'
      DO 40 I = 1, NPTS
         WRITE (NOUT,99998) (INDEX(J,I),J=1,4)
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,I3,2F10.6)
99998 FORMAT (1X,4I5)
      END
*
      INTEGER FUNCTION IN1(X,Y)
*     Circular domain
*     .. Scalar Arguments ..
      DOUBLE PRECISION     X, Y
*     .. Executable Statements ..
      IF ((X-7.0D0)**2+(Y-7.0D0)**2.LE.36.0D0) THEN
         IN1 = 1
      ELSE
         IN1 = 0
      END IF
      RETURN
      END
