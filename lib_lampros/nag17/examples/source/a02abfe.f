*     A02ABF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION XI, XR, Y
*     .. External Functions ..
      DOUBLE PRECISION A02ABF
      EXTERNAL         A02ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'A02ABF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) XR, XI
      Y = A02ABF(XR,XI)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   XR    XI       Y'
      WRITE (NOUT,99999) XR, XI, Y
      STOP
*
99999 FORMAT (1X,2F6.1,F9.4)
      END
