*     A02ACF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION XI, XR, YI, YR, ZI, ZR
*     .. External Subroutines ..
      EXTERNAL         A02ACF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'A02ACF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) XR, XI, YR, YI
*
      CALL A02ACF(XR,XI,YR,YI,ZR,ZI)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   XR    XI    YR    YI      ZR       ZI'
      WRITE (NOUT,99999) XR, XI, YR, YI, ZR, ZI
      STOP
*
99999 FORMAT (1X,4F6.1,2F9.4)
      END
