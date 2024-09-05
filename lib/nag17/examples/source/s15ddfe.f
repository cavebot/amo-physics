*     S15DDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      COMPLEX*16       W, Z
      INTEGER          IFAIL
*     .. External Functions ..
      COMPLEX*16       S15DDF
      EXTERNAL         S15DDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S15DDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '              Z                                W'
   20 READ (NIN,*,END=40) Z
      IFAIL = 0
*
      W = S15DDF(Z,IFAIL)
*
      WRITE (NOUT,99999) Z, W
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,'(',F12.4,',',F12.4,')      (',F12.4,',',F12.4,')')
      END
