*     G01EFF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, G, P
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION G01EFF
      EXTERNAL         G01EFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01EFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  'Gamma deviate    Alpha     Beta    Lower tail prob.'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) G, A, B
      IFAIL = 0
*
      P = G01EFF('Lower',G,A,B,IFAIL)
*
      WRITE (NOUT,99999) G, A, B, P
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,F9.2,F13.2,F9.2,F14.4)
      END
