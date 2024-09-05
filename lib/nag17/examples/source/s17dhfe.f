*     S17DHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      COMPLEX*16       BI, Z
      INTEGER          IFAIL
      CHARACTER*1      DERIV, SCALE
*     .. External Subroutines ..
      EXTERNAL         S17DHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S17DHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  'DERIV           Z         SCALE           BI         IFAIL'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) DERIV, Z, SCALE
      IFAIL = 0
*
      CALL S17DHF(DERIV,Z,SCALE,BI,IFAIL)
*
      WRITE (NOUT,99999) DERIV, Z, SCALE, BI, IFAIL
      GO TO 20
   40 STOP
*
99999 FORMAT (3X,A,'   (',F8.4,',',F8.4,')   ',A,'   (',F8.4,',',F8.4,
     +       ')',I5)
      END
