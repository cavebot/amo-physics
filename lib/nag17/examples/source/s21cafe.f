*     S21CAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION CN, DN, M, SN, U
      INTEGER          IFAIL
*     .. External Subroutines ..
      EXTERNAL         S21CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S21CAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     + '          U            M           SN           CN           DN'
   20 READ (NIN,*,END=40) U, M
*
      IFAIL = 0
      CALL S21CAF(U,M,SN,CN,DN,IFAIL)
*
      WRITE (NOUT,99999) U, M, SN, CN, DN
      GO TO 20
   40 STOP
*
99999 FORMAT (3X,5D13.4)
      END
