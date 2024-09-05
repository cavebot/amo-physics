*     G01BKF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION PEQK, PGTK, PLEK, RLAMDA
      INTEGER          IFAIL, K
*     .. External Subroutines ..
      EXTERNAL         G01BKF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01BKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    RLAMDA     K     PLEK      PGTK      PEQK'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) RLAMDA, K
      IFAIL = 0
*
      CALL G01BKF(RLAMDA,K,PLEK,PGTK,PEQK,IFAIL)
*
      WRITE (NOUT,99999) RLAMDA, K, PLEK, PGTK, PEQK
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,F10.3,I6,3F10.5)
      END
