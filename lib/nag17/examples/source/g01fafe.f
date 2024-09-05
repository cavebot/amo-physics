*     G01FAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DEV, P
      INTEGER          I, IFAIL
      CHARACTER        TAIL
*     .. External Functions ..
      DOUBLE PRECISION G01FAF
      EXTERNAL         G01FAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01FAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' Tail    Probability     Deviate '
      WRITE (NOUT,*)
      DO 20 I = 1, 4
         READ (NIN,*) TAIL, P
         IFAIL = 0
*
         DEV = G01FAF(TAIL,P,IFAIL)
*
         WRITE (NOUT,99999) TAIL, P, DEV
   20 CONTINUE
      STOP
*
99999 FORMAT (3X,A1,9X,F5.3,9X,F6.4)
      END
