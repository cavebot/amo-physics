*     G01EMF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION Q, V, VALP
      INTEGER          I, IFAIL, IR
*     .. External Functions ..
      DOUBLE PRECISION G01EMF
      EXTERNAL         G01EMF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01EMF Example Program Results '
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  Q       V    IR    Quantile '
      WRITE (NOUT,*)
      DO 20 I = 1, 3
         READ (NIN,*) Q, V, IR
         IFAIL = -1
*
         VALP = G01EMF(Q,V,IR,IFAIL)
*
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,99999) Q, V, IR, VALP
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,F7.4,2X,F4.1,1X,I3,1X,F10.4)
      END
