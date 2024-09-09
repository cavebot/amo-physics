*     G01FMF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION P, V, VALQ
      INTEGER          I, IFAIL, IR
*     .. External Functions ..
      DOUBLE PRECISION G01FMF
      EXTERNAL         G01FMF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01FMF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  P     V    IR    Quantile '
      WRITE (NOUT,*)
      DO 20 I = 1, 3
         READ (NIN,*) P, V, IR
         IFAIL = -1
*
         VALQ = G01FMF(P,V,IR,IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.EQ.3) THEN
            WRITE (NOUT,99999) P, V, IR, VALQ
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,F5.2,2X,F4.1,1X,I3,1X,F10.4)
      END
