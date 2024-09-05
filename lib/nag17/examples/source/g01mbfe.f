*     G01MBF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RM, X, XMU, XSIG, Z
      INTEGER          I
*     .. External Functions ..
      DOUBLE PRECISION G01MBF
      EXTERNAL         G01MBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01MBF Example Program Results '
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,99999)
      DO 20 I = 1, 3
         READ (NIN,*) X, XMU, XSIG
         Z = (X-XMU)/XSIG
         RM = G01MBF(Z)/XSIG
         WRITE (NOUT,99998) XMU, XSIG, X, RM
   20 CONTINUE
      STOP
*
99999 FORMAT (2X,'Mean',5X,'Sigma',4X,'X',8X,'Reciprocal',/'          ',
     +       '                   Mills Ratio',/)
99998 FORMAT (1X,4(F7.4,2X))
      END
