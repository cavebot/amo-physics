*     D05BYF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          IORDER, IQ, ITPMT, ITIQ, LENFW, LDSW, LWK
      PARAMETER        (IORDER=4,IQ=3,ITPMT=2*IORDER-1,ITIQ=2**(IQ+1),
     +                 LENFW=2*ITIQ,LDSW=ITIQ+ITPMT,LWK=4*ITIQ)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION SW(LDSW,ITPMT), WORK(LWK), WT(LENFW)
*     .. External Subroutines ..
      EXTERNAL         D05BYF
*     .. Executable Statements ..
*
      WRITE (NOUT,*) 'D05BYF Example Program Results'
      WRITE (NOUT,*)
      IFAIL = 0
*
      CALL D05BYF(IORDER,IQ,LENFW,WT,SW,LDSW,WORK,LWK,IFAIL)
*
      WRITE (NOUT,*) 'Fractional convolution weights'
      WRITE (NOUT,*)
      DO 20 I = 1, ITIQ
         WRITE (NOUT,99999) I - 1, WT(I)
   20 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Fractional starting weights'
      WRITE (NOUT,*)
      DO 40 I = 1, LDSW
         WRITE (NOUT,99999) I - 1, (SW(I,J),J=1,ITPMT)
   40 CONTINUE
*
      STOP
*
99999 FORMAT (1X,I5,7F9.4)
      END
