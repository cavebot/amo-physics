*     D05BWF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          IORDER, NOMG, NWT, LDSW
      PARAMETER        (IORDER=4,NOMG=10,NWT=IORDER,LDSW=NOMG+IORDER-1)
*     .. Local Scalars ..
      INTEGER          IFAIL, J, LENSW, N
*     .. Local Arrays ..
      DOUBLE PRECISION OMEGA(NOMG), SW(LDSW,NWT)
*     .. External Subroutines ..
      EXTERNAL         D05BWF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D05BWF Example Program Results'
      IFAIL = 0
*
      CALL D05BWF('BDF',IORDER,OMEGA,NOMG,LENSW,SW,LDSW,NWT,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The convolution weights'
      WRITE (NOUT,*)
*
      DO 20 N = 1, NOMG
         WRITE (NOUT,99999) N - 1, OMEGA(N)
   20 CONTINUE
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The weights W'
      WRITE (NOUT,*)
*
      DO 40 N = 1, LENSW
         WRITE (NOUT,99999) N, (SW(N,J),J=1,NWT)
   40 CONTINUE
*
      STOP
*
99999 FORMAT (1X,I3,4X,6F10.4)
      END
