*     G01BJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION P, PEQK, PGTK, PLEK
      INTEGER          IFAIL, K, N
*     .. External Subroutines ..
      EXTERNAL         G01BJF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01BJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   N     P      K     PLEK      PGTK      PEQK'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) N, P, K
      IFAIL = 0
*
      CALL G01BJF(N,P,K,PLEK,PGTK,PEQK,IFAIL)
*
      WRITE (NOUT,99999) N, P, K, PLEK, PGTK, PEQK
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,I4,F8.3,I5,3F10.5)
      END
