*     F11ZBF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          LA, NMAX
      PARAMETER        (LA=10000,NMAX=1000)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N, NNZ
      CHARACTER        DUP, ZERO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA)
      INTEGER          ICOL(LA), IROW(LA), ISTR(NMAX+1), IWORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F11ZBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F11ZBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*
*     Read order of matrix and number of non-zero entries
*
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) NNZ
*
*     Read and output the original non-zero elements
*
         DO 20 I = 1, NNZ
            READ (NIN,*) A(I), IROW(I), ICOL(I)
   20    CONTINUE
         WRITE (NOUT,*) 'Original elements'
         WRITE (NOUT,*) 'NNZ = ', NNZ
         DO 40 I = 1, NNZ
            WRITE (NOUT,99998) I, A(I), IROW(I), ICOL(I)
   40    CONTINUE
*
*     Reorder, sum duplicates and remove zeros
*
         DUP = 'S'
         ZERO = 'R'
         IFAIL = 0
*
         CALL F11ZBF(N,NNZ,A,IROW,ICOL,DUP,ZERO,ISTR,IWORK,IFAIL)
*
*     Output results
*
         WRITE (NOUT,*) 'Reordered elements'
         WRITE (NOUT,99999) 'NNZ = ', NNZ
         DO 60 I = 1, NNZ
            WRITE (NOUT,99998) I, A(I), IROW(I), ICOL(I)
   60    CONTINUE
      END IF
      STOP
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,I8,D16.4,2I8)
      END
