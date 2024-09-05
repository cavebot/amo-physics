*     F11XEF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          LA, NMAX
      PARAMETER        (LA=10000,NMAX=1000)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N, NNZ
      CHARACTER        CHECK
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA), X(NMAX), Y(NMAX)
      INTEGER          ICOL(LA), IROW(LA)
*     .. External Subroutines ..
      EXTERNAL         F11XEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F11XEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*
*     Read order of matrix and number of non-zero entries
*
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) NNZ
*
*     Read the matrix A
*
         DO 20 I = 1, NNZ
            READ (NIN,*) A(I), IROW(I), ICOL(I)
   20    CONTINUE
*
*     Read the vector x
*
         READ (NIN,*) (X(I),I=1,N)
*
*     Calculate matrix-vector product
*
         CHECK = 'C'
         IFAIL = 0
         CALL F11XEF(N,NNZ,A,IROW,ICOL,CHECK,X,Y,IFAIL)
*
*     Output results
*
         WRITE (NOUT,*) ' Matrix-vector product'
         DO 40 I = 1, N
            WRITE (NOUT,99999) Y(I)
   40    CONTINUE
      END IF
      STOP
99999 FORMAT (1X,D16.4)
      END
