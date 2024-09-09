*     F11JBF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LA, LIWORK
      PARAMETER        (NMAX=1000,LA=10000,LIWORK=2*LA+7*NMAX+1)
*     .. Local Scalars ..
      DOUBLE PRECISION DSCALE, DTOL
      INTEGER          I, IFAIL, LFILL, N, NNZ, NNZC, NPIVM
      CHARACTER        CHECK, MIC, PSTRAT
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA), X(NMAX), Y(NMAX)
      INTEGER          ICOL(LA), IPIV(NMAX), IROW(LA), ISTR(NMAX+1),
     +                 IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         F11JAF, F11JBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F11JBF Example Program Results'
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
*     Read the vector y
*
         READ (NIN,*) (Y(I),I=1,N)
*
*     Calculate Cholesky factorization
*
         LFILL = -1
         DTOL = 0.0D0
         MIC = 'N'
         DSCALE = 0.0D0
         PSTRAT = 'M'
         IFAIL = 0
*
         CALL F11JAF(N,NNZ,A,LA,IROW,ICOL,LFILL,DTOL,MIC,DSCALE,PSTRAT,
     +               IPIV,ISTR,NNZC,NPIVM,IWORK,LIWORK,IFAIL)
*
*     Check the output value of NPIVM
*
         IF (NPIVM.NE.0) THEN
*
            WRITE (NOUT,*) 'Factorization is not complete'
*
         ELSE
*                  T T
*     Solve P L D L P x = y
*
            CHECK = 'C'
*
            CALL F11JBF(N,A,LA,IROW,ICOL,IPIV,ISTR,CHECK,Y,X,IFAIL)
*
*     Output results
*
            WRITE (NOUT,*) ' Solution of linear system'
            DO 40 I = 1, N
               WRITE (NOUT,99999) X(I)
   40       CONTINUE
*
         END IF
      END IF
      STOP
99999 FORMAT (1X,D16.4)
      END
