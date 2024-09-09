*     F11JAF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LA, LIWORK
      PARAMETER        (NMAX=1000,LA=10000,LIWORK=2*LA+7*NMAX+1)
*     .. Local Scalars ..
      DOUBLE PRECISION DSCALE, DTOL
      INTEGER          I, IFAIL, LFILL, N, NNZ, NNZC, NPIVM
      CHARACTER        MIC, PSTRAT
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA)
      INTEGER          ICOL(LA), IPIV(NMAX), IROW(LA), ISTR(NMAX+1),
     +                 IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         F11JAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F11JAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*
*     Read algorithmic parameters
*
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) NNZ
         READ (NIN,*) LFILL, DTOL
         READ (NIN,*) MIC, DSCALE
         READ (NIN,*) PSTRAT
*
*     Read the matrix A
*
         DO 20 I = 1, NNZ
            READ (NIN,*) A(I), IROW(I), ICOL(I)
   20    CONTINUE
*
*     Calculate incomplete Cholesky factorization
*
         IFAIL = 0
         CALL F11JAF(N,NNZ,A,LA,IROW,ICOL,LFILL,DTOL,MIC,DSCALE,PSTRAT,
     +               IPIV,ISTR,NNZC,NPIVM,IWORK,LIWORK,IFAIL)
*
*     Output original matrix
*
         WRITE (NOUT,*) ' Original Matrix'
         WRITE (NOUT,*) ' N     =', N
         WRITE (NOUT,*) ' NNZ   =', NNZ
         DO 40 I = 1, NNZ
            WRITE (NOUT,99999) I, A(I), IROW(I), ICOL(I)
   40    CONTINUE
         WRITE (NOUT,*)
*
*     Output details of the factorization
*
         WRITE (NOUT,*) ' Factorization'
         WRITE (NOUT,*) ' N     =', N
         WRITE (NOUT,*) ' NNZ   =', NNZC
         WRITE (NOUT,*) ' NPIVM =', NPIVM
         DO 60 I = NNZ + 1, NNZ + NNZC
            WRITE (NOUT,99999) I, A(I), IROW(I), ICOL(I)
   60    CONTINUE
         WRITE (NOUT,*)
*
         WRITE (NOUT,*) '      I     IPIV(I)'
         DO 80 I = 1, N
            WRITE (NOUT,99998) I, IPIV(I)
   80    CONTINUE
*
      END IF
      STOP
99999 FORMAT (1X,I8,D16.4,2I8)
99998 FORMAT (1X,2I8)
      END
