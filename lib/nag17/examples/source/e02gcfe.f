*     E02GCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, MMAX, NDIM, MDIM
      PARAMETER        (N=3,MMAX=5,NDIM=N+3,MDIM=MMAX+1)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RELERR, RESMAX, T, TOL
      INTEGER          I, IFAIL, IRANK, ITER, M
*     .. Local Arrays ..
      DOUBLE PRECISION A(NDIM,MDIM), B(MMAX), X(N)
*     .. External Subroutines ..
      EXTERNAL         E02GCF
*     .. Intrinsic Functions ..
      INTRINSIC        EXP
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02GCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M
      IF (M.GT.0 .AND. M.LE.MMAX) THEN
         DO 20 I = 1, M
            READ (NIN,*) T, B(I)
            A(1,I) = EXP(T)
            A(2,I) = EXP(-T)
            A(3,I) = 1.0D0
   20    CONTINUE
         TOL = 0.0D0
         RELERR = 0.0D0
         IFAIL = 1
*
         CALL E02GCF(M,N,MDIM,NDIM,A,B,TOL,RELERR,X,RESMAX,IRANK,ITER,
     +               IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.LE.1) THEN
            WRITE (NOUT,99999) 'RESMAX = ', RESMAX, '  Rank = ', IRANK,
     +        '  Iterations = ', ITER, '  IFAIL =', IFAIL
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Solution'
            WRITE (NOUT,99998) (X(I),I=1,N)
         ELSE
            WRITE (NOUT,99997) 'E02GCF fails with error', IFAIL
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,D10.2,A,I5,A,I5,A,I5)
99998 FORMAT (1X,6F10.4)
99997 FORMAT (1X,A,I2)
      END
