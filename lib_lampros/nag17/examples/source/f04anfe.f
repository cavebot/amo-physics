*     F04ANF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX, IQR
      PARAMETER        (MMAX=8,NMAX=MMAX,IQR=MMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION ALPHA(NMAX), B(MMAX), QR(IQR,NMAX), X(NMAX),
     +                 Z(NMAX)
      INTEGER          IPIV(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01AXF, F04ANF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04ANF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF (M.GT.0 .AND. M.LE.MMAX .AND. N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((QR(I,J),J=1,N),B(I),I=1,M)
         IFAIL = 1
*
*        Householder reduction to upper triangular form
         CALL F01AXF(M,N,QR,IQR,ALPHA,IPIV,X,Z,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F01AXF. IFAIL =', IFAIL
         ELSE
*
*           Approximate least squares solution
            CALL F04ANF(M,N,QR,IQR,ALPHA,IPIV,B,X,Z)
*
            WRITE (NOUT,*) ' Solution'
            WRITE (NOUT,99998) (X(I),I=1,N)
         END IF
      ELSE
         WRITE (NOUT,99999) 'M or N out of range: M = ', M, '  N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,F9.4)
      END
