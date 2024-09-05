*     F01AXF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX, IQR
      PARAMETER        (MMAX=8,NMAX=MMAX,IQR=MMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION ALPHA(NMAX), E(NMAX), QR(IQR,NMAX), Y(NMAX)
      INTEGER          IPIV(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01AXF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01AXF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF (M.GT.0 .AND. M.LE.MMAX .AND. N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((QR(I,J),J=1,N),I=1,M)
         IFAIL = 1
*
         CALL F01AXF(M,N,QR,IQR,ALPHA,IPIV,Y,E,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99997) 'Error in F01AXF. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*) 'ALPHA'
            WRITE (NOUT,99999) (ALPHA(I),I=1,N)
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Array QR'
            DO 20 I = 1, M
               WRITE (NOUT,99999) (QR(I,J),J=1,N)
   20       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'IPIV'
            WRITE (NOUT,99996) (IPIV(I),I=1,N)
         END IF
      ELSE
         WRITE (NOUT,99998) 'M or N out of range: M = ', M, '   N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (1X,A,I5,A,I5)
99997 FORMAT (1X,A,I2)
99996 FORMAT (1X,8I5)
      END
