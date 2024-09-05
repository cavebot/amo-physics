*     F01QDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, NCBMAX, LDA, LDB
      PARAMETER        (MMAX=20,NMAX=10,NCBMAX=5,LDA=MMAX,LDB=MMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N, NCOLB
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,NCBMAX), WORK(NCBMAX),
     +                 ZETA(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01QCF, F01QDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01QDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF ((M.GT.MMAX) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,*) 'M or N is out of range.'
         WRITE (NOUT,99999) 'M = ', M, '   N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         READ (NIN,*) NCOLB
         IF (NCOLB.GT.NCBMAX) THEN
            WRITE (NOUT,*) 'NCOLB is out of range.'
            WRITE (NOUT,99999) 'NCOLB = ', NCOLB
         ELSE
            READ (NIN,*) ((B(I,J),J=1,NCOLB),I=1,M)
            IFAIL = 0
*
*           Find the QR factorization of A
            CALL F01QCF(M,N,A,LDA,ZETA,IFAIL)
*
            IFAIL = 0
*
*           Form  Q'*B
            CALL F01QDF('Transpose','Separate',M,N,A,LDA,ZETA,NCOLB,B,
     +                  LDB,WORK,IFAIL)
*
            WRITE (NOUT,*) 'Matrix  Q''*B'
            DO 20 I = 1, M
               WRITE (NOUT,99998) (B(I,J),J=1,NCOLB)
   20       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (2(1X,F8.4))
      END
