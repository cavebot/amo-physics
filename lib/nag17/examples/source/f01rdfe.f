*     F01RDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, NCBMAX
      PARAMETER        (MMAX=20,NMAX=10,NCBMAX=5)
      INTEGER          LDA, LDB
      PARAMETER        (LDA=MMAX,LDB=MMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N, NCOLB
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), B(LDB,NCBMAX), THETA(NMAX),
     +                 WORK(NCBMAX)
*     .. External Subroutines ..
      EXTERNAL         F01RCF, F01RDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01RDF Example Program Results'
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
            WRITE (NOUT,99998) 'NCOLB = ', NCOLB
         ELSE
            READ (NIN,*) ((B(I,J),J=1,NCOLB),I=1,M)
*           Find the QR factorization of A.
            IFAIL = 0
*
            CALL F01RCF(M,N,A,LDA,THETA,IFAIL)
*           Form  conjg( Q' )*B.
            IFAIL = 0
*
            CALL F01RDF('Conjugate transpose','Separate',M,N,A,LDA,
     +                  THETA,NCOLB,B,LDB,WORK,IFAIL)
*
            WRITE (NOUT,*) 'Matrix  conjg( Q'' )*B'
            DO 20 I = 1, M
               WRITE (NOUT,99997) (B(I,J),J=1,NCOLB)
   20       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,A,I5)
99997 FORMAT (1X,2(' (',F7.4,',',F8.4,')',:))
      END
