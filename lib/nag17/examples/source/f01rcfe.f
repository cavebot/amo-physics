*     F01RCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=20,NMAX=10)
      INTEGER          LDA
      PARAMETER        (LDA=MMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), THETA(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01RCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01RCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF ((M.GT.MMAX) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,*) 'M or N is out of range.'
         WRITE (NOUT,99999) 'M = ', M, '   N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
*        Find the QR factorization of A.
         IFAIL = 0
*
         CALL F01RCF(M,N,A,LDA,THETA,IFAIL)
*
         WRITE (NOUT,*) 'QR factorization of A'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Vector THETA'
         WRITE (NOUT,99998) (THETA(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'Matrix A after factorization (upper triangular part is R)'
         DO 20 I = 1, M
            WRITE (NOUT,99998) (A(I,J),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,3(' (',F7.4,',',F8.4,')',:))
      END
