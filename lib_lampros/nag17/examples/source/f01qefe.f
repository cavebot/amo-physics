*     F01QEF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA, LDQ
      PARAMETER        (MMAX=20,NMAX=10,LDA=MMAX,LDQ=MMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N, NCOLQ
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), Q(LDQ,MMAX), WORK(MMAX), ZETA(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01QCF, F01QEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01QEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      IF ((M.GT.MMAX) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,*) 'M or N is out of range.'
         WRITE (NOUT,99999) 'M = ', M, '   N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         IFAIL = 0
*
*        Find the QR factorization of A
         CALL F01QCF(M,N,A,LDA,ZETA,IFAIL)
*
*        Copy the array A into Q and form the m by m matrix Q
         DO 40 J = 1, N
            DO 20 I = 1, M
               Q(I,J) = A(I,J)
   20       CONTINUE
   40    CONTINUE
         NCOLQ = M
         IFAIL = 0
*
         CALL F01QEF('Separate',M,N,NCOLQ,Q,LDQ,ZETA,WORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Matrix  Q'
         DO 60 I = 1, M
            WRITE (NOUT,99998) (Q(I,J),J=1,NCOLQ)
   60    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (5(1X,F8.4))
      END
