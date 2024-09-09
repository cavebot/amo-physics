*     F01QJF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA
      PARAMETER        (MMAX=10,NMAX=20,LDA=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), ZETA(MMAX)
*     .. External Subroutines ..
      EXTERNAL         F01QJF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01QJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF ((M.GT.MMAX) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'M or N is out of range.'
         WRITE (NOUT,99999) 'M = ', M, '   N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         IFAIL = 0
*
*        Find the RQ factorization of A
         CALL F01QJF(M,N,A,LDA,ZETA,IFAIL)
*
         WRITE (NOUT,*) 'RQ factorization of A'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Vector ZETA'
         WRITE (NOUT,99998) (ZETA(I),I=1,M)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     + 'Matrix A after factorization (R is in left-hand upper triangle)'
         DO 20 I = 1, M
            WRITE (NOUT,99998) (A(I,J),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (5(1X,F8.4))
      END
