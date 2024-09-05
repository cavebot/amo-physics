*     F01QKF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA, LDPT
      PARAMETER        (MMAX=10,NMAX=20,LDA=MMAX,LDPT=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N, NROWP
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), PT(LDPT,NMAX), WORK(NMAX),
     +                 ZETA(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01QJF, F01QKF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01QKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF ((M.GT.MMAX) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,*) 'M or N is out of range.'
         WRITE (NOUT,99999) 'M = ', M, '   N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         IFAIL = 0
*
*        Find the RQ factorization of A
         CALL F01QJF(M,N,A,LDA,ZETA,IFAIL)
*
*        Copy the array A into PT and form the n by n matrix conjg(P')
         DO 40 J = 1, N
            DO 20 I = 1, M
               PT(I,J) = A(I,J)
   20       CONTINUE
   40    CONTINUE
         NROWP = N
         IFAIL = 0
*
         CALL F01QKF('Separate',M,N,NROWP,PT,LDPT,ZETA,WORK,IFAIL)
*
         WRITE (NOUT,*) 'Matrix  P'
         DO 60 I = 1, N
            WRITE (NOUT,99998) (PT(J,I),J=1,NROWP)
   60    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (5(1X,F8.4))
      END
