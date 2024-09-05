*     F01RGF Example Program Text
*     Mark 14 release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA
      PARAMETER        (MMAX=10,NMAX=20,LDA=MMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), THETA(MMAX)
*     .. External Subroutines ..
      EXTERNAL         F01RGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01RGF Example Program Results'
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
         CALL F01RGF(M,N,A,LDA,THETA,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'RQ factorization of A'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Vector THETA'
         WRITE (NOUT,99998) (THETA(I),I=1,M)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     + 'Matrix A after factorization (R is in left-hand upper triangle)'
         DO 20 I = 1, M
            WRITE (NOUT,99998) (A(I,J),J=1,N)
   20    CONTINUE
      END IF
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,4(' (',F7.4,',',F8.4,')',:))
      END
